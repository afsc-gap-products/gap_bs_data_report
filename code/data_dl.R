
# Log into Oracle --------------------------------------------------------------

if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  # This has a specific username and password because I DONT want people to have access to this!
  source("Z:/Projects/ConnectToOracle.R")
} else {
  # For those without a ConnectToOracle file
  library(rstudioapi)
  library(RODBC)
  channel <- odbcConnect(dsn = "AFSC", 
                         uid = rstudioapi::showPrompt(title = "Username", 
                                                      message = "Oracle Username", default = ""), 
                         pwd = rstudioapi::askForPassword("Enter Password"),
                         believeNRows = FALSE)
}

# Pull data from GAP_PRODUCTS --------------------------------------------------

locations <- c(
  "GAP_PRODUCTS.OLD_STATION",
  "GAP_PRODUCTS.AKFIN_AGECOMP",
  "GAP_PRODUCTS.AKFIN_AREA",
  "GAP_PRODUCTS.AKFIN_BIOMASS",
  "GAP_PRODUCTS.AKFIN_CATCH",
  "GAP_PRODUCTS.AKFIN_CRUISE",
  "GAP_PRODUCTS.AKFIN_HAUL",
  "GAP_PRODUCTS.AKFIN_CPUE",
  "GAP_PRODUCTS.AKFIN_METADATA_COLUMN",
  "GAP_PRODUCTS.AKFIN_SIZECOMP",
  "GAP_PRODUCTS.AKFIN_SPECIMEN",
  "GAP_PRODUCTS.AKFIN_STRATUM_GROUPS",
  "GAP_PRODUCTS.AKFIN_SURVEY_DESIGN",
  "GAP_PRODUCTS.AKFIN_TAXONOMIC_CLASSIFICATION",
  "GAP_PRODUCTS.TAXONOMIC_CHANGES",
  "GAP_PRODUCTS.SPECIES_YEAR"
)

print(Sys.Date())

error_loading <- c()
for (i in 1:length(locations)){
  print(locations[i])
  
  a <- RODBC::sqlQuery(channel = channel, 
                       query = paste0("SELECT *
    FROM ", locations[i], "
    FETCH FIRST 1 ROWS ONLY;"))
  
  end0 <- c()
  if ("SURVEY_DEFINITION_ID" %in% names(a)) {
    end0 <- c(end0, "SURVEY_DEFINITION_ID IN (143, 98)")
  }
  if ("YEAR" %in% names(a) & !(locations[i] %in% 
                               c("GAP_PRODUCTS.AKFIN_SIZECOMP", "GAP_PRODUCTS.AKFIN_AGECOMP", 
                                 "GAP_PRODUCTS.AKFIN_BIOMASS", 
                                 "GAP_PRODUCTS.AKFIN_CRUISE", "GAP_PRODUCTS.AKFIN_CPUE",
                                 "GAP_PRODUCTS.AKFIN_HAUL")) ) {
    end0 <- c(end0, paste0("YEAR IN (",paste0(maxyr:compareyr, collapse = ","), ")"))
  }

  # if (locations[i] %in% c("GAP_PRODUCTS.AKFIN_CRUISE")) {
  #   cruises0 <- a
  # }
  # 
  # if (locations[i] %in% c("GAP_PRODUCTS.AKFIN_HAUL")) {
  #   end0 <- c(end0, paste0("YEAR IN (",text_list(x = maxyr:(maxyr-9), sep = ", ", sep_last2 = ", ", sep_last = ", "), ")"))
  # }
  # 
  # if ("YEAR" %in% names(a) & locations[i] %in% c("GAP_PRODUCTS.AKFIN_CPUE")) {
  #   end0 <- c(end0, paste0("YEAR IN (",text_list(x = maxyr:(maxyr-9), sep = ", ", sep_last2 = ", ", sep_last = ", "), ")"))
  # }
  
  end0 <- ifelse(is.null(end0), "", paste0(" WHERE ", paste0(end0, collapse = " AND ")))
  
  start0 <- ifelse(!("START_TIME" %in% names(a)), 
                   "*", 
                   paste0(paste0(names(a)[names(a) != "START_TIME"], sep = ",", collapse = " "),
                          " TO_CHAR(START_TIME,'MM/DD/YYYY HH24:MI:SS') START_TIME "))
  
  a <- RODBC::sqlQuery(channel, paste0("SELECT ", start0, " FROM ", locations[i], end0, "; "))
  
  if (is.null(nrow(a))) { # if (sum(grepl(pattern = "SQLExecDirect ", x = a))>1) {
    error_loading <- c(error_loading, locations[i])
  } else {
    write.csv(x = a, 
              here::here("data",
                         paste0(tolower(gsub(pattern = '.', 
                                             replacement = "_", 
                                             x = locations[i], 
                                             fixed = TRUE)),
                                ".csv")))
  }
  remove(a)
}
error_loading

# Pull data from RACE_DATA and CRAB --------------------------------------------

locations <- c(
  "foodlab.predprey",
  "foodlab.haul",
  "foodlab.nodc", 
  # "crab.gap_ebs_nbs_abundance_biomass", # Biomass
  # "crab.gap_ebs_nbs_crab_cpue", # CPUE
  # "crab.ebscrab", # length data
  # "crab.ebscrab_nbs", # length data
  # "crab.nbs_size1mm_all_species", 
  "RACE_DATA.LENGTHS",   # "RACE_DATA.V_EXTRACT_FINAL_LENGTHS" also works, but is ephemeral # "GAP_PRODUCTS.AKFIN_LENGTH", # has extrapolated lengths, not how many were actually counted
  "RACE_DATA.EDIT_HAULS", 
  "race_data.edit_events",
  "RACE_DATA.HAULS", # need for manipulating RACE_DATA.LENGTHS
  "RACE_DATA.CRUISES", 
  "RACE_DATA.SURVEYS", 
  "RACE_DATA.SURVEY_DEFINITIONS", 
  "RACE_DATA.VESSELS", 
  "RACE_DATA.LENGTH_TYPES")

for (i in 1:length(locations)) {
  print(locations[i])
  a <- RODBC::sqlQuery(channel = channel, # NOT RACEBASE.HAUL
                       query = paste0("SELECT *
FROM ",locations[i],"; "))
  write.csv(x = a, 
            here::here("data",
                       paste0(tolower(gsub(pattern = '.', 
                                           replacement = "_", 
                                           x = locations[i], 
                                           fixed = TRUE)),
                              ".csv")))
}

# Create species complex tables ------------------------------------------------
source("Z:/Projects/ConnectToOracle.R")

# Species Covered
# https://docs.google.com/spreadsheets/d/10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M/edit?usp=sharing
googledrive::drive_download(file = googledrive::as_id("10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M"),
                            type = "csv",
                            overwrite = TRUE,
                            path = paste0(dir_out_rawdata, "/species-local-names"))

# identify which species complexes you need
report_spp <- readr::read_csv(file = paste0(dir_out_rawdata, "/species-local-names.csv"), 
                              skip = 1, 
                              show_col_types = FALSE)|>  
  dplyr::filter(grepl(x = species_code, pattern = "c(", fixed = TRUE)) 

report_spp$GROUP_CODE <- 1:nrow(report_spp)+1e6
report_spp$group_sci <- ifelse(report_spp$group_sci == "BLANK", "", report_spp$group_sci)

temp1 <- data.frame()
for (i in 1:nrow(report_spp)){
  # temp2 <- eval(expr = parse(text = report_spp$species_code[i]))
  temp1 <- dplyr::bind_rows(
    temp1, 
    dplyr::bind_cols(GROUP_CODE = report_spp$GROUP_CODE[i], 
                     GROUP_NAME = report_spp$print_name[i], 
                     TAXON = report_spp$taxon[i], 
                     SPECIES_NAME = report_spp$group_sci[i], 
                     SPECIES_CODE = eval(expr = parse(text = report_spp$species_code[i]))))
}

if (FALSE) {
temp1 <- temp1 |>
  dplyr::bind_rows(
    data.frame(
      GROUP_CODE = c(69322, 69323, 68560, 68580, 68590, 69400),
      SPECIES_CODE = c(69322, 69323, 68560, 68580, 68590, 69400),
      TAXON = "invert",
      SPECIES_NAME = c("Paralithodes camtschaticus", "Paralithodes platypus", "Chionoecetes bairdi", "Chionoecetes opilio", "Chionoecetes hybrid", "Erimacrus isenbeckii"),
      GROUP_NAME = c("red king crab", "blue king crab", "Tanner crab", "snow crab", "hybrid Tanner crab", "horsehair crab") ))
}

# filter temp1 for exisiting species codes in GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION
# note. maybe gp_taxa is already called somewhere but directly pulling here.
# following advice from Z Oyafuso in https://github.com/afsc-gap-products/gapindex/issues/63#issuecomment-2499660803
gp_taxa <- 
  RODBC::sqlQuery(channel = channel, 
                  query = "SELECT * FROM GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION 
                           WHERE SURVEY_SPECIES = 1 ORDER BY SPECIES_CODE")
temp1 <- subset(x = temp1,
                subset = SPECIES_CODE %in% gp_taxa$SPECIES_CODE)

## Pull all data -------------------------------------------------------------------

library(gapindex)
# channel <- gapindex::get_connected(check_access = F)

# follow instructions from https://afsc-gap-products.github.io/gapindex/articles/ex_species_complex.html
## Pull data. Note the format of the spp_codes argument with the GROUP column
complex_data <- complex_data0 <- gapindex::get_data(
  year_set = 1982:maxyr,
  survey_set = c("EBS", "NBS"),
  spp_codes = temp1[,c("GROUP_CODE", "SPECIES_CODE")],
  pull_lengths = FALSE, 
  haul_type = 3, 
  abundance_haul = "Y", 
  taxonomic_source = "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION", # "RACEBASE.SPECIES", 
  channel = channel)

if (FALSE) {
## Pull crab data --------------------------------------------------------------

# List of species and survey regions to add to GAP data

# https://github.com/AFSC-Shellfish-Assessment-Program/crabpack
# devtools::install_github("AFSC-Shellfish-Assessment-Program/crabpack")
library(crabpack)

# crabpack data pull does a funny thing where the channel *needs* to be called 'channel' even though it says it can accept other names
source("Z:/Projects/ConnectToOracle.R")
channel <- channel_akfin

spp_list <- tidyr::crossing(
  spp = c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR"), 
  reg = c("EBS", "NBS"))

# Pull crab data from `crabpack`
crabpack_specimen <- c()
for (i in 1:nrow(spp_list)) {
  source("Z:/Projects/ConnectToOracle.R")
  channel <- channel_akfin
  
  spp <- spp_list$spp[i]
  reg <- spp_list$reg[i]
  print(paste0(reg, " ", spp))
  
  specimen_data <- crabpack::get_specimen_data(species = spp,
                                               region = reg,
                                               years = c(1982:2024)) 
  
  crabpack_specimen <- crabpack_specimen |> 
    dplyr::bind_rows(specimen_data$specimen |> 
                       dplyr::mutate(spp = spp, 
                                     reg = reg))
}

crabpack_specimen0 <- crabpack_specimen

write.csv(x = crabpack_specimen0, 
          file = here::here("data/crabpack_specimen.csv"))

# NOTES
# sex = dplyr::case_when(
#   sex == 1 ~ "males",
#   sex == 0 ~ "unsexed",
#   (clutch_size == 0 & sex == 2) ~ "immature females", 
#   (clutch_size >= 1 & sex == 2) ~ "mature females"), 

# find which hauls need to be replaced with retow data
crabpack_specimen <- crabpack_specimen0
crabpack_specimen <- dplyr::bind_rows(
  # data from retow stations - female RKC
  crabpack_specimen |> 
    dplyr::filter(HAUL_TYPE == 17) |> 
    dplyr::filter(SEX == 2 & SPECIES_CODE == 69322), 
  # data from not retow stations - male and unsexed RKC, and everything else
  crabpack_specimen |> 
    dplyr::filter(HAUL_TYPE == 3) |> 
    dplyr::filter(!(SEX == 2 & SPECIES_CODE == 69322))  )  |> 
  dplyr::mutate(SEX = ifelse(SEX == 2 & CLUTCH_SIZE == 0, 5, SEX), # "immature females" 
                SEX = ifelse(SEX == 2 & CLUTCH_SIZE != 0, 6, SEX)) |> # "mature females" 
  dplyr::filter(SEX != 4) |> # unisex
  dplyr::select(HAULJOIN, YEAR, STATION = STATION_ID, HAUL_TYPE, STRATUM, 
                SPECIES_CODE, SEX, 
                SIZE, SIZE_1MM, WEIGHT, CALCULATED_WEIGHT_1MM, SAMPLING_FACTOR) |> 
  dplyr::distinct()

complex_data$catch <- crabpack_specimen |> 
  dplyr::mutate(WEIGHT = (CALCULATED_WEIGHT_1MM * SAMPLING_FACTOR)/1000) |>  # convert from grams to kg
  dplyr::group_by(HAULJOIN, SPECIES_CODE) |> 
  dplyr::summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE), 
                   NUMBER_FISH = sum(SAMPLING_FACTOR, na.rm = TRUE)) |> 
  dplyr::ungroup() |> 
  data.table::data.table(key = c("HAULJOIN", "SPECIES_CODE")) |> 
  dplyr::bind_rows(complex_data$catch)

complex_data$specimen <- crabpack_specimen |> 
  dplyr::select(WEIGHT, SEX, SPECIES_CODE, HAULJOIN, LENGTH = SIZE_1MM)  |>  
  dplyr::mutate(WEIGHT = WEIGHT / 1000) |>   # convert from grams to kg
  dplyr::group_by(HAULJOIN, SPECIES_CODE, SEX, LENGTH) |> 
  dplyr::summarise(WEIGHT = sum(WEIGHT, na.rm = TRUE)) |> 
  dplyr::ungroup() |>
  dplyr::mutate(AGE = NA)  |> 
  dplyr::left_join(complex_data$haul |> 
                     dplyr::select(CRUISEJOIN, HAULJOIN) |> 
                     dplyr::distinct() ) |> 
  data.table::data.table(key = c("HAULJOIN", "SPECIES_CODE", "SEX", "AGE", "LENGTH")) |> # "CRUISEJOIN", 
  dplyr::bind_rows(complex_data$specimen)

complex_data$size <- crabpack_specimen |> 
  dplyr::select(SEX, SPECIES_CODE, HAULJOIN, LENGTH = SIZE_1MM, FREQUENCY = SAMPLING_FACTOR)  |>  
  dplyr::group_by(HAULJOIN, SPECIES_CODE, SEX, LENGTH) |> 
  dplyr::summarise(#LENGTH = sum(LENGTH, na.rm = TRUE), 
                   FREQUENCY = sum(FREQUENCY, na.rm = TRUE)) |> 
  dplyr::ungroup()|> 
  dplyr::left_join(complex_data$haul |> 
                     dplyr::select(CRUISEJOIN, HAULJOIN) |> 
                     dplyr::distinct() ) |> 
  data.table::data.table(key = c("HAULJOIN", "SPECIES_CODE", "SEX", "LENGTH")) |> 
  dplyr::bind_rows(complex_data$size)
}
## Calculate Zero-fill CPUE ----------------------------------------------------

complex_cpue <- gapindex::calc_cpue(gapdata = complex_data) 

complex_cpue <- complex_cpue |> 
  dplyr::left_join(temp1 |> 
                     dplyr::select(SPECIES_CODE = GROUP_CODE, 
                                   COMMON_NAME = GROUP_NAME, 
                                   TAXON, 
                                   SPECIES_NAME) |> 
                     dplyr::distinct()) 

write.csv(x = complex_cpue, 
          file = here::here("data/complex_cpue.csv"), 
          row.names = FALSE)

## Calculate Biomass, abundance, mean CPUE, and associated variances by stratum ----

complex_biomass_stratum <-
  gapindex::calc_biomass_stratum(gapdata = complex_data,
                                 cpue = complex_cpue)

complex_biomass_subarea <-
  gapindex::calc_biomass_subarea(gapdata = complex_data,
                                 biomass_stratum = complex_biomass_stratum)

complex_biomass <- complex_biomass_stratum |>
  dplyr::rename(AREA_ID = STRATUM) |>
  dplyr::bind_rows(complex_biomass_subarea) |>
dplyr::left_join(temp1 |> 
                   dplyr::select(SPECIES_CODE = GROUP_CODE, 
                                 COMMON_NAME = GROUP_NAME, 
                                 TAXON, 
                                 SPECIES_NAME) |> 
                   dplyr::distinct())

write.csv(x = complex_biomass, 
          file = here::here("data/complex_biomass.csv"), 
          row.names = FALSE)


## Calculate Size composition by stratum FOR JUST CRAB---------------------------------------
if (FALSE) {
crab_data <- complex_data

crab_data$catch <- crab_data$catch |> 
  dplyr::filter(SPECIES_CODE %in% c(69322, 69323, 68560, 68580, 68590, 69400))

crab_data$size <- crab_data$size |> 
  dplyr::filter(SPECIES_CODE %in% c(69322, 69323, 68560, 68580, 68590, 69400))

write.csv(x = crab_data$size, 
          file = here::here("data/sap_lengths.csv"), 
          row.names = FALSE)

crab_data$specimen <- crab_data$specimen |> 
  dplyr::filter(SPECIES_CODE %in% c(69322, 69323, 68560, 68580, 68590, 69400))

# Calculate size composition by stratum. See ?gapindex::calc_sizecomp_stratum
# for details on arguments
# Calculate aggregated size composition across subareas, management areas, and
# regions

# Note fill_NA_method == "BS" because
# our region is EBS, NBS, or BSS. If the survey region of interest is AI or
# GOA, use "AIGOA". See ?gapindex::gapindex::calc_sizecomp_stratum for more
# details.

# Aggregate size composition to stratum
crab_sizecomp_stratum <- gapindex::calc_sizecomp_stratum(
  gapdata = crab_data,
  cpue = complex_cpue  |> 
    dplyr::filter(SPECIES_CODE %in% c(69322, 69323, 68560, 68580, 68590, 69400)),
  abundance_stratum = complex_biomass_stratum |> 
    dplyr::filter(SPECIES_CODE %in% c(69322, 69323, 68560, 68580, 68590, 69400)),
  spatial_level = "stratum",
  fill_NA_method = "BS")

# Error in vecseq(f__, len__, limit) : 
#   Join results in 1518038 rows; more than 893685 = nrow(x)+nrow(i). Check for duplicate key values in i each of which join to the same group in x over and over again. If that's ok, try by=.EACHI to run j for each group to avoid the large allocation. If you are sure you wish to proceed, rerun with allow.cartesian=TRUE. Otherwise, please search for this error message in the FAQ, Wiki, Stack Overflow and data.table issue tracker for advice.

# Aggregate size composition to subareas/region
crab_sizecomp_subarea <- gapindex::calc_sizecomp_subarea(
  gapdata = crab_data,
  sizecomp_stratum = crab_sizecomp_stratum)

# rbind stratum and subarea/region biomass estimates into one dataframe
crab_sizecomp <- crab_sizecomp_stratum |>
  dplyr::rename(AREA_ID = STRATUM) |>
  dplyr::bind_rows(crab_sizecomp_subarea) |> 
  dplyr::left_join(temp1 |> 
                     dplyr::select(SPECIES_CODE = GROUP_CODE, 
                                   COMMON_NAME = GROUP_NAME, 
                                   TAXON, SPECIES_NAME) |> 
                     dplyr::distinct()) |> 
  dplyr::select(-SURVEY)

write.csv(x = crab_sizecomp, file = here::here("data/crab_sizecomp.csv"), row.names = FALSE)
}

# Crabpack data ----------------------------------------------------------------

# Load libraries
library(crabpack)
library(dplyr)
library(tidyr)

# Setup
channel <- "API"

# lookup for species codes
species_lookup <- tibble(SPECIES_CODE = c(68560, 68580, 68590, 69322, 69323, 69400),
                         SPECIES = c("TANNER", "SNOW", "HYBRID", "RKC", "BKC", "HAIR"))

# blank dfs
bioabund_out <- c() # biomass/abundance ("crab.gap_ebs_nbs_abundance_biomass")
cpue_out <- c() # station-level CPUE ("crab.gap_ebs_nbs_crab_cpue") **need to convert this from mt/nmi2 to kg/km2!!
NBS_pop1mm_out <- c() # NBS 1mm abundance ("crab.nbs_size1mm_all_species")
EBS_pop1mm_out <- c() # NEBS 1mm abundance ("crab.nbs_size1mm_all_species")


# Loop through species and combine estimates
for(i in 1:length(species_lookup$SPECIES)){
  # Set species
  species <- species_lookup$SPECIES[i]
  print(species)
  
  ## Specimen data -------------------------------------------------------------
  dat_EBS <- crabpack::get_specimen_data(species = species,
                                         region = "EBS",
                                         channel = channel)
  dat_NBS <- crabpack::get_specimen_data(species = species,
                                         region = "NBS",
                                         channel = channel)
  
  
  # Bind haul info to add hauljoins later
  haul <- bind_rows(dat_EBS$haul, dat_NBS$haul)
  
  ## Biomass/Abundance ---------------------------------------------------------
  EBS_bioabund <- crabpack::calc_bioabund(crab_data = dat_EBS,
                                          species = species,
                                          region = "EBS",
                                          years = c(1982:maxyr),
                                          spatial_level = "region")
  ## ^^ this isn't working correctly for BKC because of a bug (see Issue 10) --
  ##    doing some janky coding below to get the estimates to combine correctly for now, 
  ##    but this will be fixed in crabpack eventually and just these few lines 
  ##    will eventually be all you need!
  
  NBS_bioabund <- crabpack::calc_bioabund(crab_data = dat_NBS,
                                          species = species,
                                          region = "NBS",
                                          years = c(1982:maxyr),
                                          spatial_level = "region")
  
  # bind regions and format
  bioabund_combined <- rbind(EBS_bioabund, NBS_bioabund) |>
    dplyr::left_join(species_lookup) |>
    dplyr::select(YEAR, SPECIES_CODE, SPECIES, REGION,
                  ABUNDANCE, ABUNDANCE_CV, ABUNDANCE_CI,
                  BIOMASS_MT, BIOMASS_MT_CV, BIOMASS_MT_CI)
  bioabund_out <- rbind(bioabund_out, bioabund_combined)
  
  
  ## CPUE ----------------------------------------------------------------------
  # **Need to convert from mt/nmi2 to kg/km2
  EBS_cpue <- crabpack::calc_cpue(crab_data = dat_EBS,
                                  species = species,
                                  region = "EBS",
                                  years = c(2010:maxyr))
  NBS_cpue <- crabpack::calc_cpue(crab_data = dat_NBS,
                                  species = species,
                                  region = "NBS",
                                  years = c(2010:maxyr))
  
  # bind regions and format
  cpue_combined <- rbind(EBS_cpue, NBS_cpue) |>
    dplyr::left_join(species_lookup) |>
    dplyr::left_join(haul) |>
    dplyr::select(YEAR, SPECIES_CODE, SPECIES, REGION,
                  HAULJOIN, STATION_ID, LATITUDE, LONGITUDE,
                  DISTRICT, STRATUM, TOTAL_AREA,
                  COUNT, CPUE, CPUE_MT)
  cpue_out <- rbind(cpue_out, cpue_combined)
  
  
  ## NBS 1mm abundance -------------------------------- ------------------------
  NBS_pop1mm_male <- crabpack::calc_bioabund(crab_data = dat_NBS,
                                             species = species,
                                             region = "NBS",
                                             years = c(2010:maxyr),
                                             spatial_level = "region",
                                             sex = "male",
                                             bin_1mm = TRUE) |>
    rename(CATEGORY = SEX_TEXT)
  NBS_pop1mm_female <- crabpack::calc_bioabund(crab_data = dat_NBS,
                                               species = species,
                                               region = "NBS",
                                               years = c(2010:maxyr),
                                               spatial_level = "region",
                                               crab_category = c("mature_female", "immature_female"),
                                               bin_1mm = TRUE)
  
  # bind regions and format
  NBS_pop1mm <- dplyr::bind_rows(NBS_pop1mm_male, NBS_pop1mm_female) |>
    dplyr::left_join(species_lookup) |>
    dplyr::select(YEAR, SPECIES_CODE, SPECIES, REGION, SIZE_1MM, CATEGORY, ABUNDANCE) |>
    tidyr::pivot_wider(names_from = CATEGORY, values_from = ABUNDANCE) |>
    dplyr::rename(NUMBER_MALES = male,
                  NUMBER_IMMATURE_FEMALES = immature_female,
                  NUMBER_MATURE_FEMALES = mature_female)
  NBS_pop1mm_out <- rbind(NBS_pop1mm_out, NBS_pop1mm)
  
  
  ## EBS 1mm abundance ---------------------------------------------------------
  EBS_pop1mm_male <- crabpack::calc_bioabund(crab_data = dat_EBS,
                                             species = species,
                                             region = "EBS",
                                             years = c(1982:maxyr),
                                             spatial_level = "region",
                                             sex = "male",
                                             bin_1mm = TRUE) |>
    rename(CATEGORY = SEX_TEXT)
  EBS_pop1mm_female <- crabpack::calc_bioabund(crab_data = dat_EBS,
                                               species = species,
                                               region = "EBS",
                                               years = c(1982:maxyr),
                                               spatial_level = "region",
                                               crab_category = c("mature_female", "immature_female"),
                                               bin_1mm = TRUE)
  
  # bind regions and format
  EBS_pop1mm <- dplyr::bind_rows(EBS_pop1mm_male, EBS_pop1mm_female) |>
    dplyr::left_join(species_lookup) |>
    dplyr::select(YEAR, SPECIES_CODE, SPECIES, REGION, SIZE_1MM, CATEGORY, ABUNDANCE) |>
    tidyr::pivot_wider(names_from = CATEGORY, values_from = ABUNDANCE) |>
    dplyr::rename(NUMBER_MALES = male, 
                  NUMBER_IMMATURE_FEMALES = immature_female,
                  NUMBER_MATURE_FEMALES = mature_female)
  
  ## 1mm abundance -------------------------------------------------------------
  EBS_pop1mm_out <- rbind(EBS_pop1mm_out, EBS_pop1mm)
  
}

crab_spp <- data.frame(
  species_code = c(69322, 69323, 68560, 68580, 68590, 69400),
  taxon = "invert",
  common_name = c("red king crab", "blue king crab", "Tanner crab", "snow crab", "hybrid Tanner crab", "horsehair crab"), 
  species_name = c("Paralithodes camtschaticus", "Paralithodes platypus", "Chionoecetes bairdi", "Chionoecetes opilio", "Chionoecetes hybrid", "Erimacrus isenbeckii"),
  species = c("RKC", "BKC", "TANNER", "SNOW", "HYBRID", "HAIR")  )

crab_sizecomp <- 
  dplyr::bind_rows(EBS_pop1mm_out, NBS_pop1mm_out)  |> 
  dplyr::rename_all(tolower)  |> 
  tidyr::pivot_longer(cols = c("number_males", "number_immature_females", "number_mature_females"), 
                      names_to = "sex", 
                      values_to = "population_count") |>
  dplyr::left_join(crab_spp) |> 
  dplyr::mutate(
    sex = gsub(pattern = "number_", replacement = "", x = sex),
    sex = gsub(pattern = "_", replacement = " ", x = sex),
    srvy = region, 
    length_mm = size_1mm, 
    population_count = ifelse(is.na(population_count), 0, population_count),
    survey_definition_id = dplyr::case_when(
      region == "NBS" ~ 143, 
      region == "EBS" ~ 98)) |> 
  dplyr::select(-region, -size_1mm, -species) 

write.csv(x = crab_sizecomp, 
          file = here::here("data/crab_sizecomp.csv"), 
          row.names = FALSE)

crab_cpue <- cpue_out |> 
  dplyr::rename_all(tolower)  |> 
  dplyr::left_join(crab_spp) |> 
  dplyr::mutate(
    cpue_nokm2 = cpue * 3.4299, 
    cpue_kgkm2 = cpue_mt * 3.4299, 
    srvy = region, 
    weight_kg = NA, 
    survey_definition_id = dplyr::case_when(
      region == "NBS" ~ 143, 
      region == "EBS" ~ 98)) |> 
  dplyr::select(year, species_code, common_name, hauljoin, count, cpue_nokm2, cpue_kgkm2, 
                taxon, species_name, srvy, survey_definition_id) # does not have weight_kg

write.csv(x = crab_cpue, 
          file = here::here("data/crab_cpue.csv"), 
          row.names = FALSE)

crab_biomass <- bioabund_out |> 
  dplyr::rename_all(tolower)  |> 
  dplyr::mutate(
    n_weight = 1, # TOLEDO - doesn't come from crabpack!
    biomass_up = biomass_mt + biomass_mt_ci, # TOLEDO - still need to check these are calculated the same way
    biomass_dw = biomass_mt - biomass_mt_ci, 
    population_up = abundance + abundance_ci,
    population_dw = abundance - abundance_ci,
    srvy = region, 
   survey_definition_id = dplyr::case_when(
      region == "NBS" ~ 143, 
      region == "EBS" ~ 98)) |> 
  dplyr::left_join(crab_spp) |> 
  dplyr::select(year, species_code, species, 
                population_count = abundance, # abundance_cv abundance_ci 
                biomass_mt = biomass_mt, # biomass_mt_cv biomass_mt_ci, 
                biomass_up, 
                biomass_dw, 
                population_up,
                population_dw, 
                n_weight, 
                taxon, common_name, species_name, 
                srvy, survey_definition_id) # does not have population_var or biomass_var, cpue_kgkm2_mean cpue_nokm2_mean n_haul n_weight n_count n_length

write.csv(x = crab_biomass, 
          file = here::here("data/crab_biomass.csv"), 
          row.names = FALSE)

# Find changes since data report was last published ----------------------------

source("Z:/Projects/ConnectToOracle.R")
source("https://raw.githubusercontent.com/afsc-gap-products/gap_products/refs/heads/main/functions/summarize_gp_updates.R")
diff <- summarize_gp_updates(channel = channel_products,
                             time_start = dl_change_start,
                             time_end = dl_change_end)

if (nrow(diff) > 0) {

diff <- diff |>
  dplyr::filter(SURVEY_DEFINITION_ID %in% c(98, 143)) |>
  dplyr::arrange(SURVEY_DEFINITION_ID) |>
  dplyr::mutate(OPERATION_TYPE = dplyr::case_when(
    OPERATION_TYPE == "UPDATE" & NUMBER_RECS == 1 ~ "update",
    OPERATION_TYPE == "INSERT" & NUMBER_RECS == 1 ~ "insertion",
    OPERATION_TYPE == "DELETE" & NUMBER_RECS == 1 ~ "deletion",
    OPERATION_TYPE == "UPDATE" ~ "updates",
    OPERATION_TYPE == "INSERT" ~ "insertions",
    OPERATION_TYPE == "DELETE" ~ "deletions"
  ),
  TABLE_NAME_order = dplyr::case_when(
    TABLE_NAME == "AGECOMP" ~ 4,
    TABLE_NAME == "SIZECOMP" ~ 3,
    TABLE_NAME == "BIOMASS" ~ 2,
    TABLE_NAME == "CPUE" ~ 1
  ),
  TABLE_NAME = dplyr::case_when(
    TABLE_NAME == "AGECOMP" ~ "age composition",
    TABLE_NAME == "SIZECOMP" ~ "size composition",
    TABLE_NAME == "BIOMASS" ~ "biomass",
    TABLE_NAME == "CPUE" ~ "catch per unit effort"
  ),
  SURVEY_DEFINITION_ID = dplyr::case_when(
    SURVEY_DEFINITION_ID == 98 ~ "eastern Bering Sea",
    SURVEY_DEFINITION_ID == 143 ~ "northern Bering Sea"
  ))

diff_maxyr <- diff |>
  dplyr::filter(YEAR == maxyr)|>
  dplyr::group_by(TABLE_NAME, TABLE_NAME_order, OPERATION_TYPE, SURVEY_DEFINITION_ID)|>
  dplyr::summarise(NO_RECS = sum(NUMBER_RECS, na.rm = TRUE)) |>
  dplyr::ungroup()|>
  dplyr::arrange(SURVEY_DEFINITION_ID, TABLE_NAME_order) |>
  dplyr::mutate(year_min = maxyr,
                year_max = maxyr)

diff_notmaxyr_years <- diff|>
  dplyr::filter(YEAR != maxyr)|>
  dplyr::group_by(TABLE_NAME, TABLE_NAME_order, SURVEY_DEFINITION_ID)|>
  dplyr::summarise(year_min = min(YEAR, na.rm = TRUE),
                   year_max = max(YEAR, na.rm = TRUE))|>
  dplyr::ungroup()

diff_notmaxyr <- diff|>
  dplyr::filter(YEAR != maxyr)|>
  dplyr::group_by(TABLE_NAME, TABLE_NAME_order, OPERATION_TYPE, SURVEY_DEFINITION_ID)|>
  dplyr::summarise(NO_RECS = sum(NUMBER_RECS, na.rm = TRUE))|>
  dplyr::ungroup()|>
  dplyr::arrange(SURVEY_DEFINITION_ID, TABLE_NAME_order)|>
  dplyr::full_join(diff_notmaxyr_years)

changes_since_string <- function(diff0, str_year, maxyr) {
  str0 <- c()
  for (ii in unique(diff$SURVEY_DEFINITION_ID)) {
    temp1 <- diff0|>
      dplyr::filter(SURVEY_DEFINITION_ID == ii)
    str0 <- paste0(str0, ifelse(ii == unique(diff0$SURVEY_DEFINITION_ID)[1],
                                paste0("In ", str_year), "Similarly"),
                   ", the ", ii, " ")

    for (i in unique(diff0$TABLE_NAME)) {
      temp <- temp1|>
        dplyr::filter(TABLE_NAME == i)|>
        dplyr::arrange(desc(OPERATION_TYPE))

      if (temp$year_max[1] == maxyr){
        str0_years <- c()
      } else if (temp$year_min[1]==temp$year_max[1]) {
        str0_years <- paste0(" (", temp$year_min[1], ")")
      } else {
        str0_years <- paste0(" (", temp$year_min[1], "-", temp$year_max[1],")")
      }

      str0 <- paste0(str0,
                     ifelse(i != unique(diff0$TABLE_NAME)[1], "the ", ""), i, " table observed ",
                     text_list(paste0(formatC(x = temp$NO_RECS, digits = 0, big.mark = ","),
                                      " ", temp$OPERATION_TYPE)),
                     str0_years,
                     ifelse(i == unique(diff0$TABLE_NAME)[length(unique(diff0$TABLE_NAME))-1], "; and ",
                            ifelse(i == unique(diff0$TABLE_NAME)[length(unique(diff0$TABLE_NAME))], 
                                   ". ", "; ")))
    }
  }
  return(str0)
}

str_maxyr <- changes_since_string(
  diff0 = diff_maxyr, 
  str_year = maxyr, 
  maxyr = maxyr)
str_notmaxyr <- changes_since_string(
  diff0 = diff_notmaxyr, 
  str_year = paste0("the years before ", maxyr), 
  maxyr = maxyr)

str_data_changes <- paste0(str_maxyr, "\n\n", str_notmaxyr)
writeLines(text = str_data_changes, con = here::here("data", "str_data_changes.txt"))
}

# Date production data last updated --------------------------------------------

library(rvest)
last_production_run <- 
  read_html("https://afsc-gap-products.github.io/gap_products/content/intro-news.html")|> 
  html_element("p")|> 
  paste0() 

last_production_run <- strsplit(x = last_production_run, split = "/", fixed = TRUE)
last_production_run <- last_production_run[[1]][grep(pattern = ".txt", x = last_production_run[[1]])]
last_production_run <- strsplit(x = last_production_run, split = ".txt", fixed = TRUE)[[1]][1]
last_production_run <- date_formatter(last_production_run)
writeLines(text = last_production_run, con = here::here("data", "last_production_run.txt"))

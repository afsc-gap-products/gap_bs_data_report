
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
  "GAP_PRODUCTS.AKFIN_CPUE",
  "GAP_PRODUCTS.AKFIN_CRUISE",
  "GAP_PRODUCTS.AKFIN_HAUL",
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
                               c("GAP_PRODUCTS.AKFIN_SIZECOMP", "GAP_PRODUCTS.AKFIN_BIOMASS", 
                                 "GAP_PRODUCTS.AKFIN_CRUISE", "GAP_PRODUCTS.AKFIN_CPUE",
                                 "GAP_PRODUCTS.AKFIN_HAUL")) ) {
    end0 <- c(end0, paste0("YEAR IN (",paste0(maxyr:compareyr0, collapse = ","), ")"))
  }
  
  if ("YEAR" %in% names(a) & locations[i] %in% c("GAP_PRODUCTS.AKFIN_CPUE")) {
    end0 <- c(end0, paste0("YEAR IN (",text_list(x = maxyr:(maxyr-9), sep = ", ", sep_last2 = ", ", sep_last = ", "), ")"))
  }
  
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
  "crab.gap_ebs_nbs_abundance_biomass", # Biomass
  "crab.gap_ebs_nbs_crab_cpue", # CPUE
  "crab.ebscrab", # length data
  "crab.ebscrab_nbs", # length data
  "crab.nbs_size1mm_all_species", 
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

# Species Covered
# https://docs.google.com/spreadsheets/d/10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M/edit?usp=sharing
googledrive::drive_download(file = googledrive::as_id("10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M"),
                            type = "csv",
                            overwrite = TRUE,
                            path = paste0(dir_out_rawdata, "/species-local-names"))

# identify which species complexes you need
report_spp <- readr::read_csv(file = paste0(dir_out_rawdata, "/species-local-names.csv"), 
                              skip = 1, 
                              show_col_types = FALSE) %>%  
  dplyr::filter(grepl(x = species_code0, pattern = "c(", fixed = TRUE)) 

temp1 <- data.frame()
for (i in 1:nrow(report_spp)){
  temp2 <- eval(expr = parse(text = report_spp$species_code[i]))
  temp1 <- dplyr::bind_rows(temp1, 
                            dplyr::bind_cols(GROUP = report_spp$print_name[i], 
                                             SPECIES_CODE = eval(expr = parse(text = report_spp$species_code[i]))))
}

# follow instructions from https://afsc-gap-products.github.io/gapindex/articles/ex_species_complex.html
## Pull data. Note the format of the `spp_codes` argument with the GROUP column
library(gapindex)
production_data <- gapindex::get_data(
  year_set = c(1982:maxyr),
  survey_set = c("EBS"), #, "NBS"),
  spp_codes = temp1,
  pull_lengths = TRUE, 
  haul_type = 3, 
  abundance_haul = "Y", 
  taxonomic_source = "GAP_PRODUCTS.TAXONOMIC_CLASSIFICATION", # "RACEBASE.SPECIES", 
  sql_channel = channel)

## Zero-fill and calculate CPUE
production_cpue <- calc_cpue(racebase_tables = production_data)

## Calculate Biomass, abundance, mean CPUE, and associated variances by stratum
production_biomass_stratum <- 
  gapindex::calc_biomass_stratum(racebase_tables = production_data,
                                 cpue = production_cpue)

## Aggregate Biomass to subareas and region
production_biomass_subarea <- 
  calc_biomass_subarea(racebase_tables = production_data, 
                       biomass_strata = production_biomass_stratum)

## Calculate size composition by stratum. Note fill_NA_method == "BS" because
## our region is EBS, NBS, or BSS. If the survey region of interest is AI or 
## GOA, use "AIGOA". See ?gapindex::gapindex::calc_sizecomp_stratum for more
## details. 
production_sizecomp_stratum <- 
  gapindex::calc_sizecomp_stratum(
    racebase_tables = production_data,
    racebase_cpue = production_cpue,
    racebase_stratum_popn = production_biomass_stratum,
    spatial_level = "stratum",
    fill_NA_method = "BS")

## Aggregate size composition to subareas/region
production_sizecomp_subarea <- gapindex::calc_sizecomp_subarea(
  racebase_tables = production_data,
  size_comps = production_sizecomp_stratum)


## rbind stratum and subarea/region biomass estimates into one dataframe
names(x = production_biomass_stratum)[
  names(x = production_biomass_stratum) == "STRATUM"
] <- "AREA_ID"
production_biomass <- rbind(production_biomass_stratum, 
                            production_biomass_subarea)

## rbind stratum and subarea/region biomass estimates into one dataframe
names(x = production_sizecomp_stratum)[
  names(x = production_sizecomp_stratum) == "STRATUM"] <- "AREA_ID"
production_sizecomp <- 
  rbind(production_sizecomp_subarea,
        production_sizecomp_stratum[, names(production_sizecomp_subarea)])

production_cpue <- production_cpue %>% 
  dplyr::left_join(
    y = report_spp %>% dplyr::select(SPECIES_CODE = print_name, TAXON = taxon ), 
    relationship = "many-to-many")

production_biomass <- production_biomass %>% 
  dplyr::left_join(
    y = report_spp %>% dplyr::select(SPECIES_CODE = print_name, TAXON = taxon ), 
    relationship = "many-to-many")

production_sizecomp <- production_sizecomp %>% 
  dplyr::left_join(
    y = report_spp %>% dplyr::select(SPECIES_CODE = print_name, TAXON = taxon ), 
    relationship = "many-to-many")

write.csv(x = production_cpue, file = here::here("data/complex_cpue.csv"), row.names = FALSE)
write.csv(x = production_biomass, file = here::here("data/complex_biomass.csv"), row.names = FALSE)
write.csv(x = production_sizecomp, file = here::here("data/complex_sizecomp.csv"), row.names = FALSE)

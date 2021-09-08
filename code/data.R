#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Wrangle data
#' start date: 2021-03-03
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---


# Knowns -----------------------------------------------------------------------

# months.words<-c("January", "February",	"March", "April", 
#                 "May", "June", "July", "August", 
#                 "September", "October", "November", "December")

# Report Specific --------------------------------------------------------------

# shps<-c(21:25)

# *** Report Species ---------------------------------------------------------------


# Northern and Southern rock sole (grouped) = c(10262, 10261, 10263)
report_species_NEBS<-list("fish1" = # all plots and tables
                            list("Walleye pollock" = c(21740, 21741, 21742, 21744),
                                 'Pacific cod' = c(21720, 21721, 21722),
                                 "Yellowfin sole" = c(10210, 10209),
                                 "Northern rock sole" = c(10261, 10263), 
                                 "Flathead sole" = c(10130),
                                 "Bering flounder" = c(10140, 10141),
                                 "Alaska plaice" = c(10285),
                                 "Greenland turbot" = c(10115, 10116), 
                                 "Arrowtooth flounder" = c(10110),
                                 "Kamchatka flounder" = c(10112),
                                 "Pacific halibut" = c(10120, 10121)), 
                          "fish2" = # get some plots and go in # specimine collected table
                            list("Bering skate" = c(435), 
                                 "Alaska skate" = c(471), 
                                 "Longhead dab" = c(10211), 
                                 "Starry flounder" = c(10220), 
                                 "Yellow Irish lord" = c(21347), 
                                 "Plain sculpin" = c(21371), 
                                 "Great sculpin" = c(21370), 
                                 "Shorthorn sculpin" = c(21368), 
                                 "Pacific ocean perch" = c(30060)), 
                          "fish3" = list (
                            "Rex sole" = c(10200),
                            "Sahkalin sole" = c(10212), 
                            "Sturgeon poacher" = c(20040),
                            "Antlered sculpin" = c(21388),
                            "Arctic staghorn sculpin" = c(21315),
                            "Butterfly scuplin" = c(21348),
                            "Variegated snailfish" = c(22205), 
                            "Bigmouth sculpin" = c(21420),
                            "Arctic cod" = c(21725), 
                            "Saffron cod" = c(21735, 21736, 21737),
                            "Pacific herring" = c(21110), 
                            "Capelin" = c(23041), 
                            "Rainbow smelt" = c(23055), 
                            "Eulachon" = c(23010), 
                            "Shortfin eelpout" = c(24191),
                            "Wattled eelpout" = c(24185), 
                            "Marbled eelpout" = c(24184)
                          ), # get some plots
                          "invert1" = 
                            list("Red king crab" = c(69322), 
                                 "Blue king crab" = c(69323), 
                                 "Snow crab" = c(68580), # snow crab 
                                 "Tanner crab" = c(68560)), 
                          "invert3" = list(
                            "Purple-orange sea star" = c(81742), 
                            "Northern neptune snail" = c(71884), 
                            "Jellyfishes" = c(40500, 40501, 40502, 40503, 
                                              40504, 40505, 40506, 40507, 
                                              40508, 40509, 40510, 40511, 
                                              40512, 40513, 40515, 40519, 
                                              40520, 40560, 40561)) # group...
)

invert<-c("Porifera",
          "Cnidaria",
          "Platyhelminthes",
          "Nematoda",
          "Annelida",
          "Chromista",
          "Echinodermata",
          "Arthropoda",
          "Mollusca")

vert<-c("Urochordata",
        "Agnatha",
        "Chondrichthyes",
        "Sarcopterygii",
        "Tetrapoda",
        "Actinopterygii")

fish<-c("Agnatha",
        "Chondrichthyes",
        "Sarcopterygii",
        "Actinopterygii")

other<-c("Plantae",
         "Fungi",
         "Protozoa",
         "Bacteria",
         "Archaea")





# SpCodeName.General<-list("Walleye Pollock" = 934083, # Species	Gadus chalcogrammus Pallas, 1814 – Walleye ), 
#                          'Pacific cod' = 164711, #Species	Gadus macrocephalus Tilesius, 1810 – morue du Pacifique, bacalao del Pacifico, Pacific cod
#                          "Yellowfin Sole" = 172907, # Species	Limanda aspera (Pallas, 1814) – yellowfin sole, limande à nageoires jaunes, Yellowfin Sole 
#                          "Northern Rock Sole" = 616392, # Species	Lepidopsetta polyxystra Orr & Matarese, 2000 – northern rock sole, limande du nord, Northern Rock Sole
#                          "Southern Rock Sole" = 172917, # Species	Lepidopsetta bilineata (Ayres, 1855) – rock sole, fausse limande du Pacifique, Rock Sole
#                          "Flathead Sole" = 172875, # Species	Hippoglossoides elassodon Jordan & Gilbert, 1880 – flathead sole, Flathead Sole, plie à tête plate
#                          "Bering Flounder" = 172876, # Species	Hippoglossoides robustus Gill & Townsend, 1897 – Bering flounder, Bering Flounder, plie de Béring
#                          "Alaska Plaice" = 172901, # Species	Pleuronectes quadrituberculatus Pallas, 1814 – Alaska plaice, Alaska Plaice
#                          "Greenland Turbot" = 172930, #  Species	Reinhardtius hippoglossoides (Walbaum, 1792) – Greenland halibut, platija negra, Greenland turbot, Newfoundland turbot, turbot, greeenland halibut, flétan du Groenland, Greenland Halibut
#                          "Arrowtooth Flounder" = 172862, # Species	Atheresthes stomias (Jordan & Gilbert, 1880) – arrowtooth flounder, Arrowtooth Flounder, plie à grande bouche 
#                          "Kamchatka Flounder" = 172861, #  Species	Atheresthes evermanni Jordan & Starks, 1904 – Kamchatka flounder, Kamchatka Flounder
#                          "Pacific Halibut" = 172932) #Species: Hippoglossus stenolepis Schmidt, 1904 – valid)



# *** *** report types ---------------------------------------------------

# When you select a region using get_base_layers(), the grid will be clipped to only include stations in the survey region.  I haven't added NBS functionality to get_base_layers() since we do both surveys in the same year, but there is an easy workaround (third block of code below).

# library(akgfmaps)
# full_ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "auto", return.survey.grid = TRUE)
# ggplot() +
#   geom_sf(data = full_ebs_layers$survey.grid)
# 
# sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "auto", return.survey.grid = TRUE)
# ggplot() +
#   geom_sf(data = sebs_layers$survey.grid)
# image.png
# 
# nbs_grid <- full_ebs_layers$survey.grid %>% filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "nbs"))
# ggplot() +
#   geom_sf(data = nbs_grid)


dat_survreg <- data.frame(reg_shapefile = c("EBS_SHELF", "NBS_SHELF"), 
                           region_long = c("Eastern Bering Sea", "Northern Bering Sea"), 
                           region = c("EBS", "NBS"))

report_types <- list(
  "EBS" = list(
    sectname = "EBS-BTS-Report", 
    SURVEY = "eastern Bering Sea", 
    map.area = "bs.south", 
    SRVY1 = "EBS", 
    SRVY0 = "BS", # in Oracle
    SRVY00 = 143, # EBS
    station_id = akgfmaps::get_survey_stations(
      select.region = "sebs"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "sebs", 
      set.crs = "auto", 
      return.survey.grid = TRUE),
    report_species = report_species_NEBS), 
  "NEBS" = list(
    sectname = "NEBS-BTS-Report", 
    SURVEY = "northern and eastern Bering Sea",
    map.area = "bs.all", 
    SRVY1 = c("EBS", "NBS"), 
    SRVY0 = "BS", # in Oracle
    SRVY00 = c(98, #NBS
                143), # EBS
    station_id = akgfmaps::get_survey_stations(
      select.region = "ebs"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "ebs", 
      set.crs = "auto", 
      return.survey.grid = TRUE),
    report_species = report_species_NEBS)
)

a <- report_types[names(report_types) == SRVY][[1]]
for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }

placenames0 <- read.csv(file = system.file("data",
                                          file = "placenames.csv", package = "akgfmaps",
                                          mustWork = TRUE), stringsAsFactors = FALSE) %>%
  transform_data_frame_crs(out.crs = sf::st_crs(reg_dat$survey.strata))


# *** Load Oracle Data -------------------------------------------------------------

a<-list.files(path = here::here("data", "oracle"))
for (i in 1:length(a)){
  b <- read_csv(file = paste0(here::here("data", "oracle", a[i])))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = a[i]), value = b)
}

# *** Load Public Data -------------------------------------------------------

# load(here::here("data", "publicdata", "all_data.Rdata"))
# lastdl <- ageoffile(here::here("data", "publicdata", "all_data.Rdata"))   

# *** Load Biomass Design Based Estimates ----------------------------------------------

# df.ls <- list()
# df.ls0 <- gsub(pattern = , ".csv", replacement = "",
#          x = a[grep(pattern = "biomass_", x = a)])
# for (i in 1:length(df.ls0)){
#   df.ls$temp <- get(df.ls0[i])
#   df.ls$temp$file <- df.ls0[i]
#   df.ls$temp$SRVY <- toupper(strsplit(x = df.ls0[i], split = "_")[[1]][2])
#   names(df.ls)[i] <- df.ls0[i]
# }

df.ls<-list()

for (ii in 1:length(SRVY1)) {
  
  a<-list.files(path = here::here("data", "surveydesign", SRVY1[ii], "biomass"), full.names = TRUE)
  if (length(grep(pattern = "_plusnw", x = a, ignore.case = T)) > 0) {
    a <- a[grep(pattern = "_plusnw", x = a)]
  }
  
  for (i in 1:length(a)){
    b <- read_csv(file = a[i])
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1"){
      b$x1<-NULL
    }
    b$file <- a[i]
    b$survey <- SRVY1[ii]
    df.ls[[i]]<-b
    names(df.ls)[i]<-a[i]
  }
}

dat_biomass<-SameColNames(df.ls)

dat_biomass_maxyr<-dat_biomass %>% 
  dplyr::filter(year == maxyr)

dat_biomass_compareyr<-dat_biomass %>% 
  dplyr::filter(year == compareyr_ebs)

dat_biomass_maxyr_1<-dat_biomass %>% 
  dplyr::filter(year == (maxyr-1))

# *** Load CPUE Design Based Estimates ----------------------------------------------

df.ls<-list()

for (ii in 1:length(SRVY1)) {

  a<-list.files(path = here::here("data", "surveydesign", SRVY1[ii], "CPUE"), full.names = TRUE)
  if (length(grep(pattern = "_plusnw", x = a, ignore.case = T)) > 0) {
    a <- a[grep(pattern = "_plusnw", x = a)]
  }

  for (i in 1:length(a)){
    b <- read_csv(file = a[i])
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1"){
      b$x1<-NULL
    }
    b$file <- a[i]
    b$survey <- SRVY1[ii]
    df.ls[[i]]<-b
    names(df.ls)[i]<-a[i]
  }
}

dat_cpue<-SameColNames(df.ls)

dat_cpue_maxyr<-dat_cpue %>% 
  dplyr::filter(year == maxyr)

dat_cpue_compareyr<-dat_cpue %>% 
  dplyr::filter(year == compareyr_ebs)

dat_cpue_maxyr_1<-dat_cpue %>% 
  dplyr::filter(year == (maxyr-1))

# *** *** stratum_area (survey area) -----------------------------------------------

if (sum((maxyr - stratum$year)<0 %in% TRUE) == 0) {
  # if there are no stratum years greater, use the most recent year
  strat_yr <- max(stratum$year)
} else {
  # if the maxyr is less than the max stratum year, use the stratum yr next less
  strat_yr <- stratum$year[which.min((maxyr - stratum$year)[(maxyr - stratum$year)>=0])]
}

stratum_area <- stratum %>% 
  # dplyr::right_join(x = ., y = domain, by = "stratum") %>%
  filter(region == SRVY0 &
           year <= maxyr & 
           stratum < 100 &
           # !(stratum %in%  c(70, 71, 81)) &
           stratum %in% reg_dat$survey.strata$Stratum) %>% # TOLEDO
  filter(year == strat_yr)

#G:\HaehnR\rScripts\working on for techmemo\tables_TechMemo\code\Fig_1_stratra_area_hauls.R
# ## year = 2019 is most up to date- not updated every year
# strata_area <- sqlQuery(channel, "SELECT STRATUM, AREA
# 
# FROM RACEBASE.STRATUM
# 
# WHERE region = 'BS' AND
# year = 2019 AND
# stratum < 100 AND stratum != 70 AND
# stratum != 71 AND stratum != 81
# ORDER BY STRATUM;
#                          ")

# *** *** cruises ------------------------------------------------------------------

cruises <- cruises %>% 
  dplyr::select(cruise_id,  year, survey_name, vessel_id, cruise, 
                vessel_name, start_date, end_date, cruisejoin, survey_definition_id) %>% 
  dplyr::mutate(vess_shape = substr(x = vessel_name, 1,1)) %>%
  dplyr::mutate(vessel_name = paste0("F/V *", stringr::str_to_sentence(vessel_name), "*")) %>%
  dplyr::mutate(start_month = format(x = as.POSIXlt(x = start_date), format="%m")) %>% 
  dplyr::mutate(end_month = format(x = as.POSIXlt(x = end_date), format="%m")) %>% 
  dplyr::left_join(x = ., 
                   y = data.frame(survey_definition_id = c(143, 98, 47), 
                                  SRVY = c("NBS", "EBS", "GOA"), 
                                  SRVY_long = c("northern Bering Sea", 
                                                "eastern Bering Sea", 
                                                "Gulf of Alaska"), 
                                  SRVY_start = c(2010, 1982, NA)), 
                   by  = "survey_definition_id")

cruises_maxyr <- cruises %>%
  dplyr::filter(
    year == maxyr & 
      survey_definition_id %in% SRVY00) 

# format(x = as.POSIXlt(x =cruises_maxyr$start_date), format="%B %d %Y")
cruises_maxyr$start_month_long <- format(x = as.POSIXlt(x =cruises_maxyr$start_date), format="%B")
cruises_maxyr$end_month_long <- format(x = as.POSIXlt(x =cruises_maxyr$end_date), format="%B")

# *** *** haul_cruises -------------------------------------------------------------

haul_cruises<-dplyr::left_join(
  x = haul %>% dplyr::select(-cruise, -region, -auditjoin), 
  y = cruises, 
  by = "cruisejoin") %>% 
  dplyr::filter(
    abundance_haul == "Y" & 
      # performance >= 0 &
      haul_type == 3 &
      !(is.null(stationid)) &
      survey_definition_id %in% SRVY00
  )  %>% 
  dplyr::mutate(performance_success =
                  dplyr::case_when(
                    performance >= 0 ~ 1, 
                    TRUE ~ 0))

haul_cruises_maxyr <- haul_cruises %>%
  dplyr::filter(
    year %in% maxyr  &
      survey_definition_id %in% SRVY00) 

cruises_maxyr <- cruises_maxyr %>%
  dplyr::left_join(x = ., 
                   y = dplyr::full_join(x = haul_cruises_maxyr %>%
                                          dplyr::select(SRVY, stationid, 
                                                        vessel_name) %>%
                                          unique(.) %>%
                                          group_by(SRVY, vessel_name) %>%
                                          dplyr::count() %>% 
                                          dplyr::rename(survey_n = n), 
                                        y = haul_cruises_maxyr %>%
                                          dplyr::select(SRVY, stationid,
                                                        performance_success, 
                                                        vessel_name) %>%
                                          unique(.) %>%
                                          group_by(SRVY, vessel_name) %>%
                                          dplyr::summarize(performance_success = 
                                                             sum(performance_success)), 
                                        by = c("SRVY", "vessel_name")),
                   by = c("SRVY", "vessel_name"))

haul_cruises <- haul_cruises %>% 
  dplyr::filter(performance >= 0)

haul_cruises_maxyr <- haul_cruises_maxyr %>% 
  dplyr::filter(performance >= 0)

# *** *** surv_info ------------------------------------------

surv_info<- cruises_maxyr %>% 
  dplyr::select(SRVY, survey_n) %>% 
  dplyr::group_by(SRVY) %>% 
  dplyr::mutate(survey_nn = sum(survey_n)) %>%
  dplyr::select(-survey_n) %>%
  unique() %>%
  dplyr::left_join(x = ., 
                   y = cruises_maxyr %>% 
  dplyr::select("survey_definition_id", "SRVY", "SRVY_long", "SRVY_start") %>% 
  unique() %>%
  dplyr::left_join(x = ., 
                   y = haul_cruises %>% 
                     dplyr::filter(year <= maxyr) %>%
                     dplyr::select(year, survey_definition_id) %>%
                     unique() %>%
                     # dplyr::group_by(survey_definition_id) %>%
                     dplyr::count(vars = survey_definition_id) %>%
                     dplyr::rename(yrofsurvey = n, 
                                   survey_definition_id = vars), 
                   by = "survey_definition_id") %>% 
  # dplyr::mutate(yrofsurvey = maxyr+1-SRVY_start) %>% 
  dplyr::mutate(stndth = NMFSReports::stndth(yrofsurvey)), 
    by = "SRVY") %>%
  dplyr::arrange(SRVY)

surv_info$compareyr <- c(compareyr_ebs, if(exists("compareyr_nbs")) {compareyr_nbs} )
surv_info$compareyr_ref <- c(ref_compareyr_ebs, if(exists("ref_compareyr_nbs")) {ref_compareyr_nbs} )

# *** *** catch_haul_cruises, dat, catch_haul_cruises_maxyr, dat_maxyr ---------
catch_haul_cruises<-dplyr::left_join(x = haul_cruises, 
                                     y = catch %>% 
                                       dplyr::select(cruisejoin, hauljoin,
                                                     species_code, weight,
                                                     number_fish, subsample_code), 
                                     by = c("hauljoin", "cruisejoin")) 

dat <- catch_haul_cruises

# This year's data
dat_maxyr <-
  catch_haul_cruises_maxyr <-
  catch_haul_cruises %>%
  dplyr::filter(
    year %in% maxyr &
      survey_definition_id %in% SRVY00)

# Previous year's data
dat_maxyr_1 <- 
  catch_haul_cruises_maxyr_1 <- 
  catch_haul_cruises %>%
  dplyr::filter(
    year %in% (maxyr-1) &
      survey_definition_id %in% SRVY00)

# Year of survey
# yrofsurvey <- length(unique(catch_haul_cruises$year))
# stndth0 <- stndth(yrofsurvey)

# *** *** haul_info ------------------------------------------------------------

haul_info <- hauls %>% 
  dplyr::select("cruise_id", "haul", "station", "stratum", "performance",  "haul_type", "gear")

# haul_info <- sqlQuery(channel, "SELECT
# CRUISE_ID, HAUL, STATION, STRATUM, PERFORMANCE, HAUL_TYPE, GEAR
# FROM
# RACE_DATA.HAULS")
# 
# 
## dataframe to assign domain (inner, middle, outer)


# latex(tablular( (Factor(strata_domain$domain, "Stratum"))))

# *** *** vessel_info ----------------------------------------------------------

vessel_info <- haul_cruises_maxyr %>%
  dplyr::select("vessel", "vessel_id", "vessel_name", 
                "SRVY", "start_date", "end_date")  %>% 
  unique() %>% 
  dplyr::left_join(x = . , 
                   y = vessels, 
                   by = "vessel_id") %>% 
  dplyr::rename(length_ft = length) %>% 
  dplyr::mutate(length_m = round(length_ft/3.28084, 
                                  digits = 1)) %>%
  dplyr::mutate(name_ital = paste0("F/V *", 
                                stringr::str_to_title(name), 
                                "*")) %>% 
  dplyr::mutate(vess_shape = substr(x = name, 1, 1)) 

vessel_info1 <-  vessel_info %>% 
  dplyr::select("vessel", "vessel_id", "vessel_name", "name", 
                "length_m", "length_ft", "name_ital", "vess_shape") %>% 
  unique()

# *** *** spp_info -------------------------------------------------------------

spp_info <- 
  dplyr::left_join(
    x = unique(catch_haul_cruises[, "species_code"]), 
    y = species_classification, 
    by = "species_code") %>%
  dplyr::left_join(x = . , 
                   y = species, 
                   by = "species_code") %>%
  dplyr::mutate(fish = class_taxon %in% c("Actinopterygii", "Chondrichthyes")) %>%#& #spp_info_maxyr$superclass_taxon %in% "Osteichythyes"
  dplyr::mutate(invert = (phylum_taxon != "Chordata"))


# species specifically caught in the survey year
spp_info_maxyr <- 
  dplyr::left_join(
    x = unique(catch_haul_cruises_maxyr[, "species_code"]), 
    y = species_classification, 
    by = "species_code") %>%
    dplyr::left_join(x = . , 
                     y = species, 
                     by = "species_code") %>%
  dplyr::mutate(fish = class_taxon %in% c("Actinopterygii", "Chondrichthyes")) %>%#& #spp_info_maxyr$superclass_taxon %in% "Osteichythyes"
  dplyr::mutate(invert = (phylum_taxon != "Chordata"))
  

# *** *** length ---------------------------------------------------------------

length <- length %>% 
  dplyr::filter(#species_code %in% spp_code, 
                hauljoin %in% unique(haul_cruises$hauljoin)) 

length <- dplyr::left_join(x = length, 
                               y = haul_cruises %>% 
                                 dplyr::select(stratum, hauljoin, SRVY, year))

length_maxyr <- length %>% 
  dplyr::filter(year == maxyr & 
                  region == SRVY0)

# *** *** length_type ----------------------------------------------------------

# length_type <- data.frame(matrix(data = c("1", "Fork length measurement,from tip of snout to fork of tail.",
#                                           "2", "Mideye to fork of tail.",
#                                           "3", "Tip of snout to hypural plate (standard).",
#                                           "4", "Mideye to hypural plate.",
#                                           "5", "Total length (extremity to extremity).",
#                                           "6", "Snout to second dorsal (e.g., Ratfish).",
#                                           "7", "Length of carapace from back of right eye socket to end of carapace.",
#                                           "8", "Width of carapace.",
#                                           "9", "Head length (snout tip to posterior opercular margin).",
#                                           "11", "Snout to anal fin origin (e.g., Rattails).",
#                                           "12", "Mantle length (e.g., Squid).",
#                                           "13", "Posterior of orbital to end of telson (e.g., Shrimp).",
#                                           "14", "Wingtip to wingtip (e.g., skates and rays)",
#                                           "15", "Outer tip of rostrum to end of telson (e.g., shrimp)",
#                                           "16", "Modal, created in merge juveniles script",
#                                           "17", "Length frequency estimated using size composition proportions from adjacent hauls with similar catch composition"), 
#                                  ncol = 2, byrow = TRUE))

# names(length_types) <- c("code", "description")
length_types$sentancefrag <- c("fork lengths",
                      "lengths from mideye to fork of the tail",
                      "lengths from the tip of snout to hypural plate",
                      "lengths from mideye to hypural plate",
                      "total lengths",
                      "snout to second dorsal lengths",
                      "lengths of carapace from back of right eye socket to the end of the carapace",
                      "carapace widths",
                      "head lengths",
                      "snout to anal fin origin lengths",
                      "mantle lengths",
                      "posterior of orbital to end of telson lengths",
                      "wingtip to wingtip lengths",
                      "outer tip of rostrum to end of telson lengths",
                      "modal lengths",
                      "frequency of lengths estimated using size composition proportions from adjacent hauls with similar catch composition")

# Specimen ------------------------------------------------

specimen <- specimen %>% 
  dplyr::filter(region == SRVY0) %>%
  dplyr::mutate(year = as.numeric(substr(cruise, start = 1, stop = 4))) %>% 
  dplyr::mutate(SRVY = (substr(cruise, start = 5, stop = 6))) %>% 
  dplyr::mutate(SRVY = dplyr::case_when(
    SRVY == "01" ~ "EBS", 
    SRVY == "02" ~ "NBS" 
  ))

specimen_maxyr <- specimen %>% 
  dplyr::filter(SRVY %in% SRVY1 & 
                  year== maxyr)


# Footnotes ------------------------

Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")

# 
# 
# minyr <- min(dat$YEAR)
# dat<-dat[dat$YEAR %in% minyr:maxyr, ]
# dat$COMMON_NAME<-dat$COMMON
# dat$STATIONID<-dat$STATION
# 
# # Time
# dates<-strsplit(x = dat$DATETIME, split = " ")
# dat$dates<-sapply(dates, "[", 1 )
# 
# dat$DAY<-as.numeric(substr(x = as.character(dat$dates), 
#                            start = 4, stop = 5))
# dat$MONTH<-as.numeric(substr(x = as.character(dat$dates), 
#                              start = 1, stop = 2))
# 
# 
# months.words<-c("January", "February",	"March", "April", 
#                 "May", "June", "July", "August", 
#                 "September", "October", "November", "December")
# 
# 
# 
# load(file = "./data/specieslistinTSN.rdata")
# reftable<-spp.cat2$reftable
# reftable<-dplyr::rename(reftable, 
#                         "SID" = "SID_orig")
# dat<-left_join(dat, reftable, "SID")
# 
# 
# dat$PERFORMANCE<-0 # TOLEDO
# dat$HAULTYPE<-3 # TOLEDO
# 
# dat<-dat[dat$PERFORMANCE == 0 & dat$HAULTYPE == 3, ]
# spp.tsn.list<-spp.cat2$tsn.list
# dat.maxyr<-dat[dat$YEAR %in% maxyr,]


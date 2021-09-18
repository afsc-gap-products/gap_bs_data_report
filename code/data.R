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
                                 "Pacific ocean perch" = c(30060)), # extra to 2017
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



# *** report types ---------------------------------------------------

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


# dat_survreg <- data.frame(reg_shapefile = c("EBS_SHELF", "NBS_SHELF"), 
#                            region_long = c("Eastern Bering Sea", "Northern Bering Sea"), 
#                            region = c("EBS", "NBS"))

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
  "NBS" = list(
    sectname = "NBS-BTS-Report", 
    SURVEY = "northern Bering Sea", 
    map.area = "bs.north", 
    SRVY1 = "NBS", 
    SRVY0 = "BS", # in Oracle
    SRVY00 = 98, # EBS
    station_id = akgfmaps::get_survey_stations(
      select.region = "nbs"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "nbs", 
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

# TOLEDO
report_types$EBS$reg_dat$survey.strata$Stratum[
  report_types$EBS$reg_dat$survey.strata$Stratum == 30]<-31
report_types$NEBS$reg_dat$survey.strata$Stratum[
  report_types$NEBS$reg_dat$survey.strata$Stratum == 30]<-31

a <- report_types[names(report_types) == SRVY][[1]]
for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }

placenames0 <- read.csv(file = system.file("data",
                                           file = "placenames.csv", package = "akgfmaps",
                                           mustWork = TRUE), stringsAsFactors = FALSE) %>%
  transform_data_frame_crs(out.crs = sf::st_crs(reg_dat$survey.strata))

# Load data --------------------------------------------------------------------

# *** Load Documents from Google Drive -----------------------------------------
if (googledrive_dl) {
  
  # Spreadsheets
  a <- googledrive::drive_ls(path = dir_googledrive, type = "spreadsheet")
  for (i in 1:nrow(a)){
    googledrive::drive_download(paste0(dir_googledrive, a$name[i]), 
                                type = "csv", 
                                overwrite = TRUE, 
                                path = paste0(dir_out_rawdata, "/tab_", a$name[i]))
  }
  
  # Word documents
  a <- googledrive::drive_ls(path = dir_googledrive, type = "document")
  for (i in 1:nrow(a)){
    googledrive::drive_download(paste0(dir_googledrive, a$name[i]), 
                                type = "docx", 
                                overwrite = TRUE, 
                                path = paste0(dir_out_rawdata, "/doc_", a$name[i]))
  }
  
}
# *** Load Oracle Data -------------------------------------------------------------

a<-list.files(path = here::here("data", "oracle"))
for (i in 1:length(a)){
  b <- read_csv(file = paste0(here::here("data", "oracle", a[i])))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = paste0(a[i], "0")), value = b)
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

biomass <- SameColNames(df.ls)  %>%
  dplyr::filter(year <= maxyr) %>% 
  dplyr::rename(SRVY = survey) %>% 
  dplyr::mutate(fish = (species_code <= 31550)) %>%
  dplyr::mutate(invert = (species_code >= 40001))

biomass_maxyr<-biomass %>% 
  dplyr::filter(year == maxyr)

biomass_compareyr<-biomass %>% 
  dplyr::filter(year == compareyr)

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

cpue <- SameColNames(df.ls)  %>%
  dplyr::rename(SRVY = survey) %>% 
  dplyr::filter(year <= maxyr) %>% 
  dplyr::mutate(fish = (species_code <= 31550)) %>%
  dplyr::mutate(invert = (species_code >= 40001))

cpue_maxyr <- cpue %>% 
  dplyr::filter(year == maxyr)

cpue_compareyr<- cpue %>% 
  dplyr::filter(year == compareyr)

# Wrangle Data -----------------------------------------------------------------

# *** stratum_info (survey area) -------------------------------------------

temp <- function(yr) {
  
  # # unique  and sort are not necessary, just easier for troubleshooting
  # if (sum(yr<unique(stratum0$year)) == 0) {
  # # if (sum((yr - stratum0$year)<0 %in% TRUE) == 0) {
  #   # if there are no stratum years greater than yr, use the most recent stratum year
    strat_yr <- max(stratum0$year)
  # } else {
  #   # if the yr is less than the max stratum year, use the stratum yr next less
  #   temp <- sort(unique(stratum0$year))
  #   strat_yr <- temp[which((yr - temp)>-1)[length(which((yr - temp)>-1))]]
  #   # strat_yr <- sort(unique(stratum0$year))[which.min((yr - sort(unique(stratum0$year)))[(yr - sort(unique(stratum0$year)))>=0])]
  # }
  # 
  
stratum_info <- stratum0 %>% 
  dplyr::filter(
    stratum %in% reg_dat$survey.strata$Stratum &
      year == strat_yr) %>%
  dplyr::mutate(depth = gsub(pattern = "> 1", 
                             replacement = ">1", 
                             x = description)) %>% 
  dplyr::mutate(depth =
                  gsub(pattern = "[a-zA-Z]+",
                       replacement = "",
                       x = sapply(X = strsplit(
                         x = depth,
                         split = " ",
                         fixed = TRUE),
                         function(x) x[1])
                  )) %>% 
  dplyr::select(-auditjoin, -portion) %>%
  dplyr::mutate(SRVY = dplyr::case_when(
    stratum %in% as.numeric(report_types$EBS$reg_dat$survey.strata$Stratum) ~ "EBS", 
    stratum %in% as.numeric(report_types$NBS$reg_dat$survey.strata$Stratum) ~ "NBS" 
  )) %>% 
  dplyr::filter(SRVY %in% SRVY1) %>% 
  dplyr::mutate(type = dplyr::case_when( 
    SRVY == "NBS" ~ "Shelf",
    depth %in% "<50" ~ "Inner Shelf", 
    depth %in% c("50-100", ">50") ~ "Middle Shelf", 
    depth %in% c("100-200", ">100") ~ "Outer Shelf"
  )) 

  return(stratum_info)

}

stratum_info <- temp(maxyr)

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


# *** station_info ------------------------------------------------------------------

station_info <- stations0 %>%
  dplyr::filter(stratum %in% reg_dat$survey.strata$Stratum) %>% 
  dplyr::left_join(x = ., 
                   y = stratum_info %>% 
                     dplyr::select(stratum, SRVY), 
                   by = "stratum")

# *** cruises + maxyr  + compareyr -----------------------------------------------

cruises <- cruises0 %>% 
  dplyr::select(cruise_id,  year, survey_name, vessel_id, cruise, survey_definition_id, 
                vessel_name, start_date, end_date, cruisejoin) %>% 
  dplyr::filter(year != 2020 & # no surveys happened this year that I care about
                  year >= 1982 &
                  year <= maxyr &
                  survey_definition_id %in% SRVY00) %>% 
  dplyr::mutate(vess_shape = substr(x = vessel_name, 1,1)) %>%
  dplyr::mutate(vessel_ital = paste0("F/V *", stringr::str_to_title(vessel_name), "*")) %>%
  dplyr::mutate(vessel_name = paste0("F/V ", stringr::str_to_title(vessel_name))) %>%
  dplyr::left_join(
    x = ., 
    y = data.frame(survey_definition_id = c(143, 98, 47), 
                   SRVY = c("NBS", "EBS", "GOA"), 
                   SRVY_long = c("northern Bering Sea", 
                                 "eastern Bering Sea", 
                                 "Gulf of Alaska"), 
                   SRVY_start = c(2010, 1982, NA)), 
    by  = "survey_definition_id") %>% 
  dplyr::rename(vessel = "vessel_id", 
                start_date_cruise = start_date, 
                end_date_cruise = end_date)
# dplyr::mutate(start_month_long = format(x = as.POSIXlt(x = start_date), format="%B")) %>%
# dplyr::mutate(end_month_long = format(x = as.POSIXlt(x = end_date), format="%B")) #%>% 
# dplyr::left_join(x = ., 
#                  y = haul %>% 
#                    dplyr::select(cruise, region, ))

cruises_maxyr <- cruises %>%
  dplyr::filter(
    year == maxyr & 
      survey_definition_id %in% SRVY00)

cruises_compareyr <- cruises %>%
  dplyr::filter(
    year == compareyr & 
      survey_definition_id %in% SRVY00)


# *** haul + maxyr ---------------------------------------------------------------------

haul <- haul0 %>%
  dplyr::left_join(x = ., 
                   y = cruises %>% 
                     dplyr::select(cruisejoin, survey_definition_id), 
                   by = "cruisejoin") %>%  
  dplyr::mutate(year = as.numeric(substr(x = cruise, 1,4))) %>% 
  dplyr::filter(abundance_haul == "Y" &
                  year <= maxyr &
                  performance >= 0 &
                  !(is.null(stationid)) &
                  haul_type == 3 & 
                  survey_definition_id %in% SRVY00) %>% 
  dplyr::select(-auditjoin) %>%  
  dplyr::mutate(SRVY = dplyr::case_when(
    survey_definition_id %in% 143 ~ "EBS",
    survey_definition_id %in% 98 ~ "NBS"
  ))   #%>%
  # dplyr::filter(SRVY %in% SRVY1) # %>%
  # dplyr::mutate(start_date_haul = 
  #                 format(x = as.POSIXlt(x = start_time), format="%Y-%m-%d"))

haul_maxyr <- haul %>% 
  dplyr::filter(year == maxyr)

haul_compareyr <- haul %>% 
  dplyr::filter(year == compareyr)


# *** stratum_info (survey area) (reprise) -------------------------------------

stratum_info <- 
  dplyr::left_join(
    x = stratum_info, 
    y = haul_maxyr %>% 
      dplyr::select(stratum, stationid) %>% 
      dplyr::group_by(stratum) %>% 
      dplyr::summarise(count(stratum)) %>% 
      dplyr::rename(stations_completed = "freq") %>% 
      dplyr::select(stratum, stations_completed), 
    by = "stratum") %>% 
  dplyr::left_join(
    x = ., 
    y = station_info %>% 
      dplyr::select(stratum, stationid) %>% 
      dplyr::group_by(stratum) %>% 
      dplyr::summarise(count(stratum)) %>% 
      dplyr::rename(stations_avail = "freq") %>% 
      dplyr::select(stratum, stations_avail), 
    by = "stratum") %>% 
  dplyr::left_join(
    x = ., 
    y = haul_maxyr %>% 
      dplyr::select(stratum, stationid, bottom_depth) %>% 
      dplyr::group_by(stratum) %>% 
      dplyr::summarise(depth_mean = mean(bottom_depth, na.rm = TRUE), 
                       depth_min = min(bottom_depth, na.rm = TRUE), 
                       depth_max = max(bottom_depth, na.rm = TRUE)), 
    by = "stratum") 
    

# *** haul_cruises_vess_maxyr + _compareyr -------------------------------------

temp <- function(cruises_, haul_){
  haul_cruises_vess_ <- 
    dplyr::left_join(x = cruises_ ,#%>% 
                       # dplyr::rename(start_date_cruise = start_date, 
                       #               end_date_cruise = end_date), 
                     y = haul_ %>% 
                       dplyr::select(cruisejoin, #hauljoin, 
                                     #vessel, cruise,  #haul, 
                                     stationid, start_time) %>% 
                       dplyr::group_by(cruisejoin) %>% 
                       dplyr::summarise(start_date_haul = min(start_time), 
                                        end_date_haul = max(start_time), 
                                        # cruisejoin = unique(cruisejoin),
                                        # hauljoin = unique(hauljoin),
                                        stations_completed = length(unique(stationid))), 
                     by = c("cruisejoin")) %>% 
    dplyr::left_join(x = . , 
                     y = vessels0 %>%
                       dplyr::rename(vessel = vessel_id) %>%
                       dplyr::select(vessel, length, tonnage), 
                     by = "vessel") %>% 
    dplyr::rename(length_ft = length) %>% 
    dplyr::mutate(length_m = round(length_ft/3.28084, 
                                   digits = 1)) 
}

haul_cruises_vess_maxyr <- temp(cruises_maxyr, haul_maxyr) 

haul_cruises_vess_compareyr <- temp(cruises_compareyr, haul_compareyr) 

# *** vessel_info -------------------------------------------------------

vessel_info <-  haul_cruises_vess_maxyr %>% 
  dplyr::select("vessel_name", "vessel_ital", "vessel", "tonnage",
                "length_m", "length_ft", "vess_shape") %>% 
  unique()

# *** haul_cruises_maxyr + _compareyr ------------------------------------------

temp <- function(haul_cruises_vess_){
  
  haul_cruises_ <- 
    dplyr::left_join(
      x = haul_cruises_vess_ %>% 
        dplyr::select("year", "survey_name", "cruise", "SRVY_start" , 
                      "survey_definition_id", "SRVY", "SRVY_long", #hauljoin, 
                      cruisejoin) %>%
        unique(), 
      y = haul_cruises_vess_ %>% 
        dplyr::select("cruise", "stations_completed", 
                      start_date_haul, end_date_haul, 
                      start_date_cruise, end_date_cruise) %>% 
        dplyr::group_by(cruise) %>% 
        dplyr::summarise(stations_completed = sum(stations_completed), 
                         start_date_haul = min(start_date_haul), 
                         end_date_haul = max(end_date_haul), 
                         start_date_cruise = min(start_date_cruise), 
                         end_date_cruise = max(end_date_cruise)), 
      by = "cruise") %>% 
    dplyr::left_join(
      x = ., 
      y = station_info %>% 
        dplyr::group_by(SRVY) %>% 
        dplyr::summarise(stations_avail = length(unique(stationid))),
      by = "SRVY") %>% 
    dplyr::left_join(
      x = ., 
      y = cruises %>% 
        dplyr::select(year, SRVY) %>%
        unique() %>%
        dplyr::count(vars = SRVY) %>%
        dplyr::rename(yrofsurvey = n, 
                      SRVY = vars), 
      by = "SRVY") %>% 
    dplyr::select(- cruisejoin) %>%
    dplyr::mutate(stndth = NMFSReports::stndth(yrofsurvey))  %>% 
    dplyr::arrange(SRVY) %>% 
    dplyr::mutate(compareyr = compareyr) %>%
    # c(compareyr_ebs, if(exists("compareyr_nbs")) {compareyr_nbs} )) %>% 
    dplyr::left_join(
      x = ., 
      y = data.frame(
        SRVY = SRVY1,
        compareyr_ref = c(ref_compareyr_ebs, if(exists("ref_compareyr_nbs")) {ref_compareyr_nbs} )), 
      by = "SRVY") %>%
    unique()
  
}

haul_cruises_maxyr <- temp(haul_cruises_vess_ = haul_cruises_vess_maxyr) 

haul_cruises_compareyr <- temp(haul_cruises_vess_compareyr) 

# *** catch_haul_cruises_maxyr + maxyr-1-----------------------------------------------

temp <- function(cruises_, haul_){
  # This year's data
  catch_haul_cruises_<-
    dplyr::left_join(
      x = haul_maxyr %>% 
        dplyr::select(cruisejoin, hauljoin, stationid, stratum), 
      y = cruises_maxyr %>% 
        dplyr::select(cruisejoin, survey_name, SRVY),  
      by = c("cruisejoin")) %>% 
    dplyr::left_join(
      x= ., 
    # dplyr::left_join(
    #   x = cruises_, 
    #   y = haul_, 
    #   by = c("cruisejoin")) %>% 
    # dplyr::left_join(
    #   x = ., 
      y = catch0 %>% 
        dplyr::select(cruisejoin, hauljoin,
                      species_code, weight,
                      number_fish, subsample_code), 
      by = c("hauljoin", "cruisejoin")) 
}

catch_haul_cruises_maxyr <- temp(cruises_maxyr, haul_maxyr)

catch_haul_cruises_compareyr <- temp(cruises_compareyr, haul_compareyr)


# *** spp_info + spp_info_maxyr ------------------------------------------------

# setdiff(unique(spp_info$species_code), 
#         find_codes(x = spp_info,  
#            str_not = c(" shells", "empty", "unsorted", "shab"#, " egg", "unid.", "compound"
#                        ), 
#            col_str_not = "common_name",
#            col_out = "species_code"))

spp_info <- 
  dplyr::left_join(x = species0, 
                   y = species_classification0, 
                   by = "species_code") %>% 
  dplyr::mutate(fish = (species_code <= 31550)) %>%
  dplyr::mutate(invert = (species_code >= 40001))

spp_info$used_in_counts <- 
                  ifelse(spp_info$species_code %in% #10:99988, 
                           find_codes(x = spp_info,  
                                      str_not = c(" shells", "empty", "unsorted", "shab"
                                                  #, " egg", "unid.", "compound"
                                      ),
                                      col_str_not = "common_name",
                                      col_out = "species_code"), 
                         TRUE, FALSE)  # remove " shells", "empty", "unsorted", "shab". May also consider removing " egg", "unid.", "compound"


# species specifically caught in the survey year
spp_info_maxyr <- spp_info %>% 
  dplyr::filter(species_code %in% unique(catch_haul_cruises_maxyr$species_code))


# *** length_maxyr ---------------------------------------------------------------

length_maxyr <- 
  dplyr::left_join(
    x = haul_maxyr %>% 
      dplyr::select(cruisejoin, hauljoin, stationid, stratum), 
    y = cruises_maxyr %>% 
      dplyr::select(cruisejoin, survey_name, SRVY),  
    by = c("cruisejoin")) %>% 
  dplyr::left_join(
    x= ., 
    y = length0,
    by = c("cruisejoin", "hauljoin")) %>% 
  dplyr::select(-auditjoin)


# *** length_type ----------------------------------------------------------

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
length_types <- length_types0
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

# *** Specimen + maxyr------------------------------------------------

specimen_maxyr <- 
  dplyr::left_join(
    x = haul_maxyr %>% 
      dplyr::select(cruisejoin, hauljoin, stationid, stratum), 
    y = cruises_maxyr %>% 
      dplyr::select(cruisejoin, survey_name, SRVY),  
    by = c("cruisejoin")) %>% 
  dplyr::left_join(
    x= ., 
    y = specimen0, 
    by = c("cruisejoin", "hauljoin")) %>% 
  dplyr::select(-region, cruisejoin, hauljoin)
  # dplyr::filter(region == SRVY0) %>%
  # dplyr::mutate(year = as.numeric(substr(cruise, start = 1, stop = 4))) %>% 
  # dplyr::mutate(SRVY = (substr(cruise, start = 5, stop = 6))) %>% 
  # dplyr::mutate(SRVY = dplyr::case_when(
  #   SRVY == "01" ~ "EBS", 
  #   SRVY == "02" ~ "NBS" 
  # ))

# specimen_maxyr <- specimen %>% 
#   dplyr::filter(SRVY %in% SRVY1 & 
#                   year== maxyr)

#***  Weighted bottom tempertures ------------------------


# temps_wt_avg_yr<-c()

# for (i in 1:length(unique(haul$year))){
  
  # yr <- sort(unique(haul$year))[i]

  # temp

## weighted mean pt 1
temps_wt_avg_strat <- stratum_info %>% #temp_strat(maxyr) %>%
    dplyr::filter(SRVY %in% SRVY1) %>%
    dplyr::select(stratum, area, SRVY) %>%
    dplyr::mutate(weight_all = area/sum(area)) %>% 
    group_by(SRVY) %>% 
    dplyr::mutate(weight_SRVY = area/sum(area)) %>% 
    dplyr::left_join(x = haul %>% 
                       # dplyr::filter(year == maxyr) %>%
                       dplyr::select(stratum, year, #stationid, 
                                     surface_temperature, 
                                     gear_temperature, bottom_depth), 
                     y = ., 
                     by = "stratum") %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year, stratum, SRVY) %>%
    dplyr::summarise(bt_wt_stratum = mean(gear_temperature * weight_SRVY, na.rm = TRUE), 
                     st_wt_stratum = mean(surface_temperature * weight_SRVY, na.rm = TRUE)) %>%
    dplyr::ungroup()  

temps_wt_avg_yr <- temps_wt_avg_strat %>%
    dplyr::group_by(year, SRVY) %>%
    dplyr::summarise(bt_wt = sum(bt_wt_stratum, na.rm = TRUE), 
                     st_wt = sum(st_wt_stratum, na.rm = TRUE)) %>%
    dplyr::filter(!is.na(SRVY)) %>%
    dplyr::filter(!is.nan(bt_wt) | !is.nan(st_wt))

  # temps_wt_avg_yr <- rbind.data.frame(temps_wt_avg_yr, temp)

# }

# long term mean pt 1
temps_wt_avg_yr_longterm <- temps_wt_avg_yr %>% 
  dplyr::filter(year != maxyr) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(SRVY) %>%
  dplyr::arrange(desc(year)) %>%
  dplyr::summarise(bt_wt_mean = mean(bt_wt, na.rm = TRUE), 
                   st_wt_mean = mean(st_wt, na.rm = TRUE))

## weighted mean pt 2
temps_wt_avg_yr <- temps_wt_avg_yr %>% 
  dplyr::ungroup() %>%
  dplyr::left_join(x = ., 
                   y = temps_wt_avg_yr_longterm, 
                   by  = "SRVY") %>% 
  dplyr::group_by(SRVY) %>% 
  dplyr::arrange(SRVY, year) %>%
  dplyr::mutate(bt_wt_above_mean = bt_wt>mean(bt_wt_mean, na.rm = TRUE)) %>% 
  dplyr::mutate(st_wt_above_mean = st_wt>mean(st_wt_mean, na.rm = TRUE)) %>% 
  dplyr::mutate(case = dplyr::case_when(
    ((st_wt_above_mean + bt_wt_above_mean)==2) ~ "both warmer",
    ((st_wt_above_mean + bt_wt_above_mean)==0) ~ "both colder",
    (st_wt_above_mean == TRUE & bt_wt_above_mean == FALSE) ~ "st warmer, bt colder",
    (st_wt_above_mean == FALSE & bt_wt_above_mean == TRUE) ~ "bt warmer, st colder") ) 

# calculate the nth year of case
nthyr <- c()
for (ii in 1:length(unique(temps_wt_avg_yr$SRVY))){
  temp <- temps_wt_avg_yr %>% 
    dplyr::filter(SRVY == unique(temps_wt_avg_yr$SRVY)[ii])
  for (i in 1:nrow(temp)) {
    if (i == 1) {
      nthyr0 <- 1
    } else if (temp$case[i] != temp$case[i-1]) {
      nthyr0 <- c(nthyr0, 1)
    } else {
      nthyr0 <- c(nthyr0, (nthyr0[length(nthyr0)]+1))
    }
  }
  nthyr <- c(nthyr, nthyr0)
}
temps_wt_avg_yr$nthyr <- nthyr
temps_wt_avg_yr <- temps_wt_avg_yr %>% 
  dplyr::arrange(desc(year))


temps_wt_avg_yr_longterm <- temps_wt_avg_yr %>% 
  dplyr::filter(year == maxyr)


# Footnotes ------------------------

# Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")

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


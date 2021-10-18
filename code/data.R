#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Wrangle data
#' start date: 2021-03-03
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---

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
                                 "Yellow irish lord" = c(21347), 
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
      select.region = "bs.south"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.south", 
      set.crs = "auto", 
      return.survey.grid = TRUE),
    report_species = report_species_NEBS), 
  "NBS" = list(
    sectname = "NBS-BTS-Report", 
    SURVEY = "northern Bering Sea", 
    map.area = "bs.north", 
    SRVY1 = "NBS", 
    SRVY0 = "BS", # in Oracle
    SRVY00 = 98,
    station_id = akgfmaps::get_survey_stations(
      select.region = "bs.north"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.north", 
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
      select.region = "bs.all"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.all", 
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

if(map.area %in% c("bs.south", "sebs")) {
  extrap.box <- c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 63)
} else if(map.area %in% c("bs.north", "nbs")) {
  extrap.box <- c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 68)
} else if(map.area %in% c("bs.all", "ebs")) {
  extrap.box <- c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 68)
}

# Load data --------------------------------------------------------------------


# *** Load Documents Cold Pool GEOTiff -----------------------------------------

# TOLEDO - waiting for coldpool package

# The geoTIFF files will look like this. You can mask them using something like this:

# proj_crs <-  "EPSG:3338"
# 
# sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = proj_crs)
# 
# temp_ste <- raster::raster(here::here("data", "coldpool", "ste_2021_gear_temperature.tif")) %>%
#   akgfmaps::rasterize_and_mask(amask = sebs_layers$survey.area)
# 
# plot(temp_ste)

# *** Load Documents from Google Drive -----------------------------------------
if (googledrive_dl) {
  
  # Species Covered
  # https://docs.google.com/spreadsheets/d/10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M/edit?usp=sharing
    # googledrive::drive_download(file = googledrive::as_id("10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M"),
    #                             type = "csv",
    #                             overwrite = TRUE,
    #                             path = paste0(dir_out_rawdata, "/0_species_local_names"))

  # Spreadsheets
  a <- googledrive::drive_ls(path = id_googledrive, type = "spreadsheet")
  for (i in 1:nrow(a)){
    googledrive::drive_download(file = googledrive::as_id(a$id[i]), 
                                type = "csv", 
                                overwrite = TRUE, 
                                path = paste0(dir_out_rawdata, "/", a$name[i]))
  }
  
  # Word documents
  a <- googledrive::drive_ls(path = id_googledrive, type = "document")
  for (i in 1:nrow(a)){
    googledrive::drive_download(file = googledrive::as_id(a$id[i]), 
                                type = "docx", 
                                overwrite = TRUE, 
                                path = paste0(dir_out_rawdata, "/", a$name[i]))
  }
  
}
# *** Load Oracle Data -------------------------------------------------------------

a<-list.files(path = here::here("data", "oracle"))
a <- a[!(grepl(pattern = "biomass_", x = a)) & 
         !(grepl(pattern = "cpue_", x = a)) & 
         !(grepl(pattern = "sizecomp_", x = a)) & 
         !(grepl(pattern = "_ADFG", x = a))]
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

df.ls<-list()

# for (ii in 1:length(SRVY1)) {

# a<-list.files(path = here::here("data", "surveydesign", SRVY1[ii], "biomass"), 
#               pattern = ".csv", 
#               full.names = TRUE)
# if (length(grep(pattern = "_plusnw", x = a, ignore.case = T)) > 0) {
#   a <- a[grep(pattern = "_plusnw", x = a)]
# }

a<-list.files(path = paste0(dir_data, "/oracle/"), 
              pattern = paste0("biomass_"), 
              full.names = TRUE)

for (i in 1:length(a)){
  b <- read_csv(file = a[i])
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  b$file <- a[i]
  b$survey <- toupper(strsplit(x = a[i], split = "_")[[1]][strsplit(x = a[i], split = "_")[[1]] %in% tolower(SRVY1)])
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}
# }

biomass <- SameColNames(df.ls)  %>%
  dplyr::filter(year <= maxyr &
                  stratum == 999) %>% 
  dplyr::rename(SRVY = survey) %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert"))

biomass_maxyr<-biomass %>% 
  dplyr::filter(year == maxyr)

biomass_compareyr<-biomass %>% 
  dplyr::filter(year == compareyr[1])


# *** Load Size Comp Design Based Estimates ----------------------------------------------

df.ls<-list()

# for (ii in 1:length(SRVY1)) {
  
  a<-list.files(path = paste0(dir_data, "/oracle/"), 
                pattern = paste0("sizecomp_"), 
                full.names = TRUE)
  
  for (i in 1:length(a)){
    b <- read_csv(file = a[i])
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1"){
      b$x1<-NULL
    }
    b$file <- a[i]
    b$survey <- toupper(strsplit(x = a[i], split = "_")[[1]][strsplit(x = a[i], split = "_")[[1]] %in% tolower(SRVY1)])
    df.ls[[i]]<-b
    names(df.ls)[i]<-a[i]
  }
# }

sizecomp <- SameColNames(df.ls)  %>%
  dplyr::filter(year <= maxyr & 
                  stratum == 999999) %>% 
  dplyr::rename(SRVY = survey) %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert")) %>%
  dplyr::mutate(length = length/10)

sizecomp_maxyr<-sizecomp %>% 
  dplyr::filter(year == maxyr)

sizecomp_compareyr<-sizecomp %>% 
  dplyr::filter(year == compareyr[1])

# *** Load CPUE Design Based Estimates ----------------------------------------------

df.ls<-list()

# for (ii in 1:length(SRVY1)) {
  
  # a<-list.files(path = here::here("data", "surveydesign", SRVY1[ii], "CPUE"), full.names = TRUE)
  # if (length(grep(pattern = "_plusnw", x = a, ignore.case = T)) > 0) {
  #   a <- a[grep(pattern = "_plusnw", x = a)]
  # }
  
  a<-list.files(path = paste0(dir_data, "/oracle/"), 
                pattern = paste0("cpue_"), 
                full.names = TRUE)
  
  for (i in 1:length(a)){
    b <- read_csv(file = a[i])
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1"){
      b$x1<-NULL
    }
    b$file <- a[i]
    temp <- strsplit(x = a[i], split = "_", fixed = TRUE)[[1]]
    b$survey <- toupper(temp[temp %in% tolower(SRVY1)])
    df.ls[[i]]<-b
    names(df.ls)[i]<-a[i]
  }
# }

cpue <- SameColNames(df.ls)  %>%
  dplyr::rename(SRVY = survey) %>% 
  dplyr::filter(year <= maxyr) %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert"))

cpue_maxyr <- cpue %>% 
  dplyr::filter(year == maxyr)

cpue_compareyr<- cpue %>% 
  dplyr::filter(year == compareyr[1])

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
  )) %>% 
  dplyr::mutate(area_km2 = area, 
                area_ha = area/100, 
                area_nmi2 = area/3.429904)

  return(stratum_info)

}

stratum_info <- temp(yr = maxyr)

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

cruises <- v_cruises0 %>% 
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
    year == compareyr[1] & 
      survey_definition_id %in% SRVY00)

nbsyr <- sort(cruises %>% 
                dplyr::filter(SRVY == "NBS") %>% 
                dplyr::select(year) %>% 
                unique() %>% 
                unlist())

# *** haul + maxyr ---------------------------------------------------------------------

temp <- haul0 %>%
  dplyr::left_join(x = ., 
                   y = cruises %>% 
                     dplyr::select(cruisejoin, survey_definition_id), 
                   by = "cruisejoin") %>%  
  dplyr::mutate(year = as.numeric(substr(x = start_time, 1,4))) %>% 
  dplyr::filter(year <= maxyr &
                  performance >= 0 &
                  !(is.null(stationid)) &
                  survey_definition_id %in% SRVY00) %>% 
  dplyr::select(-auditjoin) %>%  
  dplyr::mutate(SRVY = dplyr::case_when(
    survey_definition_id %in% 143 ~ "NBS",
    survey_definition_id %in% 98 ~ "EBS" )) #%>%
  # dplyr::filter(SRVY %in% SRVY1) # %>%
  # dplyr::mutate(start_date_haul = 
  #                 format(x = as.POSIXlt(x = start_time), format="%Y-%m-%d"))

haul <- temp %>% 
  dplyr::filter(abundance_haul == "Y" &
                  haul_type == 3)

haul_maxyr <- haul %>% 
  dplyr::filter(year == maxyr)

haul_compareyr <- haul %>% 
  dplyr::filter(year == compareyr[1])

# Crab retows?
crab_resample <- FALSE
if (sum(unique(temp$haul_type[temp$year == maxyr]) %in% 17) >0) {
  crab_resample <- TRUE
  haul_maxyr_crabretow <- haul0 %>%
    dplyr::filter(cruise %in% c(201701, 201702)  &
                    haul_type == 17)# crab retow == 17
}

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
    

# *** haul_cruises_vess_ + _maxyr + _compareyr -------------------------------------

temp <- function(cruises_, haul_){
  haul_cruises_vess_ <- 
    dplyr::left_join(x = cruises_ ,#%>% 
                       # dplyr::rename(start_date_cruise = start_date, 
                       #               end_date_cruise = end_date), 
                     y = haul_ %>% 
                       dplyr::select(cruisejoin, hauljoin, stationid, stratum, haul, 
                                     gear_depth, duration, distance_fished, net_width, net_height,
                                     #vessel, cruise,  #haul, 
                                     start_time) %>% 
                       dplyr::group_by(cruisejoin, hauljoin, stationid, stratum, haul, 
                                       gear_depth, duration, distance_fished, net_width, net_height) %>% 
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

haul_cruises_vess <- temp(cruises, haul) 

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
    dplyr::mutate(compareyr = compareyr[1]) %>%
    # c(compareyr_ebs, if(exists("compareyr_nbs")) {compareyr_nbs} )) %>% 
    dplyr::left_join(
      x = ., 
      y = data.frame(
        SRVY = SRVY1,
        compareyr_ref = c(ref_compareyr_ebs, if(exists("ref_compareyr_nbs")) {ref_compareyr_nbs} )), 
      by = "SRVY") %>%
    unique()
  
}

haul_cruises <- temp(haul_cruises_vess_ = haul_cruises_vess) 

haul_cruises_maxyr <- temp(haul_cruises_vess_ = haul_cruises_vess_maxyr) 

haul_cruises_compareyr <- temp(haul_cruises_vess_compareyr) 

# *** catch --------------------------------------------------------------------

## assigns groups based on species code
## 2 "other crab" groups because species codes 69010: 69200 are hermit crabs
catch <-  catch0


# *** catch_haul_cruises_maxyr + maxyr-1-----------------------------------------------

temp <- function(cruises_, haul_, catch){
  # This year's data
  catch_haul_cruises_<-
    dplyr::left_join(
      x = haul_ %>% 
        dplyr::select(cruisejoin, hauljoin, stationid, stratum, haul,
                      start_latitude, start_longitude, 
                      bottom_depth, gear_temperature, surface_temperature, 
                      "duration", "distance_fished" ,"net_width" ,"net_measured", "net_height"), 
      y = cruises_ %>% 
        dplyr::select(cruisejoin, survey_name, SRVY, year),  
      by = c("cruisejoin")) %>% 
    dplyr::left_join(
      x= ., 
    # dplyr::left_join(
    #   x = cruises_, 
    #   y = haul_, 
    #   by = c("cruisejoin")) %>% 
    # dplyr::left_join(
    #   x = ., 
      y = catch %>% 
        dplyr::select(cruisejoin, hauljoin,
                      species_code, weight,
                      number_fish, subsample_code), 
      by = c("hauljoin", "cruisejoin")) 
}

catch_haul_cruises <- temp(cruises, haul, catch)

catch_haul_cruises_maxyr <- temp(cruises_maxyr, haul_maxyr, catch)

catch_haul_cruises_compareyr <- temp(cruises_compareyr, haul_compareyr, catch)


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
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert")) %>% 
  dplyr::mutate(group = case_when(
    species_code == 69323 ~ "blue king crab",
    species_code == 21368 ~ "warty sculpin_Myoxocephalus@scorpius",
    species_code == 10261 ~ "northern rock sole",
    species_code == 10220 ~ "starry flounder",
    species_code == 21110 ~ "Pacific herring",
    species_code == 10140 ~ "Bering flounder",
    species_code == 71884 ~ "Neptune whelk_Neptunea@heros",
    species_code == 21371 ~ "plain sculpin",
    species_code == 81742 ~ "purple-orange sea star",
    species_code == 10285 ~ "Alaska plaice",
    species_code == 471 ~ "Alaska skate",
    species_code == 10210 ~ "yellowfin sole",
    species_code == 69322 ~ "red king crab",
    species_code == 21735 ~ "saffron cod",
    species_code == 10120 ~ "Pacific halibut",
    species_code == 68580 ~ "snow crab",
    species_code == 21725 ~ "Arctic cod",
    species_code == 21921 ~ "Atka mackerel",
    species_code == 68560 ~ "Tanner crab",
    species_code >= 66000 & species_code <= 67499 ~ "all shrimps_",
    species_code >= 21740 & species_code <= 21741 ~ "walleye pollock",
    species_code >= 21720 & species_code <= 21721 ~ "Pacific cod",
    species_code >= 83020 & species_code <= 83022 ~ "basket starfish_Gorgonocephalus@eucnemis",
    species_code >= 97000 & species_code <= 97120 ~ "brachiopods",
    species_code >= 83000 & species_code <= 83701 ~ "brittle stars and sand dollars_",
    species_code >= 82730 & species_code <= 82741 ~ "brittle stars and sand dollars_",
    species_code >= 95000 & species_code <= 95199 ~ "byrozoans_Bryozoa",
    species_code >= 70100 & species_code <= 70151 ~ "chitons",
    species_code >= 74000 & species_code <= 75799 ~ "clams, mussels, scallops_Bivalvia",
    species_code >= 41100 & species_code <= 41499 ~ "corals_Anthozoa",
    species_code >= 44000 & species_code <= 44499 ~ "corals_Anthozoa",
    species_code >= 44890 & species_code <= 44999 ~ "corals_Anthozoa",
    species_code >= 24100 & species_code <= 24395 ~ "eelpouts_Zoarcidae",
    species_code >= 99990 & species_code <= 99998 ~ "empty shells and debris",
    species_code >= 69010 & species_code <= 69249 ~ "hermit crabs_Paguridae",
    species_code >= 40060 & species_code <= 40999 ~ "jellyfishes_Scyphozoa",
    species_code >= 22170 & species_code <= 22199 ~ "lumpsuckers",
    species_code >= 78010 & species_code <= 78499 ~ "octopuses",
    species_code >= 68000 & species_code <= 69009 ~ "other crabs_",
    species_code >= 69201 & species_code <= 69549 ~ "other crabs_",
    species_code >= 10000 & species_code <= 10399 ~ "other flatfishes_Pleuronectidae",
    species_code >= 21300 & species_code <= 21499 ~ "other sculpins_Cottidae",
    species_code >= 80000 & species_code <= 82499 ~ "other sea stars_Asteroidea",
    species_code >= 71000 & species_code <= 73999 ~ "other snails_Gastropoda",
    species_code >= 20000 & species_code <= 20099 ~ "poachers_Agonidae",
    species_code >= 23800 & species_code <= 23870 ~ "pricklebacks_Stichaeidae",
    species_code >= 23200 & species_code <= 23310 ~ "salmonids",
    species_code >= 82730 & species_code <= 82740 ~ "sand dollar",
    species_code >= 43000 & species_code <= 43999 ~ "sea anemones_Actiniaria",
    species_code >= 85000 & species_code <= 85500 ~ "sea cucumbers_Holothuroidea",
    species_code >= 82500 & species_code <= 82691 ~ "sea urchins_Strongylocentrotus@spp.",
    species_code >= 23000 & species_code <= 23071 ~ "smelts_Osmeridae",
    species_code >= 22200 & species_code <= 22299 ~ "snailfishes_Liparidae",
    species_code >= 91000 & species_code <= 91999 ~ "sponges",
    species_code >= 98000 & species_code <= 99909 ~ "tunicates_Urochordata",
    species_code >= 50000 & species_code <= 59999 ~ "misc worms_",
    species_code >= 92000 & species_code <= 97499 ~ "misc worms_",
    species_code >= 20300 & species_code <= 20399 ~ "wolffishes",
    species_code >= 21900 & species_code <= 21935 ~ "greenlings",
    species_code >= 21752 & species_code <= 21753 ~ "sticklebacks",
    species_code >= 40010 & species_code <= 40050 ~ "hydroids",
    species_code >= 20200 & species_code <= 20204 ~ "sand lances",
    species_code >= 78502 & species_code <= 79001 ~ "squids",
    species_code >= 79020 & species_code <= 79513 ~ "squids",
    species_code >= 82750 & species_code <= 82775 ~ "sea lilies",
    species_code == 474 | 401 | 402 | 403 | 421 | 436 ~ "skate egg cases",
    species_code == 1 ~ "fish eggs",
    TRUE ~ "other"))

spp_info$used_in_counts <- 
                  ifelse(spp_info$species_code %in% #10:99988, 
                           find_codes(x = spp_info,  
                                      str_not = c(" shells", "empty", "unsorted", "shab"
                                                  #, " egg", "unid.", "compound"
                                      ),
                                      col_str_not = "common_name",
                                      col_out = "species_code"), 
                         TRUE, FALSE)  # remove " shells", "empty", "unsorted", "shab". May also consider removing " egg", "unid.", "compound"

temp <- spp_info[(!grepl(pattern = "@", x = spp_info$group, fixed = T) &
                    !grepl(pattern = "_", x = spp_info$group, fixed = T)),]
for (i in 1:length(unique(temp$group))) {
  temp1 <- unique(temp$species_name[temp$group == unique(temp$group)[i]])
  temp1 <- unique(gsub(pattern = " (juvenile)", replacement = "", x = temp1, fixed = TRUE))
  if (length(temp1) == 1) {
    spp_info$group[spp_info$group == unique(temp$group)[i]] <- 
      paste0(unique(temp$group)[i], "_",
             gsub(pattern = " ", replacement = "@", x = temp1, fixed = TRUE))
  } 
}

# species specifically caught in the survey year
spp_info_maxyr <- spp_info %>% 
  dplyr::filter(species_code %in% unique(catch_haul_cruises_maxyr$species_code))

# *** report_spp ---------------------------------------------------------------

report_spp <- readr::read_csv(file = paste0(dir_out_rawdata, "/0_species_local_names.csv"), 
                              skip = 1) %>% 
  dplyr::select(!(dplyr::starts_with(ifelse(grepl(pattern = "Highlights", x = report_title), "datar_", "community_"))))

names(report_spp)[
  grepl(pattern = ifelse(grepl(pattern = "Highlights", 
                               x = report_title), "community_", "datar_"), 
        x = names(report_spp))] <- 
  gsub(pattern = ifelse(grepl(pattern = "Highlights", 
                              x = report_title), "community_", "datar_"), 
       replacement = "", 
       x = names(report_spp)[
         grepl(pattern = ifelse(grepl(pattern = "Highlights", 
                                      x = report_title), "community_", "datar_"), 
               x = names(report_spp))])

report_spp <-  
  dplyr::left_join(x = report_spp %>% 
                     dplyr::filter(!is.na(order)) %>% 
                     dplyr::arrange((order)) %>% 
                     dplyr::mutate(common_name = tolower(common_name))  %>% 
                     dplyr::mutate(common_name1 = common_name) %>%
                     dplyr::mutate(common_name = tolower(common_name)), 
                   y = spp_info %>% 
                     # dplyr::mutate(common_name1 = common_name) %>%
                     dplyr::mutate(common_name = tolower(common_name)) %>%
                     dplyr::select(-species_code) %>% 
                     unique(), 
                   by = "common_name") %>% 
  dplyr::select(-common_name) %>% 
  dplyr::rename(common_name = common_name1)

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
length_type <- length_types0
length_type$sentancefrag <- c("fork lengths",
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

# ## weighted mean pt 1
# temps_wt_avg_strat <- stratum_info %>% #temp_strat(maxyr) %>%
#     dplyr::filter(SRVY %in% SRVY1) %>%
#     dplyr::select(stratum, area, SRVY) %>%
#     dplyr::mutate(weight_all = area/sum(area)) %>% 
#     dplyr::group_by(SRVY) %>% 
#     dplyr::mutate(weight_SRVY = area/sum(area)) %>% 
#     dplyr::left_join(x = haul %>% 
#                        # dplyr::filter(year == maxyr) %>%
#                        dplyr::select(stratum, year, #stationid, 
#                                      surface_temperature, 
#                                      gear_temperature, bottom_depth), 
#                      y = ., 
#                      by = "stratum") %>%
#     dplyr::ungroup() %>%
#     dplyr::group_by(year, stratum, SRVY) %>%
#     dplyr::summarise(bt_wt_stratum = mean(gear_temperature * weight_SRVY, na.rm = TRUE), 
#                      st_wt_stratum = mean(surface_temperature * weight_SRVY, na.rm = TRUE)) %>%
#     dplyr::ungroup()  
# 
# temps_wt_avg_yr <- temps_wt_avg_strat %>%
#     dplyr::group_by(year, SRVY) %>%
#     dplyr::summarise(bt_wt = sum(bt_wt_stratum, na.rm = TRUE), 
#                      st_wt = sum(st_wt_stratum, na.rm = TRUE)) %>%
#     dplyr::filter(!is.na(SRVY)) %>%
#     dplyr::filter(!is.nan(bt_wt) | !is.nan(st_wt))
# 
#   # temps_wt_avg_yr <- rbind.data.frame(temps_wt_avg_yr, temp)
# 
# # }
# 
# # long term mean pt 1
# temps_wt_avg_yr_longterm <- temps_wt_avg_yr %>% 
#   dplyr::filter(year != maxyr) %>% 
#   dplyr::ungroup() %>%
#   dplyr::group_by(SRVY) %>%
#   dplyr::arrange(desc(year)) %>%
#   dplyr::summarise(bt_wt_mean = mean(bt_wt, na.rm = TRUE), 
#                    st_wt_mean = mean(st_wt, na.rm = TRUE))
# 
# 
# 
# 
# ## weighted mean pt 2
# temps_wt_avg_yr <- temps_wt_avg_yr %>% 
#   dplyr::ungroup() %>%
#   dplyr::left_join(x = ., 
#                    y = temps_wt_avg_yr_longterm, 
#                    by  = "SRVY") %>% 
#   dplyr::group_by(SRVY) %>% 
#   dplyr::arrange(SRVY, year) %>%
#   dplyr::mutate(bt_wt_above_mean = bt_wt>mean(bt_wt_mean, na.rm = TRUE)) %>% 
#   dplyr::mutate(st_wt_above_mean = st_wt>mean(st_wt_mean, na.rm = TRUE)) %>% 
#   dplyr::mutate(case = dplyr::case_when(
#     ((st_wt_above_mean + bt_wt_above_mean)==2) ~ "both warmer",
#     ((st_wt_above_mean + bt_wt_above_mean)==0) ~ "both colder",
#     (st_wt_above_mean == TRUE & bt_wt_above_mean == FALSE) ~ "st warmer, bt colder",
#     (st_wt_above_mean == FALSE & bt_wt_above_mean == TRUE) ~ "bt warmer, st colder") ) 

temps_avg_yr <- coldpool:::cold_pool_index %>% 
  dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::rename(bt = MEAN_GEAR_TEMPERATURE, 
                st = MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::filter(YEAR <= maxyr) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(bt_mean = mean(bt, na.rm = TRUE)) %>% 
  dplyr::mutate(st_mean = mean(st, na.rm = TRUE)) %>% 
  dplyr::mutate(SRVY = "EBS") %>%
    dplyr::arrange(SRVY, year) %>%
    dplyr::mutate(bt_above_mean = bt>bt_mean) %>%
    dplyr::mutate(st_above_mean = st>st_mean) %>%
    dplyr::mutate(case = dplyr::case_when(
      ((st_above_mean + bt_above_mean)==2) ~ "both warmer",
      ((st_above_mean + bt_above_mean)==0) ~ "both colder",
      (st_above_mean == TRUE & bt_above_mean == FALSE) ~ "st warmer, bt colder",
      (st_above_mean == FALSE & bt_above_mean == TRUE) ~ "bt warmer, st colder") )

# calculate the nth year of case
nthyr <- c()
for (ii in 1:length(unique(temps_avg_yr$SRVY))){
  temp <- temps_avg_yr %>% 
    dplyr::filter(SRVY == unique(temps_avg_yr$SRVY)[ii])
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
temps_avg_yr$nthyr <- nthyr
temps_avg_yr <- temps_avg_yr %>% 
  dplyr::arrange(desc(year))

temps_avg_yr_maxyr <- temps_avg_yr %>%  # temps_avg_yr_longterm
  dplyr::filter(year == maxyr)

# which years should we look at?
temps_avg_yr_abovebelow <- 
  cbind.data.frame(
  "above" = temps_avg_yr %>% 
      dplyr::filter(SRVY == "EBS" & 
                      bt_above_mean == TRUE) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(-year) %>% 
      dplyr::select(year) %>% 
      head(8) %>%
      unlist(), 
  "below" = temps_avg_yr %>% 
  dplyr::filter(SRVY == "EBS" & 
                  bt_above_mean == FALSE) %>% 
  dplyr::ungroup() %>%
  dplyr::arrange(-year) %>% 
  dplyr::select(year) %>% 
  head(8) %>%
  unlist())

temp <- coldpool:::ebs_bottom_temperature
temp <- projectRaster(temp, crs = crs(reg_dat$akland))
temp <- as(temp, "SpatialPixelsDataFrame")
temp <- as.data.frame(temp)
temp1 <- gsub(pattern = "[A-Za-z]+", 
              replacement = "", 
              x = names(temp[!(names(temp) %in% c("x", "y"))]))
temp1 <- gsub(pattern = "_", replacement = "", x = temp1)
colnames(temp) <- c(temp1, "latitude", "longitude")
cold_pool_area <- temp %>% 
  tidyr::pivot_longer(values_to = "value", 
                      names_to = "year", 
                      cols = temp1) %>% 
  dplyr::mutate(bin = cut(x = value, 
                          breaks = c(-Inf, seq(from = -1, to = 2, by = 1)))) %>% 
  dplyr::group_by(year, bin) %>%
  dplyr::summarise(count(bin)) %>%
  dplyr::filter(year <= maxyr) %>%
  dplyr::mutate(perc = (freq/length(temp$`1982`)) * 100)  %>% # length(temp$`1982`) = 21299 is the number of cells and shouldnt change?
  dplyr::mutate(label = dplyr::case_when(
    bin == "(-Inf,-1]" ~ "> -1\u00B0C",
    bin == "(-1,0]" ~ "-1 to 0\u00B0C",
    bin == "(0,1]" ~ "0 to 1\u00B0C",
    bin == "(1,2]" ~ "1 to 2\u00B0C")) %>% 
  dplyr::filter(!is.na(bin)) %>%
  dplyr::mutate(label = factor(label, 
                               levels=c("1 to 2\u00B0C", "0 to 1\u00B0C", "-1 to 0\u00B0C", "> -1\u00B0C"), 
                               ordered = TRUE))

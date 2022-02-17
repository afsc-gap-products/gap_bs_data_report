#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Wrangle data
#' start date: 2021-03-03
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---

# Report types ---------------------------------------------------

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
report_types <- list(
  "EBS" = list(
    sectname = "EBS-BTS-Report", 
    SURVEY = "eastern Bering Sea", 
    map.area = "bs.south", 
    SRVY1 = "EBS", 
    SRVY0 = "BS", # in Oracle
    SRVY00 = 98, # EBS
    station_id = akgfmaps::get_survey_stations(
      select.region = "bs.south"),
    extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 63), 
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.south", 
      set.crs = "auto", 
      return.survey.grid = TRUE)#,
    # report_species = report_species_NEBS
  ), 
  "NBS" = list(
    sectname = "NBS-BTS-Report", 
    SURVEY = "northern Bering Sea", 
    map.area = "bs.north", 
    SRVY1 = "NBS", 
    SRVY0 = "BS", # in Oracle
    SRVY00 = 143,
    station_id = akgfmaps::get_survey_stations(
      select.region = "bs.north"),
    extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 68),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.north", 
      set.crs = "auto", 
      return.survey.grid = TRUE)#,
    # report_species = report_species_NEBS
  ), 
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
    extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, ymx = 68),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.all", 
      set.crs = "auto", 
      return.survey.grid = TRUE)#,
    # report_species = report_species_NEBS
  )
)


# TOLEDO
# report_types$EBS$reg_dat$survey.strata$Stratum[
#   report_types$EBS$reg_dat$survey.strata$Stratum == 30]<-31
# report_types$NEBS$reg_dat$survey.strata$Stratum[
#   report_types$NEBS$reg_dat$survey.strata$Stratum == 30]<-31

a <- report_types[names(report_types) == SRVY][[1]]
for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }


# Load data --------------------------------------------------------------------

# *** Load Documents from Google Drive -----------------------------------------

id_googledrive <- googledrive::as_id(dir_googledrive)

if (googledrive_dl) {
  
  # Species Covered
  # https://docs.google.com/spreadsheets/d/10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M/edit?usp=sharing
  googledrive::drive_download(file = googledrive::as_id("10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M"),
                              type = "csv",
                              overwrite = TRUE,
                              path = paste0(dir_out_rawdata, "/0_species_local_names"))
  
  # Spreadsheets
  # https://drive.google.com/drive/folders/1Vbe_mH5tlnE6eheuiSVAFEnsTJvdQGD_?usp=sharing
  a <- googledrive::drive_ls(path = googledrive::as_id("1Vbe_mH5tlnE6eheuiSVAFEnsTJvdQGD_"), type = "spreadsheet")
  for (i in 1:nrow(a)){
    googledrive::drive_download(file = googledrive::as_id(a$id[i]), 
                                type = "xlsx", 
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
         !(grepl(pattern = "old", x = a)) & 
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
  temp<-strsplit(x = a[i], split = "/", fixed = TRUE)[[1]][length(strsplit(x = a[i], split = "/", fixed = TRUE)[[1]])]
  b$survey <- toupper(strsplit(x = temp, split = "_")[[1]][strsplit(x = temp, split = "_")[[1]] %in% c("nbs", "ebs")])
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}
# }

biomass <- SameColNames(df.ls)  %>%
  dplyr::filter(year <= maxyr #& stratum == 999
                ) %>% 
  dplyr::rename(SRVY = survey) %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert")) %>% 
  dplyr::filter(!is.na(species_code)) %>% 
# modify for spp
  dplyr::filter(!(year < 1996 & common_name == "northern rock sole" )) %>% # 10263 NRS
  dplyr::filter(!(year < 2000 & common_name == "Bering skate" ))  #%>% 
  # dplyr::group_by(species_code) %>% 
  # dplyr::mutate(meanpop_spp = mean(pop, na.rm = TRUE), 
  #               meanbio_spp = mean(pop, na.rm = TRUE)) %>% 
  # dplyr::ungroup() %>% 
  # dplyr::group_by(species_code, year) %>% 
  # dplyr::mutate(varpop = sqrt(sum(population - meanpop_spp)/(catcount-1)), # from 1 SD to Standard Error
  #               varbio = sqrt(sum(biomass - meanbio_spp)/(catcount-1)))  %>% # from 1 SD to Standard Error
  # dplyr::ungroup() %>% 
  # dplyr::mutate(upperp = population+varpop, 
  #               lowerp = population-varpop, 
  #               upperb = biomass+varbio, 
  #               lowerb = biomass-varbio) 

# dplyr::mutate(varpop =  sqrt(varpop)/sqrt(catcount), # from 1 SD to Standard Error
#               upperp = population+varpop,
#               lowerp = population-varpop,
#               varbio =  sqrt(varbio)/sqrt(catcount), # from 1 SD to Standard Error
#               upperb = biomass+varbio,
#               lowerb = biomass-varbio)


biomass_strat <- biomass

biomass <- biomass %>%
  dplyr::filter(stratum == 999)

biomass_maxyr<-biomass %>%
  dplyr::filter(year == maxyr)

biomass_compareyr<-biomass %>%
  dplyr::filter(year == compareyr[1])


## *** Load CPUE Design Based Estimates ----------------------------------------------

df.ls<-list()

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
  # temp <- strsplit(x = a[i], split = "_", fixed = TRUE)[[1]]
  temp<-strsplit(x = a[i], split = "/", fixed = TRUE)[[1]][length(strsplit(x = a[i], split = "/", fixed = TRUE)[[1]])]
  b$survey <- toupper(strsplit(x = temp, split = "_")[[1]][strsplit(x = temp, split = "_")[[1]] %in% c("nbs", "ebs")])
  # b$survey <- toupper(temp[temp %in% tolower(SRVY1)])
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

cpue$common_name[cpue$species_name == "Neptunea heros"] <- "northern neptune whelk"

# Wrangle Data -----------------------------------------------------------------

# *** report_spp and spp_info ---------------------------------------------------------------

report_spp <- readr::read_csv(file = paste0(dir_out_rawdata, "/0_species_local_names.csv"), 
                              skip = 1) %>% 
  dplyr::select(!(dplyr::starts_with(ifelse(grepl(pattern = "Highlights", 
                                                  x = report_title), "datar_", "community_")))) # %>%
# dplyr::filter(!grepl(pattern = "other ", x = group) &
#!grepl(pattern = "all ", x = group) &
# !grepl(pattern = "egg ", x = group))

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

spp_info <- 
  dplyr::left_join(x = species0, 
                   y = species_classification0, 
                   by = "species_code") %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert")) %>%
  dplyr::mutate(used_in_counts = dplyr::if_else(species_code %in% #10:99988, 
                                                  find_codes(x = .,  
                                                             str_not = c(" shells", "empty", "unsorted", "shab"
                                                                         #, " egg", "unid.", "compound"
                                                             ),
                                                             col_str_not = "common_name",
                                                             col_out = "species_code"), 
                                                TRUE, FALSE))   # remove " shells", "empty", "unsorted", "shab". May also consider removing " egg", "unid.", "compound"


# report_spp <- add_report_spp(spp_info = spp_info, 
#                              spp_info_codes = "species_code", 
#                              report_spp = report_spp, 
#                              report_spp_col = "table_bio_spp", 
#                              report_spp_codes = "species_code")
# 
# 
# spp_info <- dplyr::left_join(
#   x = spp_info %>% 
#     dplyr::select(-species_name), 
#   y = report_spp %>% 
#     dplyr::select(order, file_name, print_name, common_name1, group, group_sci, 
#                   species_code, species_name, species_name1), 
#   by = "species_code")

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


# *** haul + maxyr ---------------------------------------------------------------------

temp <- dplyr::left_join(
  x = haul0, 
  y = cruises %>% 
    dplyr::select(cruisejoin, survey_definition_id), 
  by = "cruisejoin") %>%  
  dplyr::mutate(year = as.numeric(format(as.Date(haul0$start_time, 
                                                 format="%m/%d/%Y"),"%Y"))) %>%
  dplyr::filter(year <= maxyr &
                  # abundance_haul == "Y", 
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

# *** Other var (survey additions, *yrs, etc. ----------------------------------

# Crab retows?
crab_resample <- FALSE
if (sum(unique(temp$haul_type[temp$year == maxyr]) %in% 17) >0) {
  crab_resample <- TRUE
  haul_maxyr_crabretow <- haul0 %>%
    dplyr::filter(grepl(pattern = maxyr, x = cruise)) %>% 
    dplyr::filter(haul_type == 17) # crab retow == 17
}

# 15/30
tow1530 <- FALSE
if (sum(unique(temp$haul_type[temp$year == maxyr]) %in% 20) >0) {
  tow1530 <- TRUE
  haul_maxyr_tow1530 <- haul0 %>%
    dplyr::filter(grepl(pattern = maxyr, x = cruise)) %>% 
    dplyr::filter(haul_type == 20) 
}

if (SRVY == "NEBS") {
  nbsyr <- sort(cruises %>% 
                  dplyr::filter(SRVY == "NBS") %>% 
                  dplyr::select(year) %>% 
                  unique() %>% 
                  unlist())
} else {
  nbsyr <- sort(unique(haul$year), decreasing = TRUE)[1:4]
}

lastyr <- max(haul$year[haul$year != maxyr])

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
                  area_ha = area/divkm2forha, 
                  area_nmi2 = area/divkm2fornmi2)
  
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
# ORDER BY STRATUM


# *** station_info ------------------------------------------------------------------

# TOLEDO - has AA-10 not AA-21
# station_info <- stations0 %>%
#   dplyr::filter(stratum %in% reg_dat$survey.strata$Stratum) %>% 
#   dplyr::left_join(x = ., 
#                    y = stratum_info %>% 
#                      dplyr::select(stratum, SRVY), 
#                    by = "stratum") 

station_info <- haul %>% #  
  # dplyr::filter(year == maxyr) %>% 
  dplyr::select(stationid, stratum, start_latitude, start_longitude) %>% 
  dplyr::group_by(stationid, stratum) %>% 
  dplyr::summarise(start_latitude = mean(start_latitude, na.rm = TRUE), 
                   start_longitude = mean(start_longitude, na.rm = TRUE)) %>% 
  dplyr::left_join(x = ., 
                   y = stratum_info %>% 
                     dplyr::select(stratum, SRVY), 
                   by = "stratum") %>% 
  dplyr::mutate(in_maxyr = (stationid %in% haul_maxyr$stationid))

station_info$reg <- NA
station_info$reg[station_info$stationid %in% 
                   c("CC-04", "CC-05", "CC-06", "CC-07", "CC-08", "CC-09", 
                     "CC-10", "BB-04", "BB-05", "BB-06", "BB-07", "BB-08", 
                     "BB-09", "BB-10", "AA-04", "AA-05", "AA-06", "AA-07", 
                     "AA-08", "AA-10", "ZZ-04", "ZZ-05", "Y-04")] <- "Norton Sound"

# *** stratum_info (survey area) reprise -------------------------------------------

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
                       dplyr::mutate(start_time = (format(as.Date(start_time, 
                                                                                                   format="%m/%d/%Y"),"%m/%d/%Y"))) %>%
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
                                   digits = 1)) %>% 
    dplyr::ungroup()
}

haul_cruises_vess <- temp(cruises, haul) 

haul_cruises_vess_maxyr <- temp(cruises_maxyr, haul_maxyr) 

haul_cruises_vess_compareyr <- temp(cruises_compareyr, haul_compareyr) 

# *** vessel_info -------------------------------------------------------

vessel_info <-  haul_cruises_vess_maxyr %>% 
  dplyr::select("vessel_name", "vessel_ital", "vessel", "tonnage",
                "length_m", "length_ft", "vess_shape") %>% 
  unique()

# *** haul_cruises + _maxyr + _compareyr ------------------------------------------

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
        compareyr_ref = ref_compareyr), 
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
        dplyr::select(cruisejoin, hauljoin, stationid, stratum, haul, start_time, 
                      start_latitude, start_longitude, 
                      end_latitude, end_longitude, gear_depth, 
                      bottom_depth, gear_temperature, surface_temperature,
                      performance, 
                      "duration", "distance_fished" ,"net_width" ,"net_measured", "net_height"), 
      y = cruises_ %>% 
        dplyr::select(cruisejoin, survey_name, SRVY, year, cruise),  
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

catch_haul_cruises_maxyr <- temp(cruises_ = cruises_maxyr, 
                                 haul_ = haul_maxyr, 
                                 catch = catch)

catch_haul_cruises_compareyr <- temp(cruises_compareyr, haul_compareyr, catch)

spp_info_maxyr <- spp_info %>% 
  dplyr::filter(species_code %in% 
                  unique(catch_haul_cruises_maxyr$species_code))


# *** length ---------------------------------------------------------------

length_data <- v_extract_final_lengths0 %>%
  dplyr::mutate(sex_code = sex, 
                sex = dplyr::case_when(
                  sex_code == 1 ~ "Males", 
                  sex_code == 2 ~ "Females", 
                  sex_code == 3 ~ "Unsexed"), 
                taxon = dplyr::case_when(
                  species_code <= 31550 ~ "fish", 
                  species_code >= 40001 ~ "invert")) %>%
  dplyr::left_join(
    x = ., 
    y = haul %>%   # should exclude special project and bad tows
      dplyr::select(haul, cruise, vessel, stationid, abundance_haul, haul_type, SRVY, year) %>%
      dplyr::distinct(),
    by = c("haul", "cruise", "vessel")) %>% 
  dplyr::filter(
    abundance_haul == "Y" &
      haul_type == 3 &
      # cruise == 202102 &
      region == SRVY0) %>% # Remove special project tows
  dplyr::select(-region, -abundance_haul, -haul_type, -sample_type, -cruise) %>%
  dplyr::group_by(sex, sex_code, length, year,  species_code, SRVY, taxon, length_type) %>%
  dplyr::summarise(frequency = sum(frequency, na.rm = TRUE)) # Total for each species
  # dplyr::select(species_code, total_lengths) 


# length_data <- 
#   # dplyr::left_join(
#   #   x = haul %>% 
#   #     dplyr::select(cruisejoin, hauljoin, stationid, stratum, haul, cruise), 
#   #   y = cruises %>% 
#   #     dplyr::select(cruisejoin, survey_name, SRVY),  
#   #   by = c("cruisejoin")) %>% 
#   dplyr::left_join(
#     x= haul0 %>% # should exclude special project and bad tows
#       dplyr::select(haul, cruise)  %>% 
#       dplyr::filter(region == SRVY0) %>%
#       dplyr::mutate(SRVY = dplyr::case_when(
#         survey_definition_id %in% 143 ~ "NBS",
#         survey_definition_id %in% 98 ~ "EBS" )) %>% 
#       dplyr::mutate(year = substr(x = cruise, start = 1, stop = 4)) %>% 
#     # x= haul %>% # should exclude special project and bad tows
#     #   dplyr::select(haul, cruise, year, SRVY) %>% 
#       dplyr::distinct(), 
#     y = v_extract_final_lengths0 %>% 
#       dplyr::filter(region == SRVY0) %>% 
#       dplyr::select(-vessel),
#     by = c("haul", "cruise")) %>% 
#   # dplyr::select(-vessel, -survey_name, -cruisejoin, -hauljoin) %>%
#   dplyr::mutate(sex_code = sex, 
#                 sex = dplyr::case_when(
#                   sex_code == 1 ~ "Males", 
#                   sex_code == 2 ~ "Females", 
#                   sex_code == 3 ~ "Unsexed"), 
#                 taxon = dplyr::case_when(
#                   species_code <= 31550 ~ "fish", 
#                   species_code >= 40001 ~ "invert"), )


# load crab data
df.ls<-list()
a<-list.files(path = paste0(dir_data, "/crab/no_measured/"), 
              # pattern = paste0("sizecomp_"), 
              full.names = TRUE)

for (i in 1:length(a)){
  b <- read_csv(file = a[i])
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  b$file <- a[i]
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}

length_crab <- SameColNames(df.ls) %>% 
  # dplyr::left_join(
  #   x = SameColNames(df.ls),
  # y = station_info %>%
  #   dplyr::select(stationid, stratum, SRVY) %>%
  #   unique(),
  # by = c("station" = "stationid")) %>%
  dplyr::mutate(SRVY = "NBS", 
                year = as.numeric(substr(cruise, start = 1, stop = 4))) %>% 
  dplyr::select(length, width, sex, #, file, unsexed, males, females, hauljoin, stratum, gis_station, #_mature, females_immature,
                year, species_code, SRVY, 
                clutch_size) %>%
  # dplyr::rename(length = width) %>%
  dplyr::mutate(sex_code = sex, 
                sex = dplyr::case_when(
                  sex == 1 ~ "Males",
                  sex == 0 ~ "Unsexed",
                  (clutch_size == 0 & sex == 2) ~ "Females (Immature)", 
                  (clutch_size >= 1 & sex == 2) ~ "Females (Mature)"), 
                length = dplyr::case_when(
                  species_code %in% c(68580) ~ width,  # "snow crab"
                  TRUE ~ length), 
                frequency = 1) %>%
  dplyr::select(-width) %>% 
  dplyr::filter(!is.na(length)) %>% 
  dplyr::group_by(sex, sex_code, length, year, species_code, SRVY) %>%
  dplyr::summarise(frequency = n()) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish", 
      species_code >= 40001 ~ "invert"), 
    length_type = dplyr::case_when(
      species_code %in% c(69322, 69323) ~ 7,
      species_code %in% 68580 ~ 8)) 

# Combine

length_data <- SameColNames(list(
  "gf" = length_data, 
  "crab" = length_crab))  %>% 
  dplyr::rename(SRVY = srvy) %>% 
  dplyr::left_join(x = .,
                   y = species0 %>% 
                     dplyr::select(species_code, common_name),
                   by = "species_code") %>% 
  ungroup()

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
                              "carapace lengths",
                              "carapace widths",
                              "head lengths",
                              "snout to anal fin origin lengths",
                              "mantle lengths",
                              "posterior of orbital to end of telson lengths",
                              "wingtip to wingtip lengths",
                              "outer tip of rostrum to end of telson lengths",
                              "modal lengths",
                              "frequency of lengths estimated using size composition proportions from adjacent hauls with similar catch composition")

length_data <-  dplyr::left_join(x = length_data, 
                                 y = length_type, 
                                 by = c("length_type" = "length_type_id"))

length_maxyr <- length_data %>% 
  dplyr::filter(year == maxyr) 

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

# averge temp without maxyr
temp <- coldpool:::cold_pool_index %>% 
  dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::rename(bt = MEAN_GEAR_TEMPERATURE, 
                st = MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::filter(YEAR < maxyr) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(bt_mean = mean(bt, na.rm = TRUE)) %>% 
  dplyr::mutate(st_mean = mean(st, na.rm = TRUE))

temps_avg_yr <- coldpool:::cold_pool_index %>% 
  dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::rename(bt = MEAN_GEAR_TEMPERATURE, 
                st = MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::filter(YEAR <= maxyr) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(bt_mean = temp$bt[1]) %>% 
  dplyr::mutate(st_mean = temp$st[1]) %>% 
  dplyr::mutate(bt_mean_maxyr = mean(bt, na.rm = TRUE)) %>% 
  dplyr::mutate(st_mean_maxyr = mean(st, na.rm = TRUE)) %>% 
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
# temps_avg_yr_abovebelow <- 
#   cbind.data.frame(
#     "above" = temps_avg_yr %>% 
#       dplyr::filter(SRVY == "EBS" & 
#                       bt_above_mean == TRUE) %>% 
#       dplyr::ungroup() %>%
#       dplyr::arrange(-year) %>% 
#       dplyr::select(year) %>% 
#       head(8) %>%
#       unlist(), 
#     "below" = temps_avg_yr %>% 
#       dplyr::filter(SRVY == "EBS" & 
#                       bt_above_mean == FALSE) %>% 
#       dplyr::ungroup() %>%
#       dplyr::arrange(-year) %>% 
#       dplyr::select(year) %>% 
#       head(8) %>%
#       unlist())


temp1 <- temps_avg_yr %>% 
      dplyr::filter(SRVY == "EBS" & 
                      bt_above_mean == TRUE &
                      year >= maxyr-16) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(-year) %>% 
      dplyr::select(year) %>% 
      unlist() %>% 
  data.frame() 
names(temp1) <- c("above")

temp2 <- temps_avg_yr %>% 
      dplyr::filter(SRVY == "EBS" & 
                      bt_above_mean == FALSE &
                      year >= maxyr-16) %>% 
      dplyr::ungroup() %>%
      dplyr::arrange(-year) %>% 
      dplyr::select(year) %>% 
      unlist() %>% 
  data.frame() 
names(temp2) <- c("below")


# make the same length so they can be bound together
if (nrow(temp1)>nrow(temp2)) {
  temp2 <- temp2 %>% 
    dplyr::bind_rows(., 
                     data.frame(below = rep_len(NA, (nrow(temp1)-nrow(temp2)))))
} else {
  temp1 <- temp1 %>% 
    dplyr::bind_rows(., 
                     data.frame(above = rep_len(NA, (nrow(temp2)-nrow(temp1)))))
}

temps_avg_yr_abovebelow <- dplyr::bind_cols(temp1, temp2)

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

# *** Calculate Biomass and CPUE -----------------------------------------------------------

report_spp1 <- add_report_spp(spp_info = spp_info, 
                              spp_info_codes = "species_code", 
                              report_spp = report_spp, 
                              report_spp_col = "table_bio_spp", 
                              report_spp_codes = "species_code")


spp_info1 <- dplyr::left_join(
  x = spp_info %>% 
    dplyr::select(-species_name), 
  y = report_spp1 %>% 
    dplyr::select(order, file_name, print_name, common_name1, group, group_sci, 
                  species_code, species_name, species_name1), 
  by = "species_code")


cpue_biomass_station <- tidyr::crossing(
  haul_cruises_vess %>%
    dplyr::filter(SRVY %in% c("NBS", "EBS")),
  dplyr::distinct(
    catch_haul_cruises %>%
      dplyr::filter(SRVY %in% c("NBS", "EBS"))  %>%
      dplyr::left_join(
        x = .,
        y = spp_info1 %>% 
          # dplyr::mutate(group = species_name) %>% 
          dplyr::select(species_code, group, species_name, species_name1, print_name, taxon),
        by = "species_code"),
    species_code, species_name, species_name1, group, print_name, taxon)) %>%
  dplyr::left_join(
    x = .,
    y = catch_haul_cruises %>%
      dplyr::select("cruisejoin", "hauljoin", "cruisejoin", "species_code",
                    "weight", "number_fish", "SRVY"),
    by = c("species_code", "hauljoin", "cruisejoin", "SRVY")) %>%
  #### a check for species with weights greater then 0
  ## sum catch weight (by groups) by station and join to haul table (again) to add on relevent haul data
  dplyr::group_by(year, stationid, SRVY, species_name, species_name1, print_name, taxon, #species_code,
                  group, hauljoin, stratum, distance_fished, net_width) %>%
  dplyr::summarise(wt_kg_summed_by_station = sum(weight, na.rm = TRUE), # overwrite NAs in assign_group_zeros where data exists
                   num_summed_by_station = sum(number_fish, na.rm = TRUE)) %>% # overwrite NAs in
  
  ## checks catch_and_zeros table for species that are not in groups, if species are not grouped
  #### add group to assign_groups table
  ## calculates CPUE for each species group by station
  mutate(effort = distance_fished * net_width/10) %>%
  mutate(cpue_kgha = wt_kg_summed_by_station/effort) %>%
  mutate(cpue_noha = ifelse(wt_kg_summed_by_station > 0 & num_summed_by_station == 0, NA,
                            (cpue_no = num_summed_by_station/effort))) %>%
  #### this is to check CPUEs by group, station and year against the SQL code
  ## add area to CPUE table
  dplyr::ungroup() %>% 
  dplyr::left_join(x = .,
                   y = stratum_info %>%
                     dplyr::select(stratum, area),
                   by = 'stratum')  %>% 
  dplyr::left_join(x = ., 
                   y = station_info, 
                   by = c("stationid", "SRVY", "stratum")) %>% 
  dplyr::rename(latitude = start_latitude, 
                longitude = start_longitude) %>% 
  dplyr::filter(!is.na(stationid))
#   # total biomass excluding empty shells and debris for each year
#   dplyr::filter(group != 'empty shells and debris')  %>%
#   dplyr::mutate(type = ifelse(
#     grepl(pattern = "@", x = (group), fixed = TRUE),
#     # species_name == paste0(genus_taxon, " ", species_taxon),
#     "ital", NA)) %>%
#   tidyr::separate(group, c("group", "species_name", "extra"), sep = "_") %>%
#   dplyr::select(-extra) %>%
#   dplyr::mutate(species_name = gsub(pattern = "@", replacement = " ",
#                                     x = species_name, fixed = TRUE)) %>% 
#   dplyr::ungroup()



cpue_biomass_stratum <- cpue_biomass_station %>%
  ## calculates mean CPUE (weight) by year, group, stratum, and area
  dplyr::ungroup() %>%
  dplyr::group_by(year, group, species_name, species_name1, print_name, 
                  stratum, area, SRVY, taxon) %>%
  dplyr::summarise(cpue_by_group_stratum = mean(cpue_kgha, na.rm = TRUE)) %>% # TOLEDO - na.rm = T?
  ## creates column for meanCPUE per group/stratum/year*area of stratum
  dplyr::mutate(mean_cpue_times_area = (cpue_by_group_stratum * area)) %>%
  ## calculates sum of mean CPUE*area (over the 3 strata)
  dplyr::ungroup() 

cpue_biomass_total <- cpue_biomass_stratum %>%
  dplyr::group_by(year, group, SRVY, species_name, species_name1, print_name, taxon) %>%
  dplyr::summarise(mean_CPUE_all_strata_times_area =
                     sum(mean_cpue_times_area, na.rm = TRUE)) %>% # TOLEDO - na.rm = T?
  
  # calculates total area by adding up the unique area values (each strata has a different value)
  dplyr::left_join(
    x = ., 
    y = cpue_biomass_station %>% 
      dplyr::ungroup() %>%
      dplyr::select(area, SRVY) %>% 
      dplyr::distinct() %>%
      dplyr::group_by(SRVY) %>% 
      dplyr::summarise(total_area = sum(area, na.rm = TRUE)), 
    by = "SRVY") %>%
  
  ## creates column with weighted CPUEs
  dplyr::mutate(weighted_CPUE = (mean_CPUE_all_strata_times_area / total_area)) %>%
  ### uses WEIGHTED CPUEs to calculate biomass
  ## includes empty shells and debris
  dplyr::group_by(year, group, SRVY, species_name, species_name1, print_name) %>%
  dplyr::mutate(biomass_mt = weighted_CPUE*(total_area*.1)) %>%
  # total biomass excluding empty shells and debris for each year
  dplyr::filter(group != 'empty shells and debris')  %>%
  # dplyr::mutate(type = ifelse(
  #   grepl(pattern = "@", x = (group), fixed = TRUE),
  #   # species_name == paste0(genus_taxon, " ", species_taxon),
  #   "ital", NA)) %>%
  # tidyr::separate(group, c("group", "species_name", "extra"), sep = "_") %>%
  # dplyr::select(-extra) %>%
  # dplyr::mutate(species_name = gsub(pattern = "@", replacement = " ",
  #                                   x = species_name, fixed = TRUE)) %>% 
  dplyr::ungroup()


## *** Load Size Comp Design Based Estimates ----------------------------------------------

# GF data
df.ls<-list()
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
  temp<-strsplit(x = a[i], split = "/", fixed = TRUE)[[1]][length(strsplit(x = a[i], split = "/", fixed = TRUE)[[1]])]
  b$survey <- toupper(strsplit(x = temp, split = "_")[[1]][strsplit(x = temp, split = "_")[[1]] %in% c("nbs", "ebs")])
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}

sizecomp <- SameColNames(df.ls)  %>%
  dplyr::filter(year <= maxyr & 
                  stratum == 999999) %>% 
  dplyr::rename(SRVY = survey)  %>% 
  dplyr::select(length, males, females, unsexed, year, species_code, SRVY) %>%
  tidyr::pivot_longer(cols = c(males, females, unsexed),
                      names_to = "sex", values_to = "pop") %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert")) %>%
  dplyr::mutate(length = length/10) %>% 
  dplyr::filter(length > 0)


# Crab 
df.ls<-list()
a<-list.files(path = paste0(dir_data, "/crab/sizecomp/"), 
              # pattern = paste0("sizecomp_"), 
              full.names = TRUE)

for (i in 1:length(a)){
  b <- read_csv(file = a[i])
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  b$file <- a[i]
  # b$survey <- toupper(strsplit(x = a[i], split = "_")[[1]][strsplit(x = a[i], split = "_")[[1]] %in% tolower(SRVY1)])
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}


sizecomp_crab <- SameColNames(df.ls) %>% 
  # dplyr::left_join(
  #   x = SameColNames(df.ls),
  #   y = station_info %>%
  #     dplyr::select(stationid, stratum, SRVY) %>%
  #     unique(),
  #   by = c("gis_station" = "stationid")) %>%
  dplyr::mutate(SRVY = "NBS", 
                species_code = dplyr::case_when(
                  grepl(pattern = "_BK_", x = file, ignore.case = TRUE) ~ 69323, # "blue king crab"
                  grepl(pattern = "_RKC_", x = file, ignore.case = TRUE) ~ 69322, # "red king crab"
                  grepl(pattern = "_CO_", x = file, ignore.case = TRUE) ~ 68580 # "snow crab"
                )) %>%
  dplyr::rename( 
    year = survey_year,
    length = size1,
    unsexed = num_unsexed_size1, 
    males = num_male_size1, 
    females_mat = num_female_size1_mat,
    females_immat = num_female_size1_immat,
    # females_mature = number_female_size1_mature, 
    # females_immature = number_female_size1_immature
  ) %>%
  dplyr::select(length, males, females_mat, females_immat, #_mature, females_immature, 
                unsexed, year, species_code, SRVY#, file, clutch_size
  ) %>%
  tidyr::pivot_longer(cols = c(males, females_mat, females_immat,#_mature, females_immature, 
                               unsexed),
                      names_to = "sex", values_to = "pop") %>%
  dplyr::filter(!is.na(length) & !is.na(pop) & pop != 0 & !is.na(species_code)) %>% 
  dplyr::mutate(sex = dplyr::case_when(
    sex == "unsexed" ~ "Unsexed",
    sex == "males" ~ "Males",
    sex == "females_immat" ~ "Females (Immature)",
    sex == "females_mat" ~ "Females (Mature)",
    TRUE ~ sex
  )) %>%
  dplyr::group_by(sex, length, year, species_code, SRVY) %>% 
  dplyr::summarise(pop = sum(pop, na.rm = TRUE)) %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert")) %>%
  dplyr::mutate(length = length/10) # cm


# Combine

sizecomp <- SameColNames(list("gf" = sizecomp, 
                              "crab" = sizecomp_crab)) %>% 
  dplyr::rename(SRVY = srvy)

sizecomp_maxyr<-sizecomp %>%
  dplyr::filter(year == maxyr)

sizecomp_compareyr<-sizecomp %>%
  dplyr::filter(year == compareyr[1])

## *** Total Biomass ---------------------------------------------------------------

# for tech memo table: calculate biomass for fish and invert taxa in table 7
# Created by: Rebecca Haehn
# Contact: rebecca.haehn@noaa.gov
# Created: 13 January 2022
# script modifed from biomass script for stock assessments
# Modified: 

# fiter to EBS only data by innerjoin (catch and haul) --------------------

## to test this I filtered for YEAR = 2017 in the haul data, row count matches prebiocatch table in Oracle (after running legacy ebs_plusnw script) **do not run with filter to get match
## 
## the filter removes empty banacles, empty bivalve/gastropod shell, invert egg unid, unsorted catch and debris, Polychaete tubes, and unsorted shab 

# create zeros table for CPUE calculation ---------------------------------
# zeros table so every haul/vessel/year combination includes a row for every species caught (combine)
temp1 <- catch_haul_cruises_maxyr %>%
  dplyr::filter(species_code < 99991)

z <-  temp1 %>% 
  tidyr::complete(species_code, 
                  nesting(SRVY, cruise, haul, #vessel, 
                          year, hauljoin, stratum, stationid, distance_fished, net_width)) %>%
  dplyr::select(SRVY, cruise, hauljoin, haul, #vessel, 
                year, species_code, weight, number_fish, stratum, stationid, distance_fished, net_width) %>%
  tidyr::replace_na(list(weight = 0, number_fish = 0))

catch_with_zeros <- 
  dplyr::full_join(x = temp1, 
                   y = z, 
                   by = c("SRVY", "cruise", "hauljoin", "haul", #"vessel", 
                          "year", "species_code", "stratum", "stationid", 
                          "distance_fished", "net_width")) %>%
  dplyr::select(-weight.y, -number_fish.y, -gear_depth, # -abundance_haul, 
                -duration, -net_height) %>%
  dplyr::arrange(year, haul, species_code #,VESSEL
  ) %>%
  dplyr::rename(weight_kg = weight.x, number_fish = number_fish.x) %>%
  tidyr::replace_na(list(weight_kg = 0, number_fish = 0))

# calculate CPUE (mean CPUE by strata) ----------------------------------------------------------
cpue_by_stratum <- catch_with_zeros %>%
  dplyr::mutate(effort = distance_fished * net_width/10,
                cpue_weight_kgperhect = weight_kg/effort) %>%
  dplyr::arrange(stratum, species_code) %>%
  dplyr::group_by(SRVY, species_code, year, stratum) %>%
  dplyr::summarise(cpue_avg_by_stratum = mean(cpue_weight_kgperhect))

# import and join stratum areas -------------------------------------------
cpue_by_stratum <-
  dplyr::left_join(x = cpue_by_stratum,
                   y = stratum_info %>%
                     dplyr::select(stratum, area, SRVY),
                   by = c("SRVY", "stratum")) %>%
  dplyr::mutate(total_area = sum(unique(area)))

# biomass -----------------------------------------------------------------

# ## CANNOT use biomass_*** tables bc they don't contain the info for all species (ie: no poachers, blennies, lumpsuckers, eelpouts, etc.)

biomass_by_stratum <- cpue_by_stratum %>%
  dplyr::mutate(biomass_mt = cpue_avg_by_stratum * (area * 0.1)) %>%
  dplyr::mutate(strata = dplyr::case_when(
    (stratum == 31 | stratum == 32) ~ 30,
    (stratum == 41 | stratum == 42) | stratum == 43 ~ 40,
    (stratum == 61 | stratum == 62) ~ 60, 
    TRUE ~ as.numeric(stratum)))

## this global command disengages scientific notation of numbers
# options(scipen = 999)

# total_biomass <- biomass_by_stratum %>%
#   dplyr::filter((species_code >= 40000 |#& 
#                   # species_code < 99991) |
#                   species_code < 35000) 
#   )
# 
# total_biomass <- sum(total_biomass$biomass_mt, na.rm = TRUE)

total_biomass <- biomass_by_stratum %>%
  dplyr::filter((species_code >= 40000 &
                   species_code < 99991) |
                  (species_code > 1 & 
                     species_code < 35000)) %>% 
  ungroup() %>%
  dplyr::group_by(SRVY) %>% 
  dplyr::summarise(total = sum(biomass_mt, na.rm = TRUE))


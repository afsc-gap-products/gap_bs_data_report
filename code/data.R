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
# full_ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs", set.crs = "auto")
# ggplot() +
#   geom_sf(data = full_ebs_layers$survey.grid)
# 
# sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs", set.crs = "auto")
# ggplot() +
#   geom_sf(data = sebs_layers$survey.grid)
# image.png
# 
# nbs_grid <- full_ebs_layers$survey.grid %>% filter(STATIONID %in% akgfmaps::get_survey_stations(select.region = "nbs"))
# ggplot() +
#   geom_sf(data = nbs_grid)

out.crs <- "EPSG:3338"

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
    extrap.box = c(xmin = -180, xmax = -156, ymin = 54, ymax = 62), 
    strat0 = c("10", "20", "30", "31", "32", "40", "41", "42", "43", "50", "60", "61", "62", "82", "90", 
               "999"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.south", 
      set.crs = out.crs)#,
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
    extrap.box = c(xmin = -179.5, xmax = -157, ymin = 54, ymax = 68),
    strat0 = c("70", "71", "81", 
               "999"), 
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.north", 
      set.crs = out.crs)#,
    # report_species = report_species_NEBS
  ), 
  "NEBS" = list(
    sectname = "NEBS-BTS-Report", 
    SURVEY = "eastern and northern Bering Sea",
    map.area = "bs.all", 
    SRVY1 = c("EBS", "NBS"), 
    SRVY0 = "BS", # in Oracle
    SRVY00 = c(98, #NBS
               143), # EBS
    station_id = akgfmaps::get_survey_stations(
      select.region = "bs.all"),
    extrap.box = c(xmin = -179.5, xmax = -157, ymin = 54, ymax = 68),
    strat0 = c("10", "20", "30", "31", "32", "40", "41", "42", "43", "50", "60", "61", "62", "82", "90",
               "70", "71", "81", 
               "999"), 
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.all", 
      set.crs = out.crs)#,
    # report_species = report_species_NEBS
  )
)

a <- report_types[names(report_types) == SRVY][[1]]
for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }


# add color to survey.area
survey_reg_col <- gray.colors(length(unique(reg_dat$survey.area$SURVEY))+2)
survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
reg_dat$survey.area <- reg_dat$survey.area %>%
  dplyr::mutate(
    SRVY = dplyr::case_when(
      SURVEY == "EBS_SHELF" ~ "EBS", 
      SURVEY == "NBS_SHELF" ~ "NBS"), 
    color = alpha(colour = survey_reg_col, 0.7), 
    SRVY_long = dplyr::case_when(
      SRVY == "EBS" ~ "Eastern Bering Sea", 
      SRVY == "NBS" ~ "Northern Bering Sea") )

# fix reg_dat$graticule
reg_dat$graticule <- sf::st_transform(x = reg_dat$graticule, crs = CRS(as.character(reg_dat$crs)[1]))

# lon_label <- reg_dat$lon.breaks
# lat_label <- reg_dat$lat.breaks
# # get the lon and lat breaks in the right projection
# remove_lon <- 0
# remove_lat <- 0
# if (length(lon_label) > length(lat_label)) {
#   remove_lat <- (length(lon_label)-length(lat_label))
#   lat_label <- c(lat_label, rep_len(x = 0, length.out = remove_lat))
# } else if (length(lon_label) < length(lat_label)) {
#   remove_lon <- (length(lat_label)-length(lon_label))
#   lon_label <- c(lon_label, rep_len(x = 0, length.out = remove_lon))
# }
# 
# d <- data.frame("X" = lon_label, "Y" = lat_label)
# coordinates(d) <- c("X", "Y")
# sp::proj4string(d) <- CRS("+proj=longlat +datum=WGS84") 
# dd <- data.frame(sp::spTransform(d, CRS("+init=EPSG:3338")))
# 
# reg_dat$lon.label <- reg_dat$lon.breaks
# reg_dat$lat.label <- reg_dat$lat.breaks
# reg_dat$lon.breaks <- dd$X[1:(nrow(dd)-remove_lon)]
# reg_dat$lat.breaks <- dd$Y[1:(nrow(dd)-remove_lat)]


# Load data --------------------------------------------------------------------
print("Load data")

# *** Load Documents from Google Drive -----------------------------------------


# ssl_drive_id <- "1gJbb2qoqXMPFGwuk65b1HsazoMptLU6j"
# as_dribble(as_id(ssl_drive_id)) %>% 
#   drive_ls() %>% 
#   group_nest(row_number()) %>% 
#   pull(data) %>% 
#   walk(~ drive_download(.x$id, path = here::here("spatial_data", "shp_files", .x$name)))


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
  a <- googledrive::drive_ls(path = id_googledrive)
  for (i in 1:nrow(a)){
    googledrive::drive_download(file = googledrive::as_id(a$id[i]), 
                                # type = "docx", 
                                overwrite = TRUE, 
                                path = paste0(dir_out_rawdata, "/", a$name[i]))
  }
  
}
# *** Load Main Oracle Data -------------------------------------------------------------

a <- paste0(dir_data, "oracle/", c("catch", "haul", "length_types", 
                                   "species", "species_classification", "specimen", 
                                   "stratum", "v_cruises", "v_extract_final_lengths", "vessels"), ".csv") 
for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  temp <- strsplit(x = a[i], split = "/")
  temp <- gsub(pattern = "\\.csv", replacement = "", x = temp[[1]][length(temp[[1]])])
  assign(x = paste0(temp, "0"), value = b)
}

# *** Load Public Data -------------------------------------------------------

# load(here::here("data", "publicdata", "all_data.Rdata"))
# lastdl <- ageoffile(here::here("data", "publicdata", "all_data.Rdata"))   

# Wrangle Data -----------------------------------------------------------------

# *** report_spp and spp_info ---------------------------------------------------------------
print("report_spp and spp_info")

report_spp <- readr::read_csv(file = paste0(dir_out_rawdata, "/0_species_local_names.csv"), 
                              skip = 1, show_col_types = FALSE) %>% 
  dplyr::select(!(dplyr::starts_with(ifelse(report_title == "community", "datar_", "community_")))) # %>%
# dplyr::filter(!grepl(pattern = "other ", x = group) &
#!grepl(pattern = "all ", x = group) &
# !grepl(pattern = "egg ", x = group))

names(report_spp)[
  grepl(pattern = ifelse(report_title == "community", "community_", "datar_"), 
        x = names(report_spp))] <- 
  gsub(pattern = ifelse(report_title == "community", "community_", "datar_"), 
       replacement = "", 
       x = names(report_spp)[
         grepl(pattern = ifelse(report_title == "community", "community_", "datar_"), 
               x = names(report_spp))])

spp_info <- 
  dplyr::left_join(x = species0, 
                   y = species_classification0, 
                   by = "species_code") %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert")) %>%
  dplyr::mutate(used_in_counts = 
                  dplyr::if_else(
                    species_code %in% #10:99988, 
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
print("cruises + maxyr  + compareyr")

cruises <- v_cruises0 %>% 
  dplyr::select(cruise_id,  year, survey_name, vessel_id, cruise, survey_definition_id, 
                vessel_name, start_date, end_date, cruisejoin) %>% 
  dplyr::filter(year != 2020 & # no surveys happened this year that I care about
                  year >= 1982 &
                  year <= maxyr &
                  survey_definition_id %in% SRVY00) %>% 
  dplyr::mutate(vess_shape = substr(x = vessel_name, 1,1)) %>%
  dplyr::mutate(vessel_ital = paste0("FV *", stringr::str_to_title(vessel_name), "*")) %>%
  dplyr::mutate(vessel_name = paste0("FV ", stringr::str_to_title(vessel_name))) %>%
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
print("haul + maxyr  + compareyr")

temp <- dplyr::left_join(
  y = haul0, 
  x = cruises %>% 
    dplyr::select(cruisejoin, survey_definition_id), # %>% 
  # dplyr::filter(SRVY %in% SRVY1), 
  by = "cruisejoin") %>%  
  dplyr::mutate(year = as.numeric(format(as.Date(start_time, 
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

# *** other var (survey additions, *yrs, etc. ----------------------------------
print("define other vars")

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

## *** Load CPUE Design Based Estimates ----------------------------------------------

print("Load CPUE Design Based Estimates")


# cpue <- readr::read_csv(file = paste0(dir_data, "/oracle/racebase_public_foss.csv"),
#                         show_col_types = FALSE) %>%
#   janitor::clean_names() %>%
#   # dplyr::filter(species_code %in% c(69322, 69323, 68580)) %>%
#   dplyr::select(year, vessel_id, srvy,  stratum, station, scientific_name,
#                 species_code, longitude_dd_start, latitude_dd_start, #hauljoin,
#                 haul,
#                 cpue_noha, cpue_kgha, common_name#, area_fished_ha, taxon
#                 ) %>%
#   dplyr::rename(SRVY = srvy,
#                 latitude = latitude_dd_start,
#                 longitude = longitude_dd_start,
#                 vessel = vessel_id,
#                 stationid = station,
#                 species_name = scientific_name) %>%
#   dplyr::filter(SRVY %in% SRVY1)

# cpue <- dplyr::bind_rows(cpue, temp)

df.ls<-list()

a <- paste0(dir_data, "/oracle/", c("ebsshelf_cpue", "nbs_cpue"), ".csv")

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  b$file <- a[i]
  # temp <- strsplit(x = a[i], split = "_", fixed = TRUE)[[1]]
  # temp <- strsplit(x = a[i], split = "/", fixed = TRUE)[[1]][length(strsplit(x = a[i], split = "/", fixed = TRUE)[[1]])]
  b$survey <- gsub(pattern = "shelf", replacement = "", x = b$survey, ignore.case = TRUE)
  b$survey <- gsub(pattern = "_", replacement = "", x = b$survey, ignore.case = TRUE)
  # b$survey <- toupper(strsplit(x = b$survey, split = "_")[[1]][strsplit(x = b$survey, split = "_")[[1]] %in% c("nbs", "ebs")])
  # b$survey <- toupper(temp[temp %in% tolower(SRVY1)])
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}

cpue <-
  SameColNames(df.ls)  %>%
  dplyr::rename(SRVY = survey) %>%
  dplyr::filter(SRVY %in% SRVY1 & 
                  !(species_code %in% c(69323, 69322, 68580, 68560)) &
                  year <= maxyr) %>% 
  dplyr::left_join(
    x = ., 
    y = spp_info %>% 
      dplyr::select(common_name, species_name, species_code), 
    by = "species_code") %>% 
  dplyr::left_join(
    x = ., 
    y = haul %>% 
      dplyr::select(stationid, SRVY, year, start_latitude, start_longitude), 
    by = c("stationid", "SRVY", "year")) %>% 
  dplyr::rename(cpue_noha = numcpue, 
                cpue_kgha = wgtcpue, 
                latitude = start_latitude, 
                longitude = start_longitude) %>% 
  dplyr::mutate(cpue_kgha = cpue_kgha/100) %>%
  dplyr::select(-effort, -number_fish, -catchjoin, -net_width, -distance_fished, -catchjoin, -file, -weight)

# EBS Crab
cpue_crab <- readr::read_csv(file = paste0(dir_data, "oracle/gap_ebs_nbs_crab_cpue.csv"),
                             show_col_types = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::select(-x1) %>%
  dplyr::rename(cpue_kgha = cpuewgt_total,
                cpue_noha = cpuenum_total,
                year = survey_year) %>% 
  dplyr::left_join(
    x = ., 
    y = haul %>% 
      dplyr::rename(latitude = start_latitude, 
                    longitude = start_longitude) %>%
      dplyr::select(vessel, SRVY, stratum, stationid, hauljoin, haul, cruise, latitude, longitude),
    by = "hauljoin") %>% 
  dplyr::left_join(
    x = ., 
    y = spp_info %>% 
      dplyr::select(species_code, common_name, species_name), 
    by = "species_code") %>% 
  dplyr::arrange(desc(year))

# bind cpue
cpue <- dplyr::bind_rows(cpue_crab, cpue)  %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish",
    species_code >= 40001 ~ "invert"))

cpue_maxyr <- cpue %>%
  dplyr::filter(year == maxyr)

cpue_compareyr<- cpue %>%
  dplyr::filter(year == compareyr[1])

cpue$common_name[cpue$species_name == "Neptunea heros"] <- "northern neptune whelk"

# *** stratum_info (survey area) -------------------------------------------

print("stratum_info (survey area)")

temp <- function(strat_yr) {#yr) {
  
  # # unique  and sort are not necessary, just easier for troubleshooting
  # if (sum(yr<unique(stratum0$year)) == 0) {
  # # if (sum((yr - stratum0$year)<0 %in% TRUE) == 0) {
  #   # if there are no stratum years greater than yr, use the most recent stratum year
  # strat_yr <- max(stratum0$year)
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

stratum_info <- temp(strat_yr =strat_yr)#yr = strat_yr)

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
print("station_info")

# TOLEDO - has AA-10 not AA-21
# station_info <- stations0 %>%
#   dplyr::filter(stratum %in% reg_dat$survey.strata$Stratum) %>% 
#   dplyr::left_join(x = ., 
#                    y = stratum_info %>% 
#                      dplyr::select(stratum, SRVY), 
#                    by = "stratum") 

station_info <- haul %>% #  
  # dplyr::filter(year == strat_yr) %>%
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
print("stratum_info (survey area) reprise")

stratum_info <- 
  dplyr::left_join(
    x = stratum_info, 
    y = haul_maxyr %>% 
      dplyr::distinct(stratum, stationid, stationid) %>% 
      dplyr::select(stratum, stationid) %>% 
      dplyr::group_by(stratum) %>% 
      dplyr::summarise(stations_completed = length(unique(stationid))) %>% 
      dplyr::select(stratum, stations_completed), 
    by = "stratum") %>% 
  dplyr::left_join(
    x = ., 
    y = station_info %>% 
      dplyr::select(stratum, stationid) %>% 
      dplyr::group_by(stratum) %>% 
      dplyr::summarise(stations_avail = length(unique(stationid))) %>% 
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
print("haul_cruises_vess_ + _maxyr + _compareyr")

temp <- function(cruises_, haul_){
  haul_cruises_vess_ <- 
    dplyr::left_join(x = cruises_ ,
                     y = haul_ %>% 
                       dplyr::select(cruisejoin, hauljoin, stationid, stratum, haul, 
                                     gear_depth, duration, distance_fished, net_width, net_height,
                                     start_time) %>% 
                       dplyr::group_by(cruisejoin, hauljoin, stationid, stratum, haul, 
                                       gear_depth, duration, distance_fished, net_width, net_height) %>% 
                       dplyr::summarise(start_date_haul = min(start_time),
                                        end_date_haul = max(start_time), 
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

haul_cruises_vess_maxyr <- temp(cruises_ = cruises_maxyr, haul_ = haul_maxyr) 

haul_cruises_vess_compareyr <- temp(cruises_compareyr, haul_compareyr) 

# *** vessel_info -------------------------------------------------------
print("vessel_info")

vessel_info <-  haul_cruises_vess_maxyr %>% 
  dplyr::select("vessel_name", "vessel_ital", "vessel", "tonnage",
                "length_m", "length_ft", "vess_shape") %>% 
  unique() %>% 
  dplyr::mutate(img = dplyr::case_when(
    vessel == 94 ~ "94_vesteraalen.png", 
    vessel == 162 ~ "163_alaskaknight.png")) %>% 
  dplyr::arrange(vessel_name)

# *** haul_cruises + _maxyr + _compareyr ------------------------------------------
print("haul_cruises + _maxyr + _compareyr")

temp <- function(haul_cruises_vess_){
  
  haul_cruises_ <- 
    dplyr::left_join(
      x = haul_cruises_vess_ %>% 
        dplyr::select(year, survey_name, cruise, SRVY_start, 
                      survey_definition_id, SRVY, SRVY_long, 
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
                         end_date_cruise = max(end_date_cruise)) %>% 
        dplyr::mutate(start_mo_long = 
                        format(x = as.Date(start_date_haul, "%m/%d/%Y"), 
                               format="%B"), 
                      end_mo_long = 
                        format(x = as.Date(end_date_haul, "%m/%d/%Y"), 
                               format="%B")),
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
print("catch")

# ## there should only be one species_code observation per haul event, however
# ## there are occassionally multiple (with unique catchjoins). 
# ## I suspect that this is because a species_code was updated or changed, 
# ## so we will need to sum those counts and weights

catch <-  catch0 %>% 
  dplyr::group_by(region, cruisejoin, hauljoin, vessel, haul, species_code) %>% 
  dplyr::summarise(weight = sum(weight, na.rm = TRUE), 
                   number_fish = sum(number_fish, na.rm = TRUE)) %>% 
  dplyr::ungroup()


# *** catch_haul_cruises_maxyr + maxyr-1-----------------------------------------------
print("catch_haul_cruises + _maxyr + _compareyr")

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
                      duration, distance_fished ,net_width ,net_measured, net_height), 
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
                      number_fish), # , subsample_code 
      by = c("hauljoin", "cruisejoin"))
  
  return(catch_haul_cruises_)
}

catch_haul_cruises <- temp(cruises, haul, catch)

catch_haul_cruises_maxyr <- temp(cruises_ = cruises_maxyr, 
                                 haul_ = haul_maxyr, 
                                 catch = catch)

catch_haul_cruises_compareyr <- temp(cruises_compareyr, haul_compareyr, catch)

# *** report_spp and spp_info part 2 ---------------------------------------------------------------
print("report_spp and spp_info part 2")

spp_info_maxyr <- spp_info %>%
  dplyr::filter(species_code %in%
                  unique(catch_haul_cruises_maxyr$species_code))

report_spp2 <- add_report_spp(spp_info = spp_info_maxyr, 
                              spp_info_codes = "species_code", 
                              report_spp = report_spp, 
                              report_spp_col = "order", 
                              report_spp_codes = "species_code", 
                              lang = FALSE) %>% 
  dplyr::filter((species_code %in% spp_info_maxyr$species_code)) 


report_spp <- report_spp %>%
  dplyr::filter(file_name %in% c(report_spp2$file_name, NA))

# *** length ---------------------------------------------------------------
print("length")

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


ebscrab0 <- read.csv(file = paste0(dir_data, "oracle/ebscrab.csv")) %>% 
  janitor::clean_names()

ebscrab_nbs0 <- read.csv(file = paste0(dir_data, "oracle/ebscrab_nbs.csv")) %>% 
  janitor::clean_names()

length_crab <- dplyr::bind_rows(
  ebscrab0 %>%
    dplyr::filter(!(cruise %in% unique(ebscrab_nbs0$cruise))) %>% # there may be some nbs data in the ebs (201002)
    dplyr::mutate(SRVY = "EBS"),
  ebscrab_nbs0 %>%
    dplyr::mutate(SRVY = "NBS") ) %>% 
  dplyr::left_join(
    x = .,
    y = haul %>% 
      dplyr::select(SRVY, hauljoin,haul_type, stationid, abundance_haul, performance), # haul, cruise, 
    by = c("SRVY","hauljoin")) %>% # ,  "haul", "station" = "stationid", "cruise"
  dplyr::filter(
    abundance_haul == "Y" &
      performance >= 0 &
      haul_type %in% c(3)) %>% # standard stations #17, # resample stations
  dplyr::mutate(year = as.numeric(substr(cruise, start = 1, stop = 4))) %>% 
  dplyr::mutate(sex_code = sex, 
                sex = dplyr::case_when(
                  sex == 1 ~ "males",
                  sex == 0 ~ "unsexed",
                  (clutch_size == 0 & sex == 2) ~ "immature females", 
                  (clutch_size >= 1 & sex == 2) ~ "mature females"), 
                length = dplyr::case_when(
                  species_code %in% c(68580, 68590, 68560) ~ width,  # "snow crab"
                  TRUE ~ length), 
                frequency = 1) %>%
  dplyr::select(-width) %>% 
  dplyr::filter(!is.na(length) & length != 999 & !is.na(cruise)) %>% 
  dplyr::group_by(year, species_code, SRVY, sex, sex_code, length) %>%
  dplyr::summarise(frequency = n()) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish", 
      species_code >= 40001 ~ "invert"), 
    length_type = dplyr::case_when( # what are other crabs?
      species_code %in% c(68580, 68590, 68560) ~ 8,  # 8 - Width of carapace 
      TRUE ~ 7 # 7 - Length of carapace from back of right eye socket to end of carapace # species_code %in% c(69322, 69323, 69400, 69401) ~ 7, 
    ) ) 

# Combine
length_data <- SameColNames(list(
  "gf" = length_data, 
  "crab" = length_crab))  %>% 
  dplyr::rename(SRVY = srvy) %>% 
  dplyr::left_join(x = .,
                   y = spp_info %>% 
                     dplyr::select(species_code, common_name, species_name),
                   by = "species_code") %>% 
  ungroup()

# *** length_type ----------------------------------------------------------
print("length_type")

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
print("Specimen + maxyr")

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
print("bottom tempertures")

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

# temp <- coldpool:::cold_pool_index %>% 
#   dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>% 
#   dplyr::rename(bt = MEAN_GEAR_TEMPERATURE, 
#                 st = MEAN_SURFACE_TEMPERATURE) %>% 
#   dplyr::filter(YEAR < maxyr) %>% 
#   janitor::clean_names() %>% 
#   dplyr::mutate(bt_mean = mean(bt, na.rm = TRUE)) %>% 
#   dplyr::mutate(st_mean = mean(st, na.rm = TRUE))

temps_avg_yr <- coldpool:::cold_pool_index %>% 
  dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::rename(bt = MEAN_GEAR_TEMPERATURE, 
                st = MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::filter(YEAR <= maxyr) %>% 
  janitor::clean_names() %>% 
  dplyr::arrange(desc(bt)) %>%
  dplyr::mutate(warmest_rank = 1:nrow(.)) %>%
  dplyr::mutate(bt_mean = mean(bt, na.rm = TRUE), 
                st_mean = mean(st, na.rm = TRUE), 
                bt_mean_maxyr = mean(bt, na.rm = TRUE), 
                st_mean_maxyr = mean(st, na.rm = TRUE), 
                SRVY = "EBS") %>%
  dplyr::arrange(SRVY, year) %>%
  dplyr::mutate(bt_above_mean = bt>bt_mean, 
                st_above_mean = st>st_mean, 
                case = dplyr::case_when(
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

temps_avg_yr_abovebelow <- list(
  # temp1 <- 
  "above" = 
    temps_avg_yr %>% 
    dplyr::filter(SRVY == "EBS" & 
                    bt_above_mean == TRUE &
                    year >= 2006) %>% # the begining of the last cold stanza
    dplyr::ungroup() %>%
    dplyr::arrange(-year) %>% 
    dplyr::select(year) %>% 
    unlist() ,#%>% 
  # data.frame() ,
  # names(temp1) <- c("above")
  
  # temp2 <- 
  "below" = 
    temps_avg_yr %>% 
    dplyr::filter(SRVY == "EBS" & 
                    bt_above_mean == FALSE &
                    year >= 2006) %>%  # the begining of the last cold stanza
    dplyr::ungroup() %>%
    dplyr::arrange(-year) %>% 
    dplyr::select(year) %>% 
    unlist() #%>% 
  # data.frame() 
  # names(temp2) <- c("below")
)

# # make the same length so they can be bound together
# if (nrow(temp1)>nrow(temp2)) {
#   temp2 <- temp2 %>% 
#     dplyr::bind_rows(., 
#                      data.frame(below = rep_len(NA, (nrow(temp1)-nrow(temp2)))))
# } else {
#   temp1 <- temp1 %>% 
#     dplyr::bind_rows(., 
#                      data.frame(above = rep_len(NA, (nrow(temp2)-nrow(temp1)))))
# }
# 
# temps_avg_yr_abovebelow <- dplyr::bind_cols(temp1, temp2)


sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs",
                                         set.crs = coldpool:::ebs_proj_crs)

coldpool_ebs_bin_area <- coldpool:::cold_pool_index %>%
  dplyr::rename(year = YEAR) %>%
  dplyr::filter(year <= maxyr) %>%
  dplyr::mutate(lteminus1 = AREA_LTEMINUS1_KM2,
                lte0 = AREA_LTE0_KM2 - AREA_LTEMINUS1_KM2,
                lte1 = AREA_LTE1_KM2 - AREA_LTE0_KM2,
                lte2 = AREA_LTE2_KM2 - AREA_LTE1_KM2) %>%
  dplyr::select(year, lteminus1, lte0, lte1, lte2) %>%
  reshape2::melt(id.vars = "year") %>%
  dplyr::mutate(area_km2 = value, 
                variable = factor(variable, 
                                  levels = c( "lte2", "lte1", "lte0", "lteminus1"),
                                  labels = c("\u2264 2\u00b0C", "\u2264 1\u00b0C", "\u2264 0\u00b0C", "\u2264 -1\u00b0C")),
                label = variable, 
                proportion = value/sebs_layers$survey.area$AREA_KM2, 
                perc = proportion*100)  

coldpool_ebs_total_area <- coldpool_ebs_bin_area %>%
  dplyr::group_by(year) %>% 
  dplyr::summarise(proportion = sum(proportion, na.rm = TRUE), 
                   perc = sum(perc, na.rm = TRUE), 
                   value = sum(value, na.rm = TRUE), 
                   area_km2 = sum(area_km2, na.rm = TRUE)) %>%
  dplyr::arrange(desc(perc)) %>% 
  dplyr::mutate(rank = 1:nrow(.)) %>% 
  dplyr::arrange(desc(year))

# temp <- coldpool:::ebs_bottom_temperature
# temp <- projectRaster(temp, crs = crs(reg_dat$akland))
# temp <- as(temp, "SpatialPixelsDataFrame")
# temp <- as.data.frame(temp)
# temp1 <- gsub(pattern = "[A-Za-z]+", 
#               replacement = "", 
#               x = names(temp[!(names(temp) %in% c("x", "y"))]))
# temp1 <- gsub(pattern = "_", replacement = "", x = temp1)
# colnames(temp) <- c(temp1, "latitude", "longitude")
# cold_pool_area <- temp %>% 
#   tidyr::pivot_longer(values_to = "value", 
#                       names_to = "year", 
#                       cols = all_of(temp1)) %>% 
#   dplyr::mutate(bin = cut(x = value, 
#                           breaks = c(-Inf, seq(from = -1, to = 2, by = 1)))) %>% 
#   dplyr::group_by(year, bin) %>%
#   dplyr::summarise(freq = n()) %>%
#   dplyr::filter(year <= maxyr) %>%
#   dplyr::mutate(perc = (freq/nrow(temp)) * 100) %>% # length(temp$`1982`) = 21299 is the number of cells and shouldnt change?
#   dplyr::mutate(label = dplyr::case_when(
#     bin == "(-Inf,-1]" ~ "> -1\u00B0C",
#     bin == "(-1,0]" ~ "-1 to 0\u00B0C",
#     bin == "(0,1]" ~ "0 to 1\u00B0C",
#     bin == "(1,2]" ~ "1 to 2\u00B0C")) %>% 
#   dplyr::filter(!is.na(bin)) %>%
#   dplyr::mutate(label = factor(label, 
#                                levels=c("1 to 2\u00B0C", "0 to 1\u00B0C", "-1 to 0\u00B0C", "> -1\u00B0C"), 
#                                ordered = TRUE))


# sebs_layers <- akgfmaps::get_base_layers(select.region = "sebs",
#                                          set.crs = coldpool:::ebs_proj_crs)
# 
# coldpool_ebs_area <- coldpool:::cold_pool_index %>%
#   dplyr::filter(YEAR <= maxyr) %>%
#   dplyr::mutate(lteminus1 = AREA_LTEMINUS1_KM2,
#                 lte0 = AREA_LTE0_KM2 - AREA_LTEMINUS1_KM2,
#                 lte1 = AREA_LTE1_KM2 - AREA_LTE0_KM2,
#                 lte2 = AREA_LTE2_KM2 - AREA_LTE1_KM2) %>%
#   dplyr::select(YEAR, lteminus1, lte0, lte1, lte2) %>%
#   reshape2::melt(id.vars = "YEAR") %>%
#   dplyr::mutate(variable = factor(variable, 
#                                   levels = c( "lte2", "lte1", "lte0", "lteminus1"),
#                                   labels = c("\u2264 2\u00b0C", "\u2264 1\u00b0C", "\u2264 0\u00b0C", "\u2264 -1\u00b0C")),
#                 proportion = value/sebs_layers$survey.area$AREA_KM2)


## *** Load Size Comp Design Based Estimates ----------------------------------------------
print("Load Size Comp Design Based Estimates")

# GF data
df.ls<-list()
a<-list.files(path = paste0(dir_data, "/oracle/"), 
              pattern = paste0("sizecomp_"), 
              full.names = TRUE)

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
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
a<-list.files(path = paste0(dir_data, 
                            # "/oracle/"), 
                            "/crab/sizecomp/"), 
              # pattern = paste0("size1_"),
              full.names = TRUE)

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
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
                  grepl(pattern = "_BKC_", x = file, ignore.case = TRUE) ~ 69323, # "blue king crab"
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
    sex == "unsexed" ~ "unsexed",
    sex == "males" ~ "males",
    sex == "females_immat" ~ "immature females",
    sex == "females_mat" ~ "mature females",
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


# *** Load Biomass Design Based Estimates ----------------------------------------------

print("Load Biomass Design Based Estimates")

df.ls<-list()

a <- paste0(dir_data, "/oracle/", c("biomass_ebs_plusnw", "biomass_ebs_plusnw_grouped", "biomass_nbs_safe"), ".csv")

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE) %>%
    janitor::clean_names()
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  b$file <- a[i]
  temp<-strsplit(x = a[i], split = "/", fixed = TRUE)[[1]][length(strsplit(x = a[i], split = "/", fixed = TRUE)[[1]])]
  b$survey <- toupper(strsplit(x = temp, split = "_")[[1]][strsplit(x = temp, split = "_")[[1]] %in% c("nbs", "ebs")])
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}

biomass_strat <- SameColNames(df.ls)  %>%
  dplyr::filter(year <= maxyr &
                  !(species_code %in% c(69323, 69322, 68580, 68560)) ) %>% 
  dplyr::rename(SRVY = survey) %>% 
  dplyr::filter(!is.na(species_code)) %>% 
  # modify for spp
  dplyr::filter(!(year < 1996 & common_name == "northern rock sole" )) %>% # 10263 NRS
  dplyr::filter(!(year < 2000 & common_name == "Bering skate" ))  %>% 
  unique() %>%
  dplyr::mutate(sdcpuewt = sqrt(varmnwgtcpue), 
                sdcpuenum = sqrt(varmnnumcpue), 
                sdbio = sqrt(varbio), 
                sdpop = sqrt(varpop))

# crab biomass and abundance data
temp <- dplyr::bind_rows(
  ebscrab0 %>%
    dplyr::filter(!(cruise %in% unique(ebscrab_nbs0$cruise))) %>% # there may be some nbs data in the ebs (201002)
    dplyr::mutate(SRVY = "EBS"),
  ebscrab_nbs0 %>%
    dplyr::mutate(SRVY = "NBS") ) %>%
  dplyr::mutate(length = dplyr::case_when(
    species_code %in% c(68580, 68590, 68560) ~ width,  # "snow crab"
    TRUE ~ length)) %>%
  dplyr::filter(!is.na(length)) %>% # if NA, it was not lengthed!
  dplyr::select(hauljoin, SRVY, species_code, station) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    x = .,
    y = haul %>%
      dplyr::select(hauljoin, stratum, year),
    by = c("hauljoin")) %>%
  dplyr::group_by(SRVY, species_code, stratum, year) %>%
  dplyr::summarise(lencount = n()) %>%
  dplyr::filter(!is.na(stratum))

temp <- dplyr::bind_rows(
  temp, 
  temp %>% 
    dplyr::group_by(SRVY, species_code, year) %>% 
    dplyr::summarise(lencount = n()) %>% 
    dplyr::mutate(stratum = 999)) # survey total

a <- paste0(dir_data, "/oracle/", "gap_ebs_nbs_abundance_biomass.csv")
biomass_tot_crab <- readr::read_csv(file = a, 
                                    show_col_types = FALSE) %>%
  janitor::clean_names() %>% 
  dplyr::select(-x1) %>%
  dplyr::rename(biomass = biomass_total,
                lowerb = biomass_lower_ci, 
                upperb = biomass_upper_ci, 
                population = abundance, 
                upperp = abundance_lower_ci, 
                lowerp = abundance_upper_ci, 
                SRVY = survey_region,
                year = survey_year) %>%
  dplyr::left_join(
    x = ., 
    y = spp_info %>% 
      dplyr::select(species_code, common_name, species_name), 
    by = "species_code") %>%
  dplyr::arrange(desc(year)) %>% 
  dplyr::mutate(stratum = 999, 
                file = a) %>% 
  dplyr::left_join(x = .,
                   y = temp,
                   by = c("SRVY", "species_code", "stratum", "year")) %>% 
  dplyr::mutate(lencount = ifelse(is.na(lencount), 0, lencount))

biomass_strat <-  dplyr::bind_rows(biomass_tot_crab, biomass_strat)  %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert"))

# biomass_strat <- dplyr::bind_rows(
#   biomass_strat %>% # select crab data
#     dplyr::filter(species_code %in% unique(temp$species_code)) %>%
#     dplyr::select(-lencount) %>%
#     dplyr::left_join(x = ., 
#                      y = temp, 
#                      by = c("species_code", "SRVY", "stratum", "year")), 
#   biomass_strat %>% 
#     dplyr::filter(!(species_code %in% unique(temp$species_code)))) %>% 
#   dplyr::filter(SRVY %in% SRVY1)


biomass <- biomass_strat %>%
  dplyr::filter(stratum == 999)

biomass_maxyr<-biomass %>%
  dplyr::filter(year == maxyr)

biomass_compareyr<-biomass %>%
  dplyr::filter(year == compareyr[1])

# *** Calculate Biomass and CPUE -----------------------------------------------------------

print("Calculate Biomass and CPUE")

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
  dplyr::mutate(effort = distance_fished * net_width/10) %>%
  dplyr::mutate(cpue_kgha = wt_kg_summed_by_station/effort) %>%
  dplyr::mutate(cpue_noha = ifelse(wt_kg_summed_by_station > 0 & num_summed_by_station == 0, NA,
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

# bb <- dplyr::bind_rows(
#   cpue_biomass_station %>% 
#     dplyr::filter(!species_code %in% c(69323, 69322, 68580, 68560)), 
#   cpue_biomass_station %>% 
#     dplyr::filter(species_code %in% c(69323, 69322, 68580, 68560))
# )

if (report_title == "community") {
cpue_biomass_station <- dplyr::bind_rows(
  dplyr::left_join(
    x = cpue_biomass_station %>% 
      dplyr::filter(print_name %in% c("blue king crab", "red king crab", "snow crab")) %>% 
      dplyr::select(-cpue_kgha, -cpue_noha), 
    y = cpue_crab %>% 
      dplyr::mutate(print_name = dplyr::case_when(
        species_code == 69323 ~ "blue king crab", 
        species_code == 69322 ~ "red king crab", 
        species_code == 68580 ~ "snow crab", 
        # species_code == 68560 ~ "Tanner crab", 
      )) %>% 
      dplyr::select(print_name, year, hauljoin, cpue_kgha, cpue_noha), 
    by = c("print_name", "year", "hauljoin")
  ), 
  cpue_biomass_station %>% 
    dplyr::filter(!(print_name %in% c("blue king crab", "red king crab", "snow crab")) ) )
}

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
# we'll remove crab stuff from here soon

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


if (report_title == "community") {
  
  cpue_biomass_total <- dplyr::bind_rows(
    dplyr::left_join(
      x = cpue_biomass_total %>% 
        dplyr::filter(print_name %in% c("blue king crab", "red king crab", "snow crab")) %>% 
        dplyr::select(-weighted_CPUE, -biomass_mt, -mean_CPUE_all_strata_times_area), 
      y = biomass_tot_crab %>% 
        dplyr::mutate(print_name = dplyr::case_when(
          species_code == 69323 ~ "blue king crab", 
          species_code == 69322 ~ "red king crab", 
          species_code == 68580 ~ "snow crab", 
          # species_code == 68560 ~ "Tanner crab", 
        )) %>% 
        dplyr::rename(biomass_mt = biomass) %>% 
        dplyr::select(print_name, year, SRVY, biomass_mt), 
      by = c("print_name", "year", "SRVY")
    ), 
    cpue_biomass_total %>% 
      dplyr::filter(!(print_name %in% c("blue king crab", "red king crab", "snow crab"))) )
  
  cpue_biomass_stratum <- cpue_biomass_stratum %>%
    # remove crab stuff because they use diff strata etc
    dplyr::filter(!(print_name %in% c("blue king crab", "red king crab", "snow crab")))
}


## *** Total Biomass ---------------------------------------------------------------

print("Total Biomass")

calc_cpue_bio <- function(catch_haul_cruises0){
  
  # for tech memo table: calculate biomass for fish and invert taxa in table 7
  # Created by: Rebecca Haehn
  # Contact: rebecca.haehn@noaa.gov
  # Created: 13 January 2022
  # script modifed from biomass script for stock assessments
  # Modified: 
  
  # *** *** fiter to EBS only data by innerjoin (catch and haul) --------------------
  
  ## to test this I filtered for YEAR = 2017 in the haul data, row count matches prebiocatch table in Oracle (after running legacy ebs_plusnw script) **do not run with filter to get match
  ## 
  ## the filter removes empty banacles, empty bivalve/gastropod shell, invert egg unid, unsorted catch and debris, Polychaete tubes, and unsorted shab 
  
  # *** *** create zeros table for CPUE calculation ---------------------------------
  # zeros table so every haul/vessel/year combination includes a row for every species caught (combine)
  
  temp1 <- catch_haul_cruises0 #%>% 
  # dplyr::group_by(year, SRVY, cruisejoin, hauljoin, stationid, stratum, haul, cruise, 
  #                 species_code, distance_fished, net_width) %>% 
  # dplyr::summarise(weight = sum(weight, na.rm = TRUE), 
  #                  number_fish = sum(number_fish, na.rm = TRUE))
  
  if (is.numeric(catch_haul_cruises0$species_code)) {
    temp1 <- temp1 %>%
      dplyr::filter(species_code < 99991)
  }
  
  z <-  temp1 %>% 
    tidyr::complete(species_code, 
                    nesting(SRVY, cruise, haul, #vessel, 
                            year, hauljoin, stratum, stationid, 
                            distance_fished, net_width)) %>%
    dplyr::select(SRVY, cruise, hauljoin, haul, #vessel, 
                  year, species_code, weight, number_fish, stratum, 
                  stationid, distance_fished, net_width) %>%
    tidyr::replace_na(list(weight = 0, number_fish = 0))
  
  
  catch_with_zeros <- 
    dplyr::full_join(x = temp1, 
                     y = z, 
                     by = c("SRVY", "cruise", "hauljoin", "haul", 
                            "year", "species_code", "stratum", "stationid", 
                            "distance_fished", "net_width")) %>%
    dplyr::select(-weight.y, -number_fish.y, -gear_depth, 
                  -duration, -net_height) %>%
    dplyr::arrange(year, haul, species_code) %>%
    dplyr::rename(weight_kg = weight.x, number_fish = number_fish.x) %>%
    tidyr::replace_na(list(weight_kg = 0, number_fish = 0))
  
  # *** *** calculate CPUE (mean CPUE by strata) ----------------------------------------------------------
  
  # num <- temp1 %>%
  #   dplyr::distinct(SRVY, year, hauljoin, species_code) %>%
  #   dplyr::group_by(SRVY, year, species_code) %>%
  #   dplyr::summarize(num = n())
  
  cpue_by_stratum <- catch_with_zeros %>%
    dplyr::select(SRVY, species_code, year, stratum, stationid,
                  distance_fished, net_width, weight_kg) %>%
    dplyr::mutate(
      effort = distance_fished * net_width/10,
      cpue_kgha = weight_kg/effort) %>% 
    dplyr::left_join(x = .,
                     y = stratum_info %>%
                       dplyr::select(stratum, area, SRVY),
                     by = c("SRVY", "stratum")) %>%
    dplyr::arrange(stratum, species_code) %>%
    dplyr::group_by(SRVY, species_code, year, stratum, area) %>%
    dplyr::summarise( 
      cpue_kgha_strat = mean(cpue_kgha, na.rm = TRUE), #weight_kg/effort, 
      cpue_kgha_var = ifelse(n() <= 1, 0, var(cpue_kgha)/n()),
      num_hauls = n(),     # num_hauls = ifelse(num == 1, 1, (num-1)),
      total_area = sum(unique(area))) %>%
    dplyr::mutate(strata = dplyr::case_when(
      (stratum == 31 | stratum == 32) ~ 30,
      (stratum == 41 | stratum == 42) | stratum == 43 ~ 40,
      (stratum == 61 | stratum == 62) ~ 60, 
      TRUE ~ as.numeric(stratum)))
  
  
  # *** biomass -----------------------------------------------------------------
  
  # ## CANNOT use biomass_*** tables bc they don't contain the info for all species (ie: no poachers, blennies, lumpsuckers, eelpouts, etc.)
  
  biomass_by_stratum <- biomass_cpue_by_stratum <- cpue_by_stratum %>%
    dplyr::mutate(
      biomass_mt = cpue_kgha_strat * (area * 0.1), 
      bio_var = (area^2 * cpue_kgha_var/100), 
      fi = area * (area - num_hauls)/num_hauls,
      ci = qt(p = 0.025, df = num_hauls - 1, lower.tail = F) * sqrt(bio_var), 
      up_ci_bio = biomass_mt + ci,
      low_ci_bio = ifelse(biomass_mt - ci <0, 0, biomass_mt - ci) )
  
  
  total_biomass <- biomass_by_stratum %>%
    dplyr::filter((species_code >= 40000 &
                     species_code < 99991) |
                    (species_code > 1 & 
                       species_code < 35000)) %>% 
    ungroup() %>%
    dplyr::group_by(SRVY, year) %>% 
    dplyr::summarise(total = sum(biomass_mt, na.rm = TRUE))
  
  return(list("biomass_cpue_by_stratum" = biomass_cpue_by_stratum, 
              "total_biomass" = total_biomass))
  
}

a <- calc_cpue_bio(catch_haul_cruises0 = catch_haul_cruises_maxyr)

biomass_cpue_by_stratum <- cpue_by_stratum <- biomass_by_stratum <- 
  a$biomass_cpue_by_stratum %>%  # remove crab totals, as they use different stratum
    dplyr::filter(!(species_code %in% c(69323, 69322, 68580, 68560)))

# subtract our-calculated crab totals so we can add the right total from SAP
cc <- a$biomass_cpue_by_stratum %>% 
  dplyr::filter((species_code %in% c(69323, 69322, 68580, 68560))) %>%
  ungroup() %>%
  dplyr::group_by(SRVY, year) %>%
  dplyr::summarise(total_crab_wrong = sum(biomass_mt, na.rm = TRUE))

total_biomass <- 
  dplyr::left_join(x = a$total_biomass, 
                   y = cc) %>% 
  dplyr::left_join(x = ., 
                   y = biomass_tot_crab %>% 
                     dplyr::filter(stratum == 999) %>%
                     ungroup() %>%
                     dplyr::group_by(SRVY, year) %>% 
                     dplyr::summarise(total_crab_correct = sum(biomass, na.rm = TRUE))) %>% 
  dplyr::mutate(total = total - total_crab_wrong + total_crab_correct) %>% 
  dplyr::select(-total_crab_wrong, -total_crab_correct)

# Cite all papers in report ----------------------------------------------------
print("Cite all papers in report")

files0<-list.files(path = paste0(dir.output, Sys.Date(), "/", maxyr, "/rawdata/"), pattern = ".docx", full.names = TRUE)
files1 <- files0

files0<-list.files(path = dir_code, pattern = ".Rmd", full.names = TRUE)
files0<-files0[grep(pattern = "[0-9]+_", x = files0)]
files0<-files0[!grepl(pattern = "presentation", x = files0)]
files1 <- c(files1, files0)

ee <- c(ref_compareyr, paste0("@NPFMC", maxyr))

for (ii in 1:length(files1)){
  if (grepl(pattern = ".docx", x = files1[ii], fixed = TRUE)) {
    aa <- readtext(file = files1[ii])$text
  } else {
    aa <- readLines(con = files1[ii], warn = FALSE)
  }
  bb <- unlist(strsplit(x = aa, split = " ", fixed = TRUE))
  bb <- unlist(strsplit(x = bb, split = "\n", perl = TRUE))
  cc <- dd <- bb[grep(pattern = "@", x = bb)]
  if (length(dd) != 0) {
    remove <- c(";", "[", "]", ".", ",", ")", "(", '"') # , "\\" 
    for (i in 1:length(remove)){
      dd <- gsub(pattern = remove[i], replacement = "", x = dd, fixed = TRUE)
    }
  }
  ee <- c(ee, dd)
}

ee <- unique(ee)
ee <- ee[ee != "'@*'"]
ee <- ee[substr(x = ee, start = 1, stop = 1)=="@"]
ee <- sapply( strsplit(x = ee, split = "\\\n", perl = TRUE),"[[",1)


bib <- readLines(con = "./cite/bibliography.bib", warn = FALSE)
bib <- paste0(bib, collapse = "\n")
bib <- unlist(strsplit(x = bib, split = "@"))

ff <- c()
for (i in 1:length(ee)) {
  ff <- c(ff, 
          grep(pattern = gsub(pattern = "@", replacement = "", x = ee[i], fixed = TRUE), 
               x = bib, ignore.case = TRUE) )
}
bib <- paste0("@", bib[ff], collapse = "\n")

utils::write.table(x = bib,
                   file = paste0(dir_out_cite, "bib_report.bib"),
                   row.names = FALSE,
                   col.names = FALSE,
                   quote = FALSE)

# str0 <- paste0(ee, collapse = ", ")

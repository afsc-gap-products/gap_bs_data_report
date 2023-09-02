#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Wrangle data
#' start date: 2021-03-03
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---

# Report types ---------------------------------------------------

# When you select a region using get_base_layers(), the grid will be clipped to only include stations in the survey region.  
# I haven't added NBS functionality to get_base_layers() since we do both surveys in the same year, but there is an easy workaround (third block of code below).

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
# nbs_grid <- full_ebs_layers$survey.grid %>% filter(station %in% akgfmaps::get_survey_stations(select.region = "nbs"))
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
               "999", "99900"),
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.south", 
      set.crs = out.crs)
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
               "999", "99902"), 
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.north", 
      set.crs = out.crs)
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
               "999", "99900", "99902"), 
    reg_dat = akgfmaps::get_base_layers(
      select.region = "bs.all", 
      set.crs = out.crs)
  )
)


for (ii in 1:length(report_types)) {
  reg_dat <- report_types[[ii]]$reg_dat
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
  reg_dat$survey.grid$station <- reg_dat$survey.grid$STATIONID
  # fix reg_dat$graticule
  reg_dat$graticule <- sf::st_transform(x = reg_dat$graticule, crs = CRS(as.character(reg_dat$crs)[1]))
  report_types[[ii]]$reg_dat <- reg_dat
}

a <- report_types[names(report_types) == SRVY][[1]]
for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }

plural_surveys <- ifelse(length(SRVY1) > 1, "s", "")

# Load data --------------------------------------------------------------------

print("Load data")

## Load Documents from Google Drive --------------------------------------------

# ssl_drive_id <- "1gJbb2qoqXMPFGwuk65b1HsazoMptLU6j"
# as_dribble(as_id(ssl_drive_id)) %>% 
#   drive_ls() %>% 
#   group_nest(row_number()) %>% 
#   pull(data) %>% 
#   walk(~ drive_download(.x$id, path = here::here("spatial_data", "shp_files", .x$name)))


id_googledrive <- googledrive::as_id(dir_googledrive)

if (access_to_internet ) {
  
  # Species Covered
  # https://docs.google.com/spreadsheets/d/10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M/edit?usp=sharing
  googledrive::drive_download(file = googledrive::as_id("10Pn3fWkB-Jjcsz4iG7UlR-LXbIVYofy1yHhKkYZhv2M"),
                              type = "csv",
                              overwrite = TRUE,
                              path = paste0(dir_out_rawdata, "/0_species_local_names"))
  
  # Spreadsheets
  # https://drive.google.com/drive/folders/1Vbe_mH5tlnE6eheuiSVAFEnsTJvdQGD_?usp=sharing
  a <- googledrive::drive_ls(path = googledrive::as_id("1Vbe_mH5tlnE6eheuiSVAFEnsTJvdQGD_"), 
                             type = "spreadsheet")
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
                                type = "txt",
                                overwrite = TRUE, 
                                path = paste0(dir_out_rawdata, "/", a$name[i], ".txt"))
  }
  
}


# Make google doc txts into .rmds
txtfiles <- list.files(path = paste0(dir_out_rawdata, "/"), pattern = ".txt")

for (i in 1:length(txtfiles)) {
  print(txtfiles[i])
  
  # get rid of comments
  a <- readLines(con = paste0(dir_out_rawdata, txtfiles[i]), warn = FALSE)
  comment_id <-sapply(X = strsplit(x = a[which(lapply(a, FUN = substr, start = 1, stop = 1) == "[")], 
                                   split = "]", 
                                   fixed = TRUE),"[[",1)
  if (length(comment_id) > 0) {
    comment_id <- paste0(comment_id, "]")
    a <- a[which(lapply(a, FUN = substr, start = 1, stop = 1) != "[")]
    a <- a[which(lapply(a, FUN = substr, start = 1, stop = 1) != "_")]
    for (ii in 1:length(comment_id)){
      a <- gsub(pattern = comment_id[ii], replacement = "", x = a, fixed = TRUE)
    }
  }
  write.table(x = a, 
              file = paste0(dir_out_rawdata, txtfiles[i]), 
              quote = FALSE, 
              row.names = FALSE, 
              col.names = FALSE, 
              append = FALSE, )
  
  pandoc_convert(input = paste0(dir_out_rawdata, paste(txtfiles[i])),
                 to = "markdown",
                 output = paste0(dir_out_rawdata, gsub(txtfiles[i], pattern = ".txt", replacement = ".Rmd")),
                 citeproc = TRUE) # not sure if this is needed
}

## Load Main Oracle Data -------------------------------------------------------

a <- paste0(dir_data, "oracle/", c("gap_products_old_station", 
                                   "crab_ebscrab", "crab_ebscrab_nbs", 
                                   "crab_gap_ebs_nbs_crab_cpue",
                                   "race_data_length_types",
                                   # "catch", "haul", 
                                   # "species", "species_classification", "specimen", 
                                   # "stratum", "v_cruises", 
                                   # "v_extract_final_lengths", 
                                   "race_data_vessels"
), 
".csv") 

a <- c(a, list.files(path = here::here("data/oracle/"), 
                     pattern = "gap_products_akfin", full.names = TRUE))

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

# Wrangle Data -----------------------------------------------------------------

## catch -----------------------------------------------------------------------
print("catch")

# ## there should only be one species_code observation per haul event, however
# ## there are occasionally multiple (with unique catchjoins). 
# ## I suspect that this is because a species_code was updated or changed, 
# ## so we will need to sum those counts and weights

catch <- gap_products_akfin_catch0 %>% 
  dplyr::filter(hauljoin %in% unique(gap_products_akfin_haul0$hauljoin)) %>%
  dplyr::group_by(cruisejoin, hauljoin, species_code) %>% # , vessel, haul
  dplyr::summarise(weight_kg = sum(weight_kg, na.rm = TRUE), 
                   count = sum(count, na.rm = TRUE)) %>% 
  dplyr::ungroup()

## spp_info --------------------------------------------------------------------

print("report_spp and spp_info")

spp_info <- gap_products_akfin_taxonomics_worms0 %>%
  dplyr::rename(species_name = accepted_name) %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert"),
    species = ifelse(id_rank == "species", 
                     stringr::str_replace(pattern = paste0(genus, " "), 
                                          string = species_name, 
                                          replacement = ""), 
                     ""),
    used_in_counts = 
      dplyr::if_else(
        species_code %in% #10:99988, 
          find_codes(x = .,  
                     str_not = c(" shells", "empty", "unsorted", "shab"), #, " egg", "unid.", "compound"
                     col_str_not = "common_name",
                     col_out = "species_code"), 
        TRUE, FALSE)) # remove " shells", "empty", "unsorted", "shab". May also consider removing " egg", "unid.", "compound"

spp_info_maxyr <- spp_info %>%
  dplyr::filter(species_code %in%
                  unique(catch$species_code))

## report_spp ------------------------------------------------------------------

print("report_spp")

report_spp <- readr::read_csv(file = paste0(dir_out_rawdata, "/0_species_local_names.csv"), 
                              skip = 1, show_col_types = FALSE) %>% 
  dplyr::select(!(dplyr::starts_with(ifelse(report_title == "community", "datar_", "community_")))) %>%
  dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert"))

names(report_spp)[
  grepl(pattern = ifelse(report_title == "community", "community_", "datar_"), 
        x = names(report_spp))] <- 
  gsub(pattern = ifelse(report_title == "community", "community_", "datar_"), 
       replacement = "", 
       x = names(report_spp)[
         grepl(pattern = ifelse(report_title == "community", "community_", "datar_"), 
               x = names(report_spp))])

## report_spp1 ------------------------------------------------------------------

temp1 <- data.frame()
for (i in 1:nrow(report_spp)){
  temp2 <- eval(expr = parse(text = report_spp$species_code[i]))
  temp1 <- dplyr::bind_rows(temp1, 
                           dplyr::bind_cols(report_spp[i,], 
                                            species_code1 = eval(expr = parse(text = report_spp$species_code[i]))))
}

temp1 <- temp1 %>% 
  dplyr::select(print_name, common_name1, species_code1, group, group_sci, taxon, order, file_name, 
                plot_sizecomp, plot_idw, text_spp, table_cpue, table_bio_portion, table_bio_spp, cpue) %>% 
  dplyr::filter(print_name != "") %>% 
  dplyr::mutate(group0 = group)

# all -> other
temp <- unique(temp1$print_name)[grepl(pattern = "all ", 
                                       x = unique(temp1$print_name), 
                                       ignore.case = TRUE)]
if (length(temp)>0) {
  for (i in 1:length(temp)) {
    temp2 <- intersect(temp1$species_code[temp1$print_name == temp[i]], 
                       temp1$species_code[temp1$print_name != temp[i]]) # find which are duplicates 
    if (length(temp2)>0) {
      # and delete them from "all "
      temp1 <- temp1[!(temp1$species_code %in% temp2 &
                         temp1$print_name == temp[i]),]
      # and change "all " to "other "
      temp1$print_name[temp1$print_name == temp[i]] <- 
        gsub(pattern = "all ", 
             replacement = "other ", 
             x = temp1$print_name[temp1$print_name == temp[i]], 
             ignore.case = TRUE)
    }
  }
}
# if (sum(temp1$species_code[(duplicated(temp1$species_code))])>0) warning("There are still duplicates in the species split ups!")

report_spp1 <-  
  dplyr::left_join(x = temp1 %>% 
                     dplyr::rename(species_code = species_code1),# %>% 
                     # dplyr::select(-col), 
                   y = spp_info %>% 
                     dplyr::select(species_code, genus, species) %>% 
                     unique(), 
                   by = "species_code")  %>% 
  dplyr::mutate(temp = trimws(gsub(pattern = "NA", 
                                   replacement = "", 
                                   paste0(genus, " ", species))), 
                temp = ifelse(temp == " ", "", temp), 
                species_name = dplyr::case_when(
    group_sci == "BLANK" ~ "",
    !is.na(group_sci) ~ group_sci, 
    is.na(group_sci) ~ temp, 
    TRUE ~ "other")) %>%
  dplyr::mutate(temp = trimws(paste0(group_sci, " (", print_name, ")"))) %>%
  dplyr::mutate(temp = ifelse(temp == "NA ", "", temp)) %>%
  dplyr::mutate(group = dplyr::case_when(
    is.na(group_sci) ~ temp, 
    TRUE ~ "other")) %>%
  dplyr::distinct() %>% 
  dplyr::mutate(type = ifelse(
    grepl(pattern = " ", x = species_name, fixed = TRUE),
    "ital", NA)) %>%
  dplyr::ungroup() %>% 
  dplyr::mutate(species_name0 = species_name, 
                species_name1 = species_name, 
                species_name0 = dplyr::if_else(is.na(type == "ital"), species_name0, paste0("*", species_name0, "*")), 
                species_name0 = gsub(pattern = " spp.*", replacement = "* spp.", x = species_name0, fixed = TRUE), 
                species_name0 = gsub(pattern = " sp.*", replacement = "* sp.", x = species_name0, fixed = TRUE), 
                species_name = species_name0) %>% 
  dplyr::select(-type, -temp, -species_name0, -genus, -species)

## cruises + maxyr + compareyr -------------------------------------------------
print("cruises + maxyr  + compareyr")

cruises <- gap_products_akfin_cruises0 %>% 
  dplyr::select(cruise, year, survey_name, vessel_id, survey_definition_id, 
                vessel_name, date_start, date_end, cruisejoin) %>% 
  dplyr::filter(year != 2020 & # no surveys happened this year that I care about
                  year >= 1982 &
                  year <= maxyr &
                  survey_definition_id %in% SRVY00) %>% 
  dplyr::mutate(vess_shape = substr(x = vessel_name, 1,1), 
                vessel_ital = paste0("FV *", stringr::str_to_title(vessel_name), "*"), 
                vessel_name = paste0("FV ", stringr::str_to_title(vessel_name))) %>%
  dplyr::left_join(
    y = data.frame(survey_definition_id = c(143, 98, 47), 
                   SRVY = c("NBS", "EBS", "GOA"), 
                   SRVY_long = c("northern Bering Sea", 
                                 "eastern Bering Sea", 
                                 "Gulf of Alaska"), 
                   SRVY_start = c(2010, 1982, NA)), 
    by  = "survey_definition_id") %>% 
  dplyr::rename(start_date_cruise = date_start, 
                end_date_cruise = date_end)

cruises_maxyr <- cruises %>%
  dplyr::filter(year == maxyr & 
                  survey_definition_id %in% SRVY00)

cruises_compareyr <- cruises %>%
  dplyr::filter(year == compareyr[1] & 
                  survey_definition_id %in% SRVY00)

## haul + maxyr ----------------------------------------------------------------
print("haul + maxyr  + compareyr")

haul <- dplyr::left_join(
  y = gap_products_akfin_haul0, 
  x = cruises %>% 
    dplyr::select(cruisejoin, survey_definition_id), 
  by = "cruisejoin") %>%  
  dplyr::mutate(year = as.numeric(format(as.Date(date_time_start, 
                                                 format="%m/%d/%Y"),"%Y")), 
                SRVY = dplyr::case_when(
                  survey_definition_id %in% 143 ~ "NBS",
                  survey_definition_id %in% 98 ~ "EBS" )) %>%
  dplyr::filter(year <= maxyr &
                  year > 1981 & # abundance_haul == "Y" & 
                performance >= 0 &
                  !(is.null(station)) &
                  survey_definition_id %in% SRVY00 & 
                  haul_type == 3) 

haul_maxyr <- haul %>% 
  dplyr::filter(year == maxyr)

haul_compareyr <- haul %>% 
  dplyr::filter(year == compareyr[1])

## other var (survey additions, *yrs, etc. -------------------------------------

print("define other vars")

# # Crab retows?
crab_resample <- FALSE
# if (sum(unique(temp$haul_type[temp$year == maxyr]) %in% 17) >0) {
#   crab_resample <- TRUE
#   haul_maxyr_crabretow <- haul0 %>%
#     dplyr::filter(grepl(pattern = maxyr, x = cruise)) %>% 
#     dplyr::filter(haul_type == 17) # crab retow == 17
# }
# 
# # 15/30
# tow1530 <- FALSE
# if (sum(unique(temp$haul_type[temp$year == maxyr]) %in% 20) >0) {
#   tow1530 <- TRUE
#   haul_maxyr_tow1530 <- haul0 %>%
#     dplyr::filter(grepl(pattern = maxyr, x = cruise)) %>% 
#     dplyr::filter(haul_type == 20) 
# }

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

## CPUE Design Based Estimates -------------------------------------------------

print("CPUE Design Based Estimates")

cpue <- dplyr::bind_rows( 
  gap_products_akfin_cpue0 %>% # groundfish data (sans crab)
    dplyr::filter(!(species_code %in% c(69322, 69323, 68580))) %>%
    dplyr::select(species_code, hauljoin, cpue_kgkm2, cpue_nokm2), 
  crab_gap_ebs_nbs_crab_cpue0 %>%  # crab data
    dplyr::filter(survey_year > 1981) %>%
    dplyr::select(cpue_kgkm2 = cpuewgt_total, 
                  cpue_nokm2 = cpuenum_total) ) %>% 
  dplyr::left_join(y = haul) %>% 
  dplyr::left_join(y = spp_info %>% 
                     dplyr::select(species_code, common_name, species_name)) %>% 
  dplyr::select(year, hauljoin, 
                vessel_id, SRVY,  stratum, station, 
                species_code, longitude_dd_start, latitude_dd_start, 
                cpue_nokm2, cpue_kgkm2, common_name) %>% 
  dplyr::filter(!(year < 1996 & species_code == 10261) & # modify for spp
                  !(year < 2000 & species_code == 435 ) &
                  !(year < 2000 & species_code == 471) ) %>% # 2022/10/28 - Duane - Alaska skate abundance/distribution figures should include only data from 2000 and later, due to earlier identification issues (which are clearly indicated in the plots).
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish",
    species_code >= 40001 ~ "invert"))

# cpue$common_name[cpue$species_name == "Neptunea heros"] <- "northern neptune whelk"

cpue_maxyr <- cpue %>%
  dplyr::filter(year == maxyr)

cpue_compareyr<- cpue %>%
  dplyr::filter(year == compareyr[1])

## station ---------------------------------------------------------------------

print("station")

station <- haul %>%  
  dplyr::filter(year == maxyr) %>%
  dplyr::select(station, stratum, latitude_dd_start, longitude_dd_start) %>% 
  dplyr::group_by(station, stratum) %>% 
  dplyr::summarise(latitude_dd_start = mean(latitude_dd_start, na.rm = TRUE), 
                   longitude_dd_start = mean(longitude_dd_start, na.rm = TRUE)) %>% 
  dplyr::ungroup() %>%
  dplyr::left_join(y = gap_products_old_station0 %>%
                     dplyr::filter(srvy %in% SRVY1 &
                                     design_year == max(design_year, na.rm = TRUE)) %>%
                     dplyr::select(station, stratum, SRVY = srvy) %>%
                     dplyr::mutate(
                       stratum = as.numeric(stratum), 
                       survey_definition_id = dplyr::case_when(
                       SRVY == "NBS" ~ 143, 
                       SRVY == "EBS" ~ 98 ) ), 
                   by = c("stratum", "station")) %>% 
  dplyr::mutate(in_maxyr = (station %in% haul_maxyr$station), 
                reg = dplyr::case_when(
                  station %in% c("CC-04", "CC-05", "CC-06", "CC-07", "CC-08", "CC-09", 
                                 "CC-10", "BB-04", "BB-05", "BB-06", "BB-07", "BB-08", 
                                 "BB-09", "BB-10", "AA-04", "AA-05", "AA-06", "AA-07", 
                                 "AA-08", "AA-10", "ZZ-04", "ZZ-05", "Y-04") ~ "Norton Sound"))

## stratum (survey area) -------------------------------------------------------

print("stratum")

stratum <- gap_products_akfin_area0 %>% 
  dplyr::filter(
    survey_definition_id %in% SRVY00 &
      type == "STRATUM" &
      design_year == strat_yr) %>%  
  dplyr::mutate(
    SRVY = dplyr::case_when(
      survey_definition_id == 143 ~ "NBS",
      survey_definition_id == 98 ~ "EBS" ),
    depth = ifelse(depth_min_m == 1, 
                   paste0(">", depth_max_m), 
                   paste0(depth_min_m, "-", depth_max_m)) ) %>% 
  dplyr::rename(stratum = area_id) %>% 
  dplyr::left_join(
    y = haul_maxyr %>% 
      dplyr::distinct(stratum, station) %>% 
      dplyr::select(stratum, station) %>% 
      dplyr::group_by(stratum) %>% 
      dplyr::summarise(stations_completed = length(unique(station))) %>% 
      dplyr::select(stratum, stations_completed), 
    by = "stratum") %>% 
  dplyr::left_join(
    y = station %>% 
      dplyr::select(stratum, station) %>% 
      dplyr::group_by(stratum) %>% 
      dplyr::summarise(stations_avail = length(unique(station))) %>% 
      dplyr::select(stratum, stations_avail), 
    by = "stratum") #%>% 
# dplyr::left_join(
#   y = haul_maxyr %>% 
#     dplyr::select(stratum, station, bottom_depth) %>% 
#     dplyr::group_by(stratum) %>% 
#     dplyr::summarise(depth_mean = mean(bottom_depth, na.rm = TRUE), 
#                      depth_min = min(bottom_depth, na.rm = TRUE), 
#                      depth_max = max(bottom_depth, na.rm = TRUE)
#                      ), 
#   by = "stratum") 


## haul_cruises_vess_ + _maxyr + _compareyr ------------------------------------

print("haul_cruises_vess_ + _maxyr + _compareyr")

haul_cruises_vess <- 
  dplyr::left_join(x = cruises,
                   y = haul %>% 
                     dplyr::select(cruisejoin, hauljoin, station, stratum, haul, 
                                   depth_gear_m, duration_hr, distance_fished_km, 
                                   net_width_m, net_height_m, date_time_start) %>% 
                     dplyr::group_by(cruisejoin, hauljoin, station, stratum, haul, 
                                     depth_gear_m, duration_hr, distance_fished_km, 
                                     net_width_m, net_height_m) %>% 
                     dplyr::summarise(start_date_haul = min(date_time_start),
                                      end_date_haul = max(date_time_start), 
                                      stations_completed = length(unique(station))),
                   by = c("cruisejoin")) %>% 
  dplyr::left_join(y = race_data_vessels0 %>%
                     dplyr::select(vessel_id, length, tonnage), 
                   by = "vessel_id") %>% 
  dplyr::rename(length_ft = length) %>% 
  dplyr::mutate(length_m = round(length_ft/3.28084, 
                                 digits = 1)) %>% 
  dplyr::ungroup() 

haul_cruises_vess_maxyr <- haul_cruises_vess %>% # temp(cruises_ = cruises_maxyr, haul_ = haul_maxyr) 
  dplyr::filter(year(haul_cruises_vess$start_date_haul) == maxyr)

haul_cruises_vess_compareyr <- haul_cruises_vess %>% # temp(cruises_compareyr, haul_compareyr) 
  dplyr::filter(year(haul_cruises_vess$start_date_haul) == compareyr)

## vessels ----------------------------------------------------------------------

print("vessels")

vessels <-  haul_cruises_vess_maxyr %>% 
  dplyr::select("vessel_name", "vessel_ital", "vessel_id", "tonnage",
                "length_m", "length_ft", "vess_shape") %>% 
  unique() %>% 
  dplyr::mutate(img = dplyr::case_when(
    vessel_id == 94 ~ "94_vesteraalen.png", 
    vessel_id == 134 ~ "134_northwestexplorer.png", 
    vessel_id == 162 ~ "163_alaskaknight.png")) %>% 
  dplyr::arrange(vessel_name)

## haul_cruises + _maxyr + _compareyr ------------------------------------------

print("haul_cruises + _maxyr + _compareyr")

haul_cruises <- 
  dplyr::left_join(
    x = haul_cruises_vess %>% 
      dplyr::select(year, survey_name, cruise, SRVY_start, 
                    survey_definition_id, SRVY, SRVY_long, 
                    cruisejoin) %>%
      unique(), 
    y = haul_cruises_vess %>% 
      dplyr::select("cruise", "stations_completed", 
                    start_date_haul, end_date_haul, 
                    start_date_cruise, end_date_cruise) %>% 
      dplyr::group_by(cruise) %>% 
      dplyr::summarise(stations_completed = sum(stations_completed), 
                       start_date_haul = min(start_date_haul), 
                       end_date_haul = max(end_date_haul), 
                       start_date_cruise = min(start_date_cruise), 
                       end_date_cruise = max(end_date_cruise)) %>% 
      dplyr::mutate(start_mo_long = month(start_date_haul, label = TRUE, abbr = FALSE), 
                    end_mo_long = month(end_date_haul, label = TRUE, abbr = FALSE)),
    by = "cruise") %>% 
  dplyr::left_join(
    x = ., 
    y = station %>% 
      dplyr::group_by(survey_definition_id) %>% 
      dplyr::summarise(stations_avail = length(unique(station))),
    by = "survey_definition_id") %>% 
  dplyr::left_join(
    x = ., 
    y = cruises %>% 
      dplyr::select(year, SRVY) %>%
      unique() %>%
      dplyr::count(vars = SRVY) %>%
      dplyr::rename(yrofsurvey = n, 
                    SRVY = vars), 
    by = "SRVY") %>% 
  dplyr::select(-cruisejoin, -cruise) %>%
  dplyr::mutate(stndth = stndth(yrofsurvey))  %>% 
  dplyr::arrange(SRVY) %>% 
  dplyr::mutate(compareyr = compareyr[1]) %>%
  dplyr::left_join(
    x = ., 
    y = data.frame(
      SRVY = SRVY1,
      compareyr_ref = ref_compareyr), 
    by = "SRVY") %>%
  unique()

haul_cruises_maxyr <- haul_cruises %>% 
  dplyr::filter(year == maxyr)

haul_cruises_compareyr <- haul_cruises %>% 
  dplyr::filter(year == compareyr)

## catch_haul_cruises_maxyr + maxyr-1-------------------------------------------

print("catch_haul_cruises + _maxyr + _compareyr")

catch_haul_cruises <- 
  dplyr::left_join(
    x = haul %>% 
      dplyr::select(cruisejoin, hauljoin, station, stratum, #haul, 
                    date_time_start, 
                    latitude_dd_start, longitude_dd_start, 
                    latitude_dd_end, longitude_dd_start, 
                    depth_gear_m, depth_m, 
                    bottom_temperature_c = gear_temperature_c, surface_temperature_c,
                    performance, 
                    duration_hr, distance_fished_km, net_width_m, net_measured, net_height_m), 
    y = cruises %>% 
      dplyr::select(cruisejoin, survey_name, SRVY, year, cruise),  
    by = c("cruisejoin")) %>% 
  dplyr::left_join(
    y = catch %>% 
      dplyr::select(cruisejoin, hauljoin, species_code, weight_kg, count), 
    by = c("hauljoin", "cruisejoin"))

catch_haul_cruises_maxyr <- catch_haul_cruises  %>% 
  dplyr::filter(year == maxyr)

catch_haul_cruises_compareyr <- catch_haul_cruises  %>% 
  dplyr::filter(year == compareyr)

## length_type -----------------------------------------------------------------

print("length_type")

length_type <- race_data_length_types0
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

## length ----------------------------------------------------------------------

print("lengths")

lengths <- dplyr::bind_rows(
  gap_products_akfin_length0 %>% # GAP lengths
    dplyr::filter(hauljoin %in% unique(haul$hauljoin)) %>%
    dplyr::mutate(sex_code = sex, 
                  sex = dplyr::case_when(
                    sex_code == 1 ~ "Males", 
                    sex_code == 2 ~ "Females", 
                    sex_code == 3 ~ "Unsexed"), 
                  taxon = dplyr::case_when(
                    species_code <= 31550 ~ "fish",
                    species_code >= 40001 ~ "invert") ) %>%
    dplyr::left_join(
      y = haul %>% # should already exclude special project and bad tows
        dplyr::select(hauljoin, station, survey_definition_id, SRVY, year) %>%
        dplyr::distinct(),
      by = c("hauljoin")), 
  dplyr::bind_rows( # SAP lengths
    crab_ebscrab0 %>%
      dplyr::filter(!(cruise %in% unique(crab_ebscrab_nbs0$cruise))) %>% # there may be some nbs data in the ebs (201002)
      dplyr::mutate(SRVY = "EBS"),
    crab_ebscrab_nbs0 %>%
      dplyr::mutate(SRVY = "NBS") ) %>% 
    dplyr::left_join(
      y = haul %>% 
        dplyr::select(SRVY, hauljoin,haul_type, station, performance), # abundance_haul, 
      by = c("SRVY","hauljoin")) %>% 
    dplyr::filter(
      # abundance_haul == "Y" &
        performance >= 0 &
        haul_type %in% c(3)) %>% # standard stations # Note: 17 = resample stations
    dplyr::mutate(year = as.numeric(substr(cruise, start = 1, stop = 4))) %>% 
    dplyr::mutate(sex_code = sex, 
                  sex = dplyr::case_when(
                    sex == 1 ~ "males",
                    sex == 0 ~ "unsexed",
                    (clutch_size == 0 & sex == 2) ~ "immature females", 
                    (clutch_size >= 1 & sex == 2) ~ "mature females"), 
                  length = dplyr::case_when(
                    species_code %in% c(68580, 68590, 68560) ~ width, # "snow crab"
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
        TRUE ~ 7  ) )  %>% # 7 - Length of carapace from back of right eye socket to end of carapace # species_code %in% c(69322, 69323, 69400, 69401) ~ 7, 
    dplyr::ungroup() ) %>% 
  dplyr::left_join(y = spp_info %>% 
                     dplyr::select(species_code, common_name, species_name),
                   by = "species_code") %>% 
  dplyr::ungroup() %>% 
  dplyr::left_join(y = length_type, # Add in length type data 
                   by = c("length_type" = "length_type_id"))

lengths_maxyr <- lengths %>% 
  dplyr::filter(year == maxyr) 

## Specimen_maxyr-------------------------------------------------------------

print("Specimen + maxyr")

specimen_maxyr <- 
  dplyr::left_join(
    x = haul_maxyr %>% 
      dplyr::select(cruisejoin, hauljoin, station, stratum), 
    y = cruises_maxyr %>% 
      dplyr::select(cruisejoin, survey_name, SRVY),  
    by = c("cruisejoin")) %>% 
  dplyr::left_join(
    x= ., 
    y = gap_products_akfin_specimen0, 
    by = c("cruisejoin", "hauljoin")) %>% 
  dplyr::select(-region, cruisejoin, hauljoin)

## Bottom temperatures ---------------------------------------------------------

print("bottom tempertures")

temps_avg_yr <- coldpool:::cold_pool_index %>% 
  dplyr::select(YEAR, 
                bt = MEAN_GEAR_TEMPERATURE, 
                st = MEAN_SURFACE_TEMPERATURE) %>% 
  dplyr::filter(YEAR <= maxyr) %>% 
  janitor::clean_names() %>% 
  dplyr::arrange(desc(bt)) %>%
  dplyr::mutate(warmest_rank = 1:nrow(.), bt_mean = mean(bt, na.rm = TRUE), 
                st_mean = mean(st, na.rm = TRUE), 
                SRVY = "EBS", 
                bt_zscore = (bt-bt_mean)/sd(bt, na.rm = TRUE), 
                st_zscore = (st-st_mean)/sd(st, na.rm = TRUE), 
                bt_notsig = (bt_zscore > -1 & bt_zscore < 1), 
                st_notsig = (st_zscore > -1 & st_zscore < 1), 
                bt_above_mean = bt>bt_mean, 
                st_above_mean = st>st_mean, 
                bt_case = dplyr::case_when(
                  bt_notsig ~ "average", 
                  (!bt_notsig & bt_above_mean) ~ "warmer", 
                  (!bt_notsig & !bt_above_mean) ~ "colder"), 
                st_case = dplyr::case_when(
                  st_notsig ~ "average", 
                  (!st_notsig & st_above_mean) ~ "warmer", 
                  (!st_notsig & !st_above_mean) ~ "colder"),                   
                case = ifelse(bt_case == st_case, 
                              paste0("both ", bt_case), 
                              paste0("bt ", bt_case, ", st ", st_case)) ) %>% 
  dplyr::arrange(SRVY, year) 

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

temps_avg_yr_maxyr <- temps_avg_yr %>% 
  dplyr::filter(year == maxyr)

temps_avg_yr_abovebelow <- list(
  "above" = 
    temps_avg_yr %>% 
    dplyr::filter(SRVY == "EBS" & 
                    bt_above_mean == TRUE &
                    year >= 2006) %>% # the begining of the last cold stanza
    dplyr::ungroup() %>%
    dplyr::arrange(-year) %>% 
    dplyr::select(year) %>% 
    unlist(),
  "below" = 
    temps_avg_yr %>% 
    dplyr::filter(SRVY == "EBS" & 
                    bt_above_mean == FALSE &
                    year >= 2006) %>%  # the begining of the last cold stanza
    dplyr::ungroup() %>%
    dplyr::arrange(-year) %>% 
    dplyr::select(year) %>% 
    unlist()
)

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

## sizecomps -------------------------------------------------------------------

print("sizecomps")

# Crab 
df.ls<-list()
a<-list.files(path = paste0(dir_data, "/crab/sizecomp/"), full.names = TRUE)

for (i in 1:length(a)){
  b <- readr::read_csv(file = a[i], show_col_types = FALSE)
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  b$file <- a[i]
  df.ls[[i]]<-b
  names(df.ls)[i]<-a[i]
}

sizecomp_crab <- dplyr::bind_rows(df.ls) %>% 
  dplyr::mutate(SRVY = "NBS", 
                species_code = dplyr::case_when(
                  grepl(pattern = "_BKC_", x = file, ignore.case = TRUE) ~ 69323, # "blue king crab"
                  grepl(pattern = "_RKC_", x = file, ignore.case = TRUE) ~ 69322, # "red king crab"
                  grepl(pattern = "_CO_", x = file, ignore.case = TRUE) ~ 68580 # "snow crab"
                )) %>%
  dplyr::rename( 
    year = survey_year,
    length_mm = size1) %>%
  dplyr::select(length_mm, year, species_code, SRVY, 
                num_unsexed_size1, num_male_size1, num_female_size1_mat, num_female_size1_immat) %>%
  tidyr::pivot_longer(cols = c(num_unsexed_size1, num_male_size1, num_female_size1_mat, num_female_size1_immat),
                      names_to = "sex", values_to = "population_count")  %>%
  dplyr::group_by(sex, length_mm, year, species_code, SRVY) %>% 
  dplyr::summarise(population_count = sum(population_count, na.rm = TRUE)) %>%
  dplyr::mutate(
    sex = dplyr::case_when(
      sex == "num_unsexed_size1" ~ "unsexed",
      sex == "num_male_size1" ~ "males",
      sex == "num_female_size1_immat" ~ "immature females",
      sex == "num_female_size1_mat" ~ "mature females"),
    survey_definition_id = dplyr::case_when(
      SRVY == "NBS" ~ 143, 
      SRVY == "EBS" ~ 98) )

sizecomp <- dplyr::bind_rows(
  gap_products_akfin_sizecomp0 %>% # GAP data
    dplyr::filter(
      area_id > 99900 & 
        length_mm > 0 &
        !(year < 1996 & species_code == 10261) &
        !(year < 2000 & species_code == 435) & # 2022/10/28 - Duane - Alaska skate abundance/distribution figures should include only data from 2000 and later, due to earlier identification issues (which are clearly indicated in the plots).
        !(year < 2000 & species_code == 471)) %>% 
    dplyr::mutate(
      sex = dplyr::case_when(
        sex == 1 ~ "males", 
        sex == 2 ~ "females", 
        sex == 3 ~ "unsexed"),
      SRVY = dplyr::case_when(
        survey_definition_id == 43 ~ "NBS", 
        survey_definition_id == 98 ~ "EBS")), # convert to cm
  sizecomp_crab) %>% 
  dplyr::filter(
    year <= maxyr &
      survey_definition_id %in% SRVY00 & 
      year > 1981 & 
      !is.na(length_mm) & 
      !is.na(population_count) & 
      population_count!= 0 & 
      !is.na(species_code)) %>%
  dplyr::mutate(
    taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish", 
      species_code >= 40001 ~ "invert"), 
    length_mm = length_mm/10) %>% 
  dplyr::left_join(y = gap_products_akfin_stratum_groups0 %>% 
                     dplyr::select(area_id, stratum), 
                   by = "area_id", 
                   relationship = "many-to-many")

sizecomp_maxyr <- sizecomp %>%
  dplyr::filter(year == maxyr)

sizecomp_compareyr <- sizecomp %>%
  dplyr::filter(year == compareyr[1])

## biomass ---------------------------------------------------------------------
print("biomass")

biomass <- gap_products_akfin_biomass0 %>%
  dplyr::filter(
    area_id %in% strat0 &
    n_weight > 0 & 
    year <= maxyr &
   !(species_code %in% c(69323, 69322, 68580, 68560)) &
    !is.na(species_code) & 
      !(year < 1996 & species_code == 10261) &
      !(year < 2000 & species_code == 471) & # 2022/10/28 - Duane - Alaska skate abundance/distribution figures should include only data from 2000 and later, due to earlier identification issues (which are clearly indicated in the plots).
      !(year < 2000 & species_code == 435 )) %>%  
  unique() %>%
  dplyr::mutate(
    SRVY = dplyr::case_when(
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 98 ~ "EBS"),
    stratum = ifelse(area_id > 999, 999, area_id),
    cpue_kgkm2_sd = sqrt(cpue_kgkm2_var), 
    cpue_nokm2_sd = sqrt(cpue_nokm2_var), 
    biomass_sd = sqrt(biomass_var), 
    population_sd = sqrt(population_var))

biomass_crab <- dplyr::bind_rows( # crab
  crab_ebscrab0 %>%
    dplyr::filter(!(cruise %in% unique(crab_ebscrab_nbs0$cruise))) %>% # there may be some nbs data in the ebs (201002)
    dplyr::mutate(SRVY = "EBS"),
  crab_ebscrab_nbs0 %>%
    dplyr::mutate(SRVY = "NBS") ) %>%
  dplyr::mutate(
    survey_definition_id = dplyr::case_when(
      SRVY == "NBS" ~ 143, 
      SRVY == "EBS" ~ 98), 
    length = dplyr::case_when(
    species_code %in% c(68580, 68590, 68560) ~ width,  # "snow crab"
    TRUE ~ length)) %>%
  dplyr::filter(!is.na(length)) %>% # if NA, it was not lengthed!
  dplyr::select(hauljoin, SRVY, species_code, station) %>%
  dplyr::distinct() %>%
  dplyr::left_join(
    y = haul %>%
      dplyr::select(hauljoin, stratum, year),
    by = c("hauljoin")) %>%
  dplyr::group_by(SRVY, species_code, stratum, year) %>%
  dplyr::summarise(n_length = n()) %>%
  dplyr::filter(!is.na(stratum))

biomass_crab <- dplyr::bind_rows(
  biomass_crab, 
  biomass_crab %>% 
    dplyr::group_by(SRVY, species_code, year) %>% 
    dplyr::summarise(n_length = n()) %>% 
    dplyr::mutate(stratum = 999)) # survey total

biomass_crab <- readr::read_csv(
  file = here::here("data/oracle/crab_gap_ebs_nbs_abundance_biomass.csv"), 
  show_col_types = FALSE) %>%
  janitor::clean_names() %>% 
  dplyr::rename(biomass_mt = biomass_total,
                # lowerb = biomass_lower_ci, 
                # upperb = biomass_upper_ci, 
                population_count = abundance, 
                # upperp = abundance_lower_ci, 
                # lowerp = abundance_upper_ci, 
                SRVY = survey_region,
                year = survey_year) %>%
  dplyr::select(biomass_mt, population_count, SRVY, year, species_code) %>%
  dplyr::mutate(survey_definition_id = dplyr::case_when(
    SRVY == "NBS" ~ 143, 
    SRVY == "EBS" ~ 98), 
    stratum = 999,
    file = "crab_gap_ebs_nbs_abundance_biomass.csv") %>% 
  dplyr::left_join(y = biomass_crab,
                   by = c("SRVY", "species_code", "stratum", "year")) %>% 
  dplyr::mutate(n_length = ifelse(is.na(n_length), 0, n_length))

biomass <-  dplyr::bind_rows(biomass_crab, biomass) %>%
  dplyr::filter(year > 1981 & 
                  stratum %in% c(strat0)) %>%
  dplyr::left_join(
    y = spp_info %>% 
      dplyr::select(species_code, common_name, species_name), 
    by = "species_code") %>%
  dplyr::mutate(taxon = dplyr::case_when(
    species_code <= 31550 ~ "fish", 
    species_code >= 40001 ~ "invert"))

biomass_maxyr <- biomass %>%
  dplyr::filter(stratum == 999) %>%
  dplyr::filter(year == maxyr)

biomass_compareyr <- biomass %>%
  dplyr::filter(stratum == 999) %>%
  dplyr::filter(year == compareyr[1])

total_biomass <- biomass %>% 
  dplyr::filter(year %in% c(maxyr, compareyr)) %>% 
  dplyr::filter(stratum == 999) %>%
  dplyr::group_by(SRVY, year, taxon) %>% 
  dplyr::summarise(total = sum(biomass_mt, na.rm = T)) %>% 
  dplyr::ungroup()

# ## Calculate Biomass and CPUE --------------------------------------------------
# 
# print("Calculate Biomass and CPUE")
# 
# report_spp1 <- add_report_spp(spp_info = spp_info, 
#                               spp_info_codes = "species_code", 
#                               report_spp = report_spp, 
#                               report_spp_col = "table_bio_spp", 
#                               report_spp_codes = "species_code")
# 
# spp_info1 <- dplyr::left_join(
#   x = spp_info %>% 
#     dplyr::select(-species_name), 
#   y = report_spp1 %>% 
#     dplyr::select(order, file_name, print_name, common_name1, group, group_sci, 
#                   species_code, species_name, species_name1), 
#   by = "species_code")
# 
# cpue_biomass_station <- tidyr::crossing(
#   haul_cruises_vess %>%
#     dplyr::filter(SRVY %in% c("NBS", "EBS")),
#   dplyr::distinct(
#     catch_haul_cruises %>%
#       dplyr::filter(SRVY %in% c("NBS", "EBS"))  %>%
#       dplyr::left_join(
#         x = .,
#         y = spp_info1 %>% 
#           # dplyr::mutate(group = species_name) %>% 
#           dplyr::select(species_code, group, species_name, species_name1, print_name, taxon),
#         by = "species_code"),
#     species_code, species_name, species_name1, group, print_name, taxon)) %>%
#   dplyr::left_join(
#     x = .,
#     y = catch_haul_cruises %>%
#       dplyr::select("cruisejoin", "hauljoin", "cruisejoin", "species_code",
#                     "weight", "number_fish", "SRVY"),
#     by = c("species_code", "hauljoin", "cruisejoin", "SRVY")) %>%
#   #### a check for species with weights greater then 0
#   ## sum catch weight (by groups) by station and join to haul table (again) to add on relevent haul data
#   dplyr::group_by(year, station, SRVY, species_name, species_name1, print_name, taxon, #species_code,
#                   group, hauljoin, stratum, distance_fished, net_width) %>%
#   dplyr::summarise(wt_kg_summed_by_station = sum(weight, na.rm = TRUE), # overwrite NAs in assign_group_zeros where data exists
#                    num_summed_by_station = sum(number_fish, na.rm = TRUE)) %>% # overwrite NAs in
#   
#   ## checks catch_and_zeros table for species that are not in groups, if species are not grouped
#   #### add group to assign_groups table
#   ## calculates CPUE for each species group by station
#   dplyr::mutate(effort = distance_fished * net_width/10) %>%
#   dplyr::mutate(cpue_kgkm2 = wt_kg_summed_by_station/effort) %>%
#   dplyr::mutate(cpue_nokm2 = ifelse(wt_kg_summed_by_station > 0 & num_summed_by_station == 0, NA,
#                                     (cpue_no = num_summed_by_station/effort))) %>%
#   #### this is to check CPUEs by group, station and year against the SQL code
#   ## add area to CPUE table
#   dplyr::ungroup() %>% 
#   dplyr::left_join(x = .,
#                    y = stratum_info %>%
#                      dplyr::select(stratum, area),
#                    by = 'stratum')  %>% 
#   dplyr::left_join(x = ., 
#                    y = station_info, 
#                    by = c("station", "SRVY", "stratum")) %>% 
#   dplyr::rename(latitude = latitude_dd_start, 
#                 longitude = start_longitude) %>% 
#   dplyr::filter(!is.na(station))
# #   # total biomass excluding empty shells and debris for each year
# #   dplyr::filter(group != 'empty shells and debris')  %>%
# #   dplyr::mutate(type = ifelse(
# #     grepl(pattern = "@", x = (group), fixed = TRUE),
# #     # species_name == paste0(genus_taxon, " ", species_taxon),
# #     "ital", NA)) %>%
# #   tidyr::separate(group, c("group", "species_name", "extra"), sep = "_") %>%
# #   dplyr::select(-extra) %>%
# #   dplyr::mutate(species_name = gsub(pattern = "@", replacement = " ",
# #                                     x = species_name, fixed = TRUE)) %>% 
# #   dplyr::ungroup()
# 
# # bb <- dplyr::bind_rows(
# #   cpue_biomass_station %>% 
# #     dplyr::filter(!species_code %in% c(69323, 69322, 68580, 68560)), 
# #   cpue_biomass_station %>% 
# #     dplyr::filter(species_code %in% c(69323, 69322, 68580, 68560))
# # )
# 
# if (report_title == "community") {
#   cpue_biomass_station <- dplyr::bind_rows(
#     dplyr::left_join(
#       x = cpue_biomass_station %>% 
#         dplyr::filter(print_name %in% c("blue king crab", "red king crab", "snow crab")) %>% 
#         dplyr::select(-cpue_kgkm2, -cpue_nokm2), 
#       y = cpue_crab %>% 
#         dplyr::mutate(print_name = dplyr::case_when(
#           species_code == 69323 ~ "blue king crab", 
#           species_code == 69322 ~ "red king crab", 
#           species_code == 68580 ~ "snow crab", 
#           # species_code == 68560 ~ "Tanner crab", 
#         )) %>% 
#         dplyr::select(print_name, year, hauljoin, cpue_kgkm2, cpue_nokm2), 
#       by = c("print_name", "year", "hauljoin")
#     ), 
#     cpue_biomass_station %>% 
#       dplyr::filter(!(print_name %in% c("blue king crab", "red king crab", "snow crab")) ) )
# }
# 
# cpue_biomass_stratum <- cpue_biomass_station %>%
#   ## calculates mean CPUE (weight) by year, group, stratum, and area
#   dplyr::ungroup() %>%
#   dplyr::group_by(year, group, species_name, species_name1, print_name, 
#                   stratum, area, SRVY, taxon) %>%
#   dplyr::summarise(cpue_by_group_stratum = mean(cpue_kgkm2, na.rm = TRUE)) %>% # TOLEDO - na.rm = T?
#   ## creates column for meanCPUE per group/stratum/year*area of stratum
#   dplyr::mutate(mean_cpue_times_area = (cpue_by_group_stratum * area)) %>%
#   ## calculates sum of mean CPUE*area (over the 3 strata)
#   dplyr::ungroup()
# # we'll remove crab stuff from here soon
# 
# cpue_biomass_total <- cpue_biomass_stratum %>%
#   dplyr::group_by(year, group, SRVY, species_name, species_name1, print_name, taxon) %>%
#   dplyr::summarise(mean_CPUE_all_strata_times_area =
#                      sum(mean_cpue_times_area, na.rm = TRUE)) %>% # TOLEDO - na.rm = T?
#   
#   # calculates total area by adding up the unique area values (each strata has a different value)
#   dplyr::left_join(
#     x = ., 
#     y = cpue_biomass_station %>% 
#       dplyr::ungroup() %>%
#       dplyr::select(area, SRVY) %>% 
#       dplyr::distinct() %>%
#       dplyr::group_by(SRVY) %>% 
#       dplyr::summarise(total_area = sum(area, na.rm = TRUE)), 
#     by = "SRVY") %>%
#   
#   ## creates column with weighted CPUEs
#   dplyr::mutate(weighted_CPUE = (mean_CPUE_all_strata_times_area / total_area)) %>%
#   ### uses WEIGHTED CPUEs to calculate biomass
#   ## includes empty shells and debris
#   dplyr::group_by(year, group, SRVY, species_name, species_name1, print_name) %>%
#   dplyr::mutate(biomass_mt = weighted_CPUE*(total_area*.1)) %>%
#   # total biomass excluding empty shells and debris for each year
#   dplyr::filter(group != 'empty shells and debris')  %>%
#   # dplyr::mutate(type = ifelse(
#   #   grepl(pattern = "@", x = (group), fixed = TRUE),
#   #   # species_name == paste0(genus_taxon, " ", species_taxon),
#   #   "ital", NA)) %>%
#   # tidyr::separate(group, c("group", "species_name", "extra"), sep = "_") %>%
#   # dplyr::select(-extra) %>%
#   # dplyr::mutate(species_name = gsub(pattern = "@", replacement = " ",
#   #                                   x = species_name, fixed = TRUE)) %>% 
#   dplyr::ungroup()
# 
# 
# if (report_title == "community") {
#   
#   cpue_biomass_total <- dplyr::bind_rows(
#     dplyr::left_join(
#       x = cpue_biomass_total %>% 
#         dplyr::filter(print_name %in% c("blue king crab", "red king crab", "snow crab")) %>% 
#         dplyr::select(-weighted_CPUE, -biomass_mt, -mean_CPUE_all_strata_times_area), 
#       y = biomass_tot_crab %>% 
#         dplyr::mutate(print_name = dplyr::case_when(
#           species_code == 69323 ~ "blue king crab", 
#           species_code == 69322 ~ "red king crab", 
#           species_code == 68580 ~ "snow crab", 
#           # species_code == 68560 ~ "Tanner crab", 
#         )) %>% 
#         dplyr::rename(biomass_mt = biomass) %>% 
#         dplyr::select(print_name, year, SRVY, biomass_mt), 
#       by = c("print_name", "year", "SRVY")
#     ), 
#     cpue_biomass_total %>% 
#       dplyr::filter(!(print_name %in% c("blue king crab", "red king crab", "snow crab"))) )
#   
#   cpue_biomass_stratum <- cpue_biomass_stratum %>%
#     # remove crab stuff because they use diff strata etc
#     dplyr::filter(!(print_name %in% c("blue king crab", "red king crab", "snow crab")))
# }
# 
# 
# ## Total Biomass ---------------------------------------------------------------
# 
# print("Total Biomass")
# 
# calc_cpue_bio <- function(catch_haul_cruises0){
#   
#   # for tech memo table: calculate biomass for fish and invert taxa in table 7
#   # Created by: Rebecca Haehn
#   # Contact: rebecca.haehn@noaa.gov
#   # Created: 13 January 2022
#   # script modifed from biomass script for stock assessments
#   # Modified: 
#   
#   # *** *** fiter to EBS only data by innerjoin (catch and haul) --------------------
#   
#   ## to test this I filtered for YEAR = 2017 in the haul data, row count matches prebiocatch table in Oracle (after running legacy ebs_plusnw script) **do not run with filter to get match
#   ## 
#   ## the filter removes empty banacles, empty bivalve/gastropod shell, invert egg unid, unsorted catch and debris, Polychaete tubes, and unsorted shab 
#   
#   # *** *** create zeros table for CPUE calculation ---------------------------------
#   # zeros table so every haul/vessel/year combination includes a row for every species caught (combine)
#   
#   temp1 <- catch_haul_cruises0 #%>% 
#   # dplyr::group_by(year, SRVY, cruisejoin, hauljoin, station, stratum, haul, cruise, 
#   #                 species_code, distance_fished, net_width) %>% 
#   # dplyr::summarise(weight = sum(weight, na.rm = TRUE), 
#   #                  number_fish = sum(number_fish, na.rm = TRUE))
#   
#   if (is.numeric(catch_haul_cruises0$species_code)) {
#     temp1 <- temp1 %>%
#       dplyr::filter(species_code < 99991)
#   }
#   
#   z <-  temp1 %>% 
#     tidyr::complete(species_code, 
#                     nesting(SRVY, cruise, haul, #vessel, 
#                             year, hauljoin, stratum, station, 
#                             distance_fished, net_width)) %>%
#     dplyr::select(SRVY, cruise, hauljoin, haul, #vessel, 
#                   year, species_code, weight, number_fish, stratum, 
#                   station, distance_fished, net_width) %>%
#     tidyr::replace_na(list(weight = 0, number_fish = 0))
#   
#   
#   catch_with_zeros <- 
#     dplyr::full_join(x = temp1, 
#                      y = z, 
#                      by = c("SRVY", "cruise", "hauljoin", "haul", 
#                             "year", "species_code", "stratum", "station", 
#                             "distance_fished", "net_width")) %>%
#     dplyr::select(-weight.y, -number_fish.y, -gear_depth, 
#                   -duration, -net_height) %>%
#     dplyr::arrange(year, haul, species_code) %>%
#     dplyr::rename(weight_kg = weight.x, number_fish = number_fish.x) %>%
#     tidyr::replace_na(list(weight_kg = 0, number_fish = 0))
#   
#   # *** *** calculate CPUE (mean CPUE by strata) ----------------------------------------------------------
#   
#   # num <- temp1 %>%
#   #   dplyr::distinct(SRVY, year, hauljoin, species_code) %>%
#   #   dplyr::group_by(SRVY, year, species_code) %>%
#   #   dplyr::summarize(num = n())
#   
#   cpue_by_stratum <- catch_with_zeros %>%
#     dplyr::select(SRVY, species_code, year, stratum, station,
#                   distance_fished, net_width, weight_kg) %>%
#     dplyr::mutate(
#       effort = distance_fished * net_width/10,
#       cpue_kgkm2 = weight_kg/effort) %>% 
#     dplyr::left_join(x = .,
#                      y = stratum_info %>%
#                        dplyr::select(stratum, area, SRVY),
#                      by = c("SRVY", "stratum")) %>%
#     dplyr::arrange(stratum, species_code) %>%
#     dplyr::group_by(SRVY, species_code, year, stratum, area) %>%
#     dplyr::summarise( 
#       cpue_kgha_strat = mean(cpue_kgkm2, na.rm = TRUE), #weight_kg/effort, 
#       cpue_kgha_var = ifelse(n() <= 1, 0, var(cpue_kgkm2)/n()),
#       num_hauls = n(),     # num_hauls = ifelse(num == 1, 1, (num-1)),
#       total_area = sum(unique(area))) %>%
#     dplyr::mutate(strata = dplyr::case_when(
#       (stratum == 31 | stratum == 32) ~ 30,
#       (stratum == 41 | stratum == 42) | stratum == 43 ~ 40,
#       (stratum == 61 | stratum == 62) ~ 60, 
#       TRUE ~ as.numeric(stratum)))
#   
#   
#   # *** biomass -----------------------------------------------------------------
#   
#   # ## CANNOT use biomass_*** tables bc they don't contain the info for all species (ie: no poachers, blennies, lumpsuckers, eelpouts, etc.)
#   
#   biomass_by_stratum <- biomass_cpue_by_stratum <- cpue_by_stratum %>%
#     dplyr::mutate(
#       biomass_mt = cpue_kgha_strat * (area * 0.1), 
#       bio_var = (area^2 * cpue_kgha_var/100), 
#       fi = area * (area - num_hauls)/num_hauls,
#       ci = qt(p = 0.025, df = num_hauls - 1, lower.tail = F) * sqrt(bio_var), 
#       up_ci_bio = biomass_mt + ci,
#       low_ci_bio = ifelse(biomass_mt - ci <0, 0, biomass_mt - ci) )
#   
#   
#   total_biomass <- biomass_by_stratum %>%
#     dplyr::filter((species_code >= 40000 &
#                      species_code < 99991) |
#                     (species_code > 1 & 
#                        species_code < 35000)) %>% 
#     ungroup() %>%
#     dplyr::group_by(SRVY, year) %>% 
#     dplyr::summarise(total = sum(biomass_mt, na.rm = TRUE))
#   
#   return(list("biomass_cpue_by_stratum" = biomass_cpue_by_stratum, 
#               "total_biomass" = total_biomass))
#   
# }
# 
# a <- calc_cpue_bio(catch_haul_cruises0 = catch_haul_cruises_maxyr)
# b <- calc_cpue_bio(catch_haul_cruises0 = catch_haul_cruises_compareyr)
# 
# biomass_cpue_by_stratum <- cpue_by_stratum <- biomass_by_stratum <- 
#   a$biomass_cpue_by_stratum # %>%  # remove crab totals, as they use different stratum # TOLEDO
# # dplyr::filter(!(species_code %in% c(69323, 69322, 68580, 68560)))
# 
# # subtract our-calculated crab totals so we can add the right total from SAP
# cc <- dplyr::bind_rows(a$biomass_cpue_by_stratum, 
#                        b$biomass_cpue_by_stratum) %>% 
#   dplyr::filter((species_code %in% c(69323, 69322, 68580, 68560))) %>%
#   ungroup() %>%
#   dplyr::group_by(SRVY, year) %>%
#   dplyr::summarise(total_crab_wrong = sum(biomass_mt, na.rm = TRUE))
# 
# total_biomass <- 
#   dplyr::left_join(x = dplyr::bind_rows(a$total_biomass, 
#                                         b$total_biomass), 
#                    y = cc) %>% 
#   dplyr::left_join(x = ., 
#                    y = biomass_tot_crab %>% 
#                      dplyr::filter(stratum == 999) %>%
#                      ungroup() %>%
#                      dplyr::group_by(SRVY, year) %>% 
#                      dplyr::summarise(total_crab_correct = sum(biomass, na.rm = TRUE))) %>% 
#   dplyr::mutate(total = total - total_crab_wrong + total_crab_correct) %>% 
#   dplyr::select(-total_crab_wrong, -total_crab_correct)
# 

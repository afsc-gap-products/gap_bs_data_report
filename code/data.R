# Report types ---------------------------------------------------

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

## Load Documents from Google Drive --------------------------------------------

print("Load google drive data")

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
    if (a$name[i] == "0-survey-team-bios") {
      googledrive::drive_download(file = googledrive::as_id(a$id[i]), 
                                  type = "docx",
                                  overwrite = TRUE, 
                                  path = paste0(dir_out_rawdata, "/", a$name[i], ".docx"))
    }
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

print("Load oracle data")

a <- list.files(path = here::here("data"), 
                full.names = TRUE, recursive = FALSE, pattern = ".csv")

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

catch <- gap_products_akfin_catch0 %>% 
  dplyr::filter(hauljoin %in% unique(gap_products_akfin_haul0$hauljoin)) %>%
  dplyr::ungroup()

## spp_info --------------------------------------------------------------------

print("report_spp and spp_info")

spp_info0 <- gap_products_test_species_classification0 

names(spp_info0) <- gsub(pattern = "_taxon", replacement = "", x = names(spp_info0))

spp_info <- spp_info0  %>% 
  dplyr::filter(database != "ITIS") %>% 
  dplyr::mutate(
    taxon = dplyr::case_when(
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
        TRUE, FALSE)) %>% # remove " shells", "empty", "unsorted", "shab". May also consider removing " egg", "unid.", "compound"
  dplyr::distinct()

spp_info_maxyr <- spp_info %>%
  dplyr::filter(species_code %in%
                  unique(catch$species_code))

## report_spp ------------------------------------------------------------------

print("report_spp")

report_spp <- readr::read_csv(file = paste0(dir_out_rawdata, "/0_species_local_names.csv"), 
                              skip = 1, 
                              show_col_types = FALSE) %>% 
  dplyr::select(!(dplyr::starts_with(ifelse(report_title == "community", "datar_", "community_")))) %>%
  dplyr::select(where(~ !(all(is.na(.)) | all(. == "")))) %>% 
  dplyr::select(-questions) 

names(report_spp)[
  grepl(pattern = ifelse(report_title == "community", "community_", "datar_"), 
        x = names(report_spp))] <- 
  gsub(pattern = ifelse(report_title == "community", "community_", "datar_"), 
       replacement = "", 
       x = names(report_spp)[
         grepl(pattern = ifelse(report_title == "community", "community_", "datar_"), 
               x = names(report_spp))])

report_spp <- report_spp  %>% # Replace NA by FALSE
  dplyr::mutate(dplyr::across(dplyr::starts_with("text_"), ~replace(., is.na(.), FALSE))) %>% 
  dplyr::mutate(dplyr::across(dplyr::starts_with("cpue"), ~replace(., is.na(.), FALSE))) %>% 
  dplyr::mutate(dplyr::across(dplyr::starts_with("table_"), ~replace(., is.na(.), FALSE))) %>% 
  dplyr::mutate(dplyr::across(dplyr::starts_with("plot_"), ~replace(., is.na(.), FALSE)))

## report_spp1 ------------------------------------------------------------------

temp1 <- data.frame()
for (i in 1:nrow(report_spp)){
  temp2 <- eval(expr = parse(text = report_spp$species_code[i]))
  temp1 <- dplyr::bind_rows(temp1, 
                            dplyr::bind_cols(report_spp[i,], 
                                             species_code1 = eval(expr = parse(text = report_spp$species_code[i]))))
}

temp1 <- temp1 %>% 
  dplyr::mutate(taxon = dplyr::case_when(
    species_code1 <= 31550 ~ "fish", 
    species_code1 >= 40001 ~ "invert")) %>% 
  dplyr::filter(print_name != "") %>% 
  dplyr::mutate(group0 = group)

# all -> other
temp <- unique(temp1$print_name)[grepl(pattern = "all ", 
                                       x = unique(temp1$print_name), 
                                       ignore.case = TRUE)]
temp1$print_name1 <- temp1$print_name

if (length(temp)>0) {
  for (i in 1:length(temp)) {
    temp2 <- intersect(temp1$species_code1[temp1$print_name == temp[i]], 
                       temp1$species_code1[temp1$print_name != temp[i]]) # find which are duplicates 
    if (length(temp2)>0) {
      # and delete them from "all "
      temp1 <- temp1[!(temp1$species_code1 %in% temp2 &
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

report_spp1 <-  
  dplyr::left_join(x = temp1  %>% 
                     dplyr::select(-species_code) %>% 
                     dplyr::rename(species_code = species_code1),
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
                species_name0 = gsub(r"{\s*\([^\)]+\)}","", species_name0), 
                species_name = species_name0) %>% 
  dplyr::select(-type, -temp, -species_name0, -genus, -species)

## cruises + maxyr + compareyr -------------------------------------------------
print("cruises + maxyr  + compareyr")

cruises <- gap_products_akfin_cruise0 %>% 
  dplyr::select(cruise, year, survey_name, vessel_id, survey_definition_id, 
                vessel_name, date_start, date_end, cruisejoin) %>% 
  dplyr::filter(year != 2020 & # no surveys happened this year that we care about
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

## haul and station ------------------------------------------------------------------------

print("haul and station")

haul <- dplyr::left_join(
  y = gap_products_akfin_haul0, 
  x = cruises %>% 
    dplyr::select(cruisejoin, survey_definition_id, year), 
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

station <- gap_products_old_station0 %>%
  dplyr::filter(srvy %in% c("EBS", "NBS") &
                  design_year == max(design_year, na.rm = TRUE)) %>%
  dplyr::select(station, stratum, SRVY = srvy) %>%
  dplyr::mutate(
    stratum = as.numeric(stratum), 
    survey_definition_id = dplyr::case_when(
      SRVY == "NBS" ~ 143, 
      SRVY == "EBS" ~ 98 ) )

# if NBS is not released

cruises_race <- race_data_cruises0 %>% 
  dplyr::mutate(year = as.numeric(substr(x = cruise, 1, 4)), 
                SRVY = dplyr::case_when(
                  substr(cruise, 6, 6) == 1 ~ "EBS", 
                  substr(cruise, 6, 6) == 2 ~ "NBS"), 
                survey_definition_id = dplyr::case_when(
                  SRVY == "EBS" ~ 98, 
                  SRVY == "NBS" ~ 143) )%>% 
  dplyr::filter(
    year == maxyr &
      vessel_id %in% unique(haul$vessel_id[haul$year == maxyr]) ) 

if (length(cruises_race$data_in_final == "N")>0) {
  haul_missing <- cruises_race %>%
    dplyr::distinct() %>%
    dplyr::left_join(y = race_data_edit_haul0, 
                     by = c("cruise_id")) %>%
    dplyr::filter(year == maxyr & 
                    abundance_haul == "Y" &
                    haul_type %in% 3 & 
                    data_in_final == "N") %>%
    dplyr::select(cruise, year, haul, station, vessel_id, haul_id) %>% # , stratum, SRVY, survey_definition_id
    dplyr::left_join(y = race_data_edit_events0 %>% 
                       dplyr::select(haul_id, latitude_dd_start = edit_latitude, longitude_dd_start = edit_longitude), 
                     by = c("haul_id")) %>%
    dplyr::left_join(y = station)
  
  haul <- dplyr::bind_rows(haul, haul_missing)
}

haul_maxyr <- haul %>% 
  dplyr::filter(year == maxyr)

haul_compareyr <- haul %>% 
  dplyr::filter(year == compareyr[1])

station <- station  %>% 
  dplyr::full_join(
    y = 
      haul %>%  
      dplyr::filter(year == maxyr) %>%
      # dplyr::select(station, stratum, latitude_dd_start, longitude_dd_start) %>% 
      dplyr::group_by(station, stratum) %>% 
      dplyr::summarise(latitude_dd_start = mean(latitude_dd_start, na.rm = TRUE), 
                       longitude_dd_start = mean(longitude_dd_start, na.rm = TRUE)) %>% 
      dplyr::ungroup(), 
    by = c("stratum", "station")) %>% 
  dplyr::mutate(in_maxyr = (station %in% haul_maxyr$station), 
                reg = dplyr::case_when(
                  station %in% c("CC-04", "CC-05", "CC-06", "CC-07", "CC-08", "CC-09", 
                                 "CC-10", "BB-04", "BB-05", "BB-06", "BB-07", "BB-08", 
                                 "BB-09", "BB-10", "AA-04", "AA-05", "AA-06", "AA-07", 
                                 "AA-08", "AA-10", "ZZ-04", "ZZ-05", "Y-04") ~ "Norton Sound")) 

## other var (survey additions, *yrs, etc. -------------------------------------

print("define other vars")

temp <- cruises_race %>%
  dplyr::left_join(y = race_data_hauls0) %>%
  # dplyr::left_join(y = cruises_race) %>%
  dplyr::filter(#year == maxyr &
    survey_definition_id %in% SRVY00 &
      # stationid %in% reg_dat$survey.grid$station &
      haul_type %in% c(17, 20, 4))

if (length(cruises_race$data_in_final == "N")>0) {
  haul_missing <- cruises_race %>%
    dplyr::distinct() %>%
    dplyr::left_join(y = race_data_edit_haul0, 
                     by = c("cruise_id")) %>%
    dplyr::filter(year == maxyr & 
                    # abundance_haul == "Y" &
                    haul_type %in% c(17, 20, 4) & 
                    data_in_final == "N") %>%
    dplyr::select(cruise, year, haul, station) %>% # , stratum, SRVY, survey_definition_id
    dplyr::left_join(y = station)
  
  temp <- dplyr::bind_rows(temp, haul_missing)
}

# temp <- cruises_race %>%
#   dplyr::distinct() %>%
#   dplyr::left_join(y = race_data_edit_haul0, 
#                    by = c("cruise_id")) %>%
#   dplyr::filter(year == maxyr & 
#                   haul_type %in% c(17, 20))

# temp <- haul %>% 
#   # dplyr::left_join(y = cruises_race) %>% 
#   dplyr::filter(year == maxyr & 
#                   survey_definition_id %in% SRVY00 &
#                   stationid %in% reg_dat$survey.grid$station &
#                   haul_type %in% c(17, 20)) 

# Crab retows
crab_resample <- FALSE
if (sum(unique(temp$haul_type[temp$year == maxyr]) %in% 17) >0) {
  crab_resample <- TRUE
  haul_maxyr_crabretow <- temp %>%
    dplyr::filter(haul_type == 17) 
}

# 15/30
tow1530 <- FALSE
if (sum(unique(temp$haul_type[temp$year == maxyr]) %in% c(20, 4)) >0) {
  haul_maxyr_tow1530 <- temp %>%
    dplyr::filter(haul_type == c(20, 4), 
                  station %in% report_types$NEBS$station) 
  tow1530 <- ifelse(nrow(haul_maxyr_tow1530)==0, FALSE, TRUE)
}

nbsyr <- gap_products_akfin_cruise0 %>% 
  dplyr::filter(survey_definition_id == 143 & 
                  year <= maxyr) %>% 
  dplyr::select(year) %>% 
  unique() %>% 
  unlist() %>% 
  sort(decreasing = TRUE)

lastyr <- max(haul$year[haul$year != maxyr])

## CPUE Design Based Estimates -------------------------------------------------

print("CPUE Design Based Estimates")

cpue <- dplyr::bind_rows(
  gap_products_akfin_cpue0 %>% # groundfish data (sans crab)
    dplyr::filter(!(species_code %in% c(69322, 69323, 68580, 68560))) %>%
    dplyr::select(species_code, hauljoin, cpue_kgkm2, cpue_nokm2), 
  crab_gap_ebs_nbs_crab_cpue0 %>%  # crab data
    dplyr::filter(survey_year > 1981) %>%
    dplyr::select(species_code, hauljoin, 
                  cpue_kgkm2 = cpuewgt_total, 
                  cpue_nokm2 = cpuenum_total) )  %>% 
  dplyr::right_join(y = haul %>% 
                      dplyr::select(hauljoin, station, stratum, survey_definition_id, SRVY, 
                                    year, cruisejoin, longitude_dd_start, latitude_dd_start), 
                    by = "hauljoin") %>% 
  dplyr::filter(!(year < 1996 & species_code == 10261) & # modify for spp
                  !(year < 2000 & species_code == 435 ) &
                  !(year < 2000 & species_code == 471) )  %>% # 2022/10/28 - Duane - Alaska skate abundance/distribution figures should include only data from 2000 and later, due to earlier identification issues (which are clearly indicated in the plots).
  dplyr::left_join(y = spp_info %>% 
                     dplyr::select(species_code, common_name, species_name, taxon), 
                   by = "species_code") 

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
      survey_definition_id == 98 ~ "EBS"),
    area_nmi2 = area_km2/3.429904, 
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
    by = "stratum") 

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

vessels <- haul_cruises_vess_maxyr %>% 
  dplyr::select(vessel_name, vessel_ital, vessel_id, tonnage,
                length_m, length_ft, vess_shape) %>% 
  unique() %>% 
  dplyr::mutate(img = dplyr::case_when(
    vessel_id == 94 ~ "94_vesteraalen.png", 
    vessel_id == 134 ~ "134_northwestexplorer.png", 
    vessel_id == 162 ~ "163_alaskaknight.png"), 
    vess_col = c("grey20", "grey30")) %>% 
  dplyr::arrange(vessel_name) %>% 
  dplyr::add_row(
    vessel_id = NA, 
    vessel_name = "Not Surveyed", 
    vessel_ital = "Not Surveyed", 
    vess_shape = "X", 
    vess_col = "#E6E6E6") 
# c(gray.colors(n = nrow(.))))

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

lengths_sap <- dplyr::bind_rows(
  crab_ebscrab0 %>%
    dplyr::filter(!(cruise %in% unique(crab_ebscrab_nbs0$cruise))) %>% # there may be some nbs data in the ebs (201002)
    dplyr::mutate(SRVY = "EBS"),
  crab_ebscrab_nbs0 %>%
    dplyr::mutate(SRVY = "NBS") ) %>% 
  dplyr::group_by(cruise, species_code)  %>% 
  dplyr::summarise(frequency = n()) %>% 
  dplyr::ungroup() %>% 
  dplyr::right_join(y = cruises %>% 
                     dplyr::select(survey_definition_id, year, cruise) %>% 
                     dplyr::distinct()) %>% 
  dplyr::select(-cruise)

lengths_gap <- race_data_cruises0 %>%
  dplyr::left_join(race_data_surveys0, by = "survey_id") %>% 
  dplyr::left_join(race_data_survey_definitions0, by = "survey_definition_id") %>% 
  dplyr::left_join(race_data_hauls0, by = "cruise_id") %>%
  dplyr::left_join(race_data_lengths0, by = "haul_id") %>% 
  dplyr::filter(region == "BS" & 
                  !is.na(species_code) &
                  survey_definition_id %in% SRVY00 &
                  performance >= 0 &
                  haul_type == 3) %>%
  dplyr::mutate(species_code = dplyr::case_when(
    species_code == 21721 ~ 21720, 
    species_code == 21741 ~ 21740, 
    TRUE ~ species_code) ) %>%
  dplyr::group_by(survey_definition_id, species_code, year) %>% 
  dplyr::summarise(frequency = sum(frequency, na.rm = TRUE)) %>% 
  dplyr::ungroup()

lengths <- dplyr::bind_rows(lengths_gap, lengths_sap) %>% 
  dplyr::left_join(cruises %>% dplyr::select(SRVY, survey_definition_id) %>% dplyr::distinct() )

lengths_maxyr <- lengths %>%
  dplyr::filter(year == maxyr)

## specimen --------------------------------------------------------------------

print("specimen")

specimen <- gap_products_akfin_specimen0 %>% 
  dplyr::select(hauljoin, cruisejoin, species_code, length_mm, sex, age_years, 
                specimen_subsample_method, specimen_sample_type) %>% 
  dplyr::left_join(
    y = haul %>% 
      dplyr::select(cruisejoin, hauljoin, station, stratum),  
    by = c("cruisejoin", "hauljoin")) %>% 
  dplyr::left_join(
    y = cruises %>% 
      dplyr::select(cruisejoin, survey_name, SRVY, survey_definition_id, year),  
    by = c("cruisejoin"))  %>% 
  dplyr::filter(!is.na(year)) %>% 
  dplyr::arrange(desc(year))

specimen_compareyr <- specimen %>% 
  dplyr::filter(year == compareyr)

specimen_maxyr <- specimen %>% 
  dplyr::filter(year == maxyr)

## temperatures ----------------------------------------------------------------

print("tempertures")

temps_avg_yr <- coldpool::cold_pool_index %>% 
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

print("sizecomp")

# Crab 
sizecomp_sap <- crab_nbs_size1mm_all_species0 %>% 
  dplyr::rename(SRVY = survey_definition_id) %>%
  tidyr::pivot_longer(cols = c(number_unsexed, number_males, number_mature_females, number_immature_females),
                      names_to = "sex", values_to = "population_count")  %>%
  dplyr::group_by(sex, length_mm, year, species_code, SRVY) %>% 
  dplyr::summarise(population_count = sum(population_count, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    survey_definition_id = dplyr::case_when(
      SRVY == "NBS" ~ 143, 
      SRVY == "EBS" ~ 98), 
    sex = dplyr::case_when(
      sex == "number_unsexed" ~ "unsexed",
      sex == "number_males" ~ "males",
      sex == "number_immature_females" ~ "immature females",
      sex == "number_mature_females" ~ "mature females"))

sizecomp_gap <- gap_products_akfin_sizecomp0 %>% # GAP data
  dplyr::filter(
    area_id %in% c(99900, 99902) & # 10, 20, 30, 40, 50, 60, 82, 90, 
      survey_definition_id %in% SRVY00 &
      year <= maxyr &
      # area_id > 99900 & 
      length_mm > 0 &
      !(year < 1996 & species_code == 10261) &
      !(year < 2000 & species_code == 435) & # 2022/10/28 - Duane - Alaska skate abundance/distribution figures should include only data from 2000 and later, due to earlier identification issues (which are clearly indicated in the plots).
      !(year < 2000 & species_code == 471)) %>% 
  dplyr::mutate(
    sex = dplyr::case_when(
      sex == 1 ~ "males", 
      sex == 2 ~ "females", 
      sex == 3 ~ "unsexed"))

sizecomp <- dplyr::bind_rows(sizecomp_sap, sizecomp_gap) %>% 
  dplyr::filter(
    year <= maxyr &
      survey_definition_id %in% SRVY00 & 
      year > 1981 & 
      !is.na(length_mm) & 
      !is.na(population_count) & 
      population_count!= 0 & 
      !is.na(species_code)) %>%
  dplyr::left_join(y = spp_info %>% 
                     dplyr::select(species_code, taxon),
                   by  = "species_code") %>% 
  dplyr::mutate(
    SRVY = dplyr::case_when(
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 98 ~ "EBS")) %>% 
  dplyr::select(-area_id)

sizecomp_maxyr <- sizecomp %>%
  dplyr::filter(year == maxyr)

sizecomp_compareyr <- sizecomp %>%
  dplyr::filter(year == compareyr[1])

## biomass ---------------------------------------------------------------------
print("biomass")

biomass_gap <- gap_products_akfin_biomass0 %>%
  dplyr::filter(
    area_id %in% c(10, 20, 30, 40, 50, 60, 82, 90, 99900, 99902) &
      survey_definition_id %in% SRVY00 &
      n_weight > 0 & 
      year <= maxyr &
      !(species_code %in% c(69323, 69322, 68580, 68560)) &
      !is.na(species_code) & 
      !(year < 1996 & species_code == 10261) &
      !(year < 2000 & species_code == 471) & # 2022/10/28 - Duane - Alaska skate abundance/distribution figures should include only data from 2000 and later, due to earlier identification issues (which are clearly indicated in the plots).
      !(year < 2000 & species_code == 435 )
  ) %>%  
  unique() %>%
  dplyr::mutate(
    SRVY = dplyr::case_when(
      survey_definition_id == 143 ~ "NBS", 
      survey_definition_id == 98 ~ "EBS"),
    stratum = ifelse(area_id %in% c(99900, 99902), 999, area_id))

biomass_sap <- crab_gap_ebs_nbs_abundance_biomass0 %>% 
  dplyr::rename(biomass_mt = biomass_total,
                population_count = abundance, 
                SRVY = survey_region,
                year = survey_year) %>%
  dplyr::select(biomass_mt, population_count, SRVY, year, species_code) %>%
  dplyr::mutate(survey_definition_id = dplyr::case_when(
    SRVY == "NBS" ~ 143, 
    SRVY == "EBS" ~ 98), 
    area_id = dplyr::case_when(
      SRVY == "NBS" ~ 99902, 
      SRVY == "EBS" ~ 99900), 
    stratum = 999) 

biomass <- dplyr::bind_rows(biomass_gap, biomass_sap) %>%
  dplyr::filter(year > 1981 & 
                  stratum %in% c(strat0)) %>%
  dplyr::left_join(
    y = spp_info %>% 
      dplyr::select(species_code, common_name, species_name, taxon), 
    by = "species_code") %>% 
  dplyr::mutate(
    cpue_kgkm2_sd = sqrt(cpue_kgkm2_var), 
    cpue_nokm2_sd = sqrt(cpue_nokm2_var), 
    biomass_sd = sqrt(biomass_var), 
    # biomass_cv = (biomass_sd/biomass_mt), 
    # biomass_cv_up = biomass_mt + biomass_cv, 
    # biomass_cv_dw = biomass_mt - biomass_cv, 
    biomass_95ci_up = biomass_mt + (2*biomass_sd), 
    biomass_95ci_dw = biomass_mt - (2*biomass_sd), 
    population_sd = sqrt(population_var), 
    # population_cv = (population_sd/population_count), 
    # population_cv_up = population_count + population_cv, 
    # population_cv_dw = population_count - population_cv, 
    population_95ci_up = population_count + (2*population_sd), 
    population_95ci_dw = population_count - (2*population_sd) )

biomass_maxyr <- biomass %>%
  dplyr::filter(stratum == 999) %>%
  dplyr::filter(year == maxyr)

biomass_compareyr <- biomass %>%
  dplyr::filter(stratum == 999) %>%
  dplyr::filter(year == compareyr[1])

total_biomass <- biomass %>% 
  dplyr::filter(stratum == 999) %>%
  dplyr::group_by(SRVY, year, taxon) %>% 
  dplyr::summarise(total = sum(biomass_mt, na.rm = T)) %>% 
  dplyr::ungroup()

#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. J. Dawson, R. Haehn and E. H. Markowitz'
#' purpose: Store functions
#' start date: 2021-03-03
#' date modified: 2021-03-03        # CHANGE
#' Notes:                             # CHANGE
#' ---

#############INSTALL PACKAGES##############
# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  # For creating R Markdown Docs
  "knitr", # A general-purpose tool for dynamic report generation in R
  "rmarkdown", # R Markdown Document Conversion

  # File Management
  "here", # For finding the root directory of your scripts and thus, find your files
  "officer",

  # Keeping Organized
  "devtools", # Package development tools for R; used here for downloading packages from GitHub
  # "renv", # saves the packages in the R environment


  # Graphics
  "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
  "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette"
  "cowplot",
  "png",
  "extrafont",

  # Text
  "NMFSReports", # devtools::install_github("emilymarkowitz-noaa/NMFSReports") # Package of my favorite grammar and file managment functions for writing reproducible reports

  # Citations
  "knitcitations", # devtools::install_github("cboettig/knitcitations")

  # other tidyverse
  "dplyr",
  "googledrive",
  "magrittr",
  "readr",

  # Text Management
  "stringr",
  
  # Spatial
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "sf",
  "rlist", 
  "jsonlite", 
  "prettymapr",
  "rgdal", 
  "rosm", 
  "shadowtext", 
  "ggspatial", 
  "digest", 
  "ggsn",
  "rgdal", 
  "ps", 
  "backports", 
  "callr", 
  "labeling", 
  "gstat", 
  "magrittr", 
  "raster", 
  "reshape", 
  "stars",

  
  # Time
  # "lubridate",
  
  # Species
  "taxize", 
  
  # For outputting JS files
  "jsonlite", 
  
  # For editing XML files
  "XML", 
  
  # Oracle
  "RODBC")


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}

loadfonts(device = "win")

# renv::snapshot()


#######CITE R PACKAGES###########
tmp <- tempfile(fileext=".bib")

a<-write.bibtex(entry = eval(parse(text=paste0("c(", paste(paste0("citation('",PKG,"')"), collapse = ", "), ")"))),
                file = tmp)

write.bibtex(entry = a,
             file = paste0(dir_out_rawdata, "bibliography_RPack.bib"))
write.bibtex(entry = a,
             file = paste0(dir_cite,"/bibliography_RPack.bib"))

################Functions#############

######### General Stuff ########

SameColNames<-function(df.ls) {
  #All column names
  colnames0<-c()
  for (i in 1:length(df.ls)){
    df0<-df.ls[[i]]
    # colnames(df0)<-toupper(colnames(df0))
    df0<-janitor::clean_names(df0)
    df.ls[[i]]<-df0
    colnames0<-c(colnames0, (colnames(df0)))
  }
  colnames0<-sort(unique(colnames0), decreasing = T)
  
  #New df's
  df.ls0<-list()
  df.rbind0<-c()
  for (i in 1:length(df.ls)){
    df0<-df.ls[[i]]
    colnames.out<-colnames0[!(colnames0 %in% colnames(df0))]
    if (length(colnames.out) != 0) {
      for (ii in 1:length(colnames.out)){
        df0[,(ncol(df0)+1)]<-NA
        names(df0)[ncol(df0)]<-colnames.out[ii]
      }
    }
    df0<-df0[,match(table =  colnames(df0), x = colnames0)]
    df.ls0[[i]]<-df0
    names(df.ls0)[i]<-names(df.ls)[i]
    df.rbind0<-rbind.data.frame(df.rbind0, df0)
  }
  return(df.rbind0)
}


findhowmanyspp<-function(spp.tsn.list, ranklvl) {
  
  a<-rlist::list.search(.data = lapply(X = spp.tsn.list, '[', 2), 
                        ranklvl == .)
  names(a)<-gsub(pattern = ".rank", replacement = "", x = names(a))
  
  b<-lapply(X = spp.tsn.list[names(spp.tsn.list) %in% names(a)], '[', 3)
  # b<-rlist::list.search(.data = lapply(X = spp.tsn.list, '[', 3), 
  #                    . == ranklvl)
  
  unq<-c()
  for (i in 1:length(a)) {
    if (!(is.na(b[[i]]))) {
      idx<-ifelse(a[[i]] %in% "species", 1, which(a[[i]])) # for invalid species
      cc<-as.numeric(b[[i]]$id)[idx]
      unq<-c(unq, cc)
    }
  }
  unq<-unique(unq)
  
  return(unq)
}


CapStr <- function(y) {
  c <- strsplit(y, " ")[[1]]
  paste(toupper(substring(c, 1,1)), substring(c, 2),
        sep="", collapse=" ")
}

yrofsurvey<-(maxyr-2018)+37



stndth<-ifelse(grepl(pattern = 1, x = substr(x = (maxyr-2018)+37, start = nchar((maxyr-2018)+37), 
                                             stop = nchar((maxyr-2018)+37) )), 
               "st", 
               ifelse(grepl(pattern = 2, x = substr(x = (maxyr-2018)+37, start = nchar((maxyr-2018)+37), 
                                                    stop = nchar((maxyr-2018)+37))), "nd", "th")
)


#' Find the age of the file, when it was created. 
#'
#' @param path Path to the file. 
#' @param format default = "%B %d, %Y"
#' @return x
#' @export
ageoffile<-function(path, format = "%B %d, %Y") {
  # system("touch temp")
  info <- file.info(path)
  x<-format(info$mtime, format)
  return(x)
}




# EBS_summary <- EBS %>% 
#   dplyr::select(year, hauljoin, distance_fished_km, net_width_m, weight) %>%
#   dplyr::group_by(year, hauljoin, distance_fished_km, net_width_m) %>% 
#   dplyr::summarise(net_width_kg = sum(weight, na.rm = TRUE))
# 
# head(EBS_summary)

#' Calculate CPUE of Survey
#'
#' @param dat A data.frame. This contains at least 3 headers named net_width_kg, distance_fished_km, and net_width_m.  All 3 columns are A numeric or a vector. net_width_kg is a numeric or a vector that represents the total weight of catch of a haul. dist_fished is the distnace survey fished on this trip and net_width_m and width of net.  
#' @return A numeric or a data.frame of hauljoin and CPUE (kg/ha). 
#' @export
#'
#' @examples
#' CPUE0<-CPUE(dat)
CPUE <- function(dat){
  
  CPUE_kg_ha <- dat %>% 
    dplyr::group_by(hauljoin) %>%
    dplyr::summarise(CPUE = (net_width/
                               area_swept(dist_fish = distance_fished, 
                                          net_width = net_width)))
  
  return(CPUE_kg_ha)
}


#' Area Swept
#'
#' @param dist_fish A numeric or a vector. Distnace survey fished on this trip.
#' @param net_width A numeric or a vector. Width of net.
#'
#' @return
#' @export
#'
#' @examples area_swept(dist_fish = 2.77, net_width_m = 16.8)
area_swept<-function(dist_fish, net_width) {
  return((dist_fish * net_width)/10)
}


######### Spatial ########

make_idw_map0 <- function (x = NA, COMMON_NAME = NA, LATITUDE = NA, LONGITUDE = NA, 
                            CPUE_KGHA = NA, region = "bs.south", extrap.box = NA, 
                            set.breaks = "jenks", grid.cell = c(0.05, 0.05), in.crs = "+proj=longlat", 
                            out.crs = "auto", key.title = "auto", log.transform = FALSE, 
                            idw.nmax = 4, use.survey.bathymetry = TRUE, return.continuous.grid = TRUE) 
{
  if (is.na(x)) {
    x <- data.frame(COMMON_NAME = COMMON_NAME, 
                    LATITUDE = LATITUDE, 
                    LONGITUDE = LONGITUDE, 
                    CPUE_KGHA = CPUE_KGHA)
  }
  if (key.title == "auto") {
    key.title <- x$COMMON_NAME[1]
  }
  if (is.na(extrap.box)) {
    if (region %in% c("bs.south", "sebs")) {
      extrap.box = c(xmn = -179.5, xmx = -157, 
                     ymn = 54, ymx = 63)
    }
    if (region %in% c("bs.all", "ebs")) {
      extrap.box = c(xmn = -179.5, xmx = -157, 
                     ymn = 54, ymx = 68)
    } 
    if (region %in% c("bs.north", "nbs")) {
      extrap.box = c(xmn = -179.5, xmx = -157, 
                     ymn = 60, ymx = 68)
    } 
  }
  map_layers <- get_base_layers0(select.region = region, 
                                 set.crs = out.crs)
  out.crs <- map_layers$crs
  if (use.survey.bathymetry) {
    map_layers$bathymetry <- get_survey_bathymetry0(select.region = region, 
                                                    set.crs = out.crs)
  }
  x <- sf::st_as_sf(x, coords = c(x = "LONGITUDE", y = "LATITUDE"), 
                    crs = sf::st_crs(in.crs)) %>% sf::st_transform(crs = map_layers$crs)
  idw_fit <- gstat::gstat(formula = CPUE_KGHA ~ 1, locations = x, 
                          nmax = idw.nmax)
  stn.predict <- predict(idw_fit, x)
  sp_extrap.raster <- raster::raster(xmn = extrap.box["xmn"], 
                                     xmx = extrap.box["xmx"], ymn = extrap.box["ymn"], 
                                     ymx = extrap.box["ymx"], ncol = (extrap.box["xmx"] - 
                                                                        extrap.box["xmn"])/grid.cell, nrow = (extrap.box["ymx"] - 
                                                                                                                extrap.box["ymn"])/grid.cell, crs = crs(in.crs)) %>% 
    projectRaster(crs = crs(x))
  
  extrap.grid <- predict(idw_fit, as(sp_extrap.raster, "SpatialPoints")) %>% 
    sf::st_as_sf() %>% sf::st_transform(crs = crs(x)) %>% 
    stars::st_rasterize() %>% sf::st_join(map_layers$survey.area, 
                                          join = st_intersects)
  if (return.continuous.grid) {
    continuous.grid <- extrap.grid
  } else {
    continuous.grid <- NA
  }
  alt.round <- 0
  if (is.character(set.breaks[1])) {
    set.breaks <- tolower(set.breaks)
    break.vals <- classInt::classIntervals(x$CPUE_KGHA, n = 5, 
                                           style = set.breaks)$brks
    alt.round <- floor(-1 * (min((log10(break.vals) - 2)[abs(break.vals) > 
                                                           0])))
    set.breaks <- c(-1, round(break.vals, alt.round))
  }
  if (min(set.breaks) > 0) {
    set.breaks <- c(0, set.breaks)
  }
  if (min(set.breaks) == 0) {
    set.breaks <- c(-1, set.breaks)
  }
  if (max(set.breaks) < max(stn.predict$var1.pred)) {
    set.breaks[length(set.breaks)] <- max(stn.predict$var1.pred) +1
  }
  dig.lab <- 7
  set.levels <- cut(stn.predict$var1.pred, set.breaks, right = TRUE, 
                    dig.lab = dig.lab)
  if (alt.round > 0) {
    while (dig.lab > alt.round) {
      dig.lab <- dig.lab - 1
      set.levels <- cut(stn.predict$var1.pred, set.breaks, 
                        right = TRUE, dig.lab = dig.lab)
    }
  }
  else {
    while (length(grep("\\.", set.levels)) > 0) {
      dig.lab <- dig.lab - 1
      set.levels <- cut(stn.predict$var1.pred, set.breaks, 
                        right = TRUE, dig.lab = dig.lab)
    }
  }
  extrap.grid$var1.pred <- cut(extrap.grid$var1.pred, set.breaks, 
                               right = TRUE, dig.lab = dig.lab)
  sig.dig <- round(set.breaks[which(nchar(round(set.breaks)) >= 
                                      4)])
  make_level_labels <- function(vec) {
    vec <- as.character(vec)
    vec[grep("-1", vec)] <- "No catch"
    vec <- sub("\\(", "\\>", vec)
    vec <- sub("\\,", "–", vec)
    vec <- sub("\\]", "", vec)
    if (length(sig.dig) > 3) {
      for (j in 1:length(sig.dig)) {
        vec <- sub(sig.dig[j], format(sig.dig[j], nsmall = 0, 
                                      big.mark = ","), vec)
      }
    }
    return(vec)
  }
  extrap.grid$var1.pred <- factor(make_level_labels(extrap.grid$var1.pred), 
                                  levels = make_level_labels(levels(set.levels)))
  n.breaks <- length(levels(set.levels))
  p1 <- ggplot() + 
    geom_sf(data = map_layers$survey.area, fill = NA) + 
    geom_stars(data = extrap.grid) + 
    geom_sf(data = map_layers$survey.area, 
            fill = NA) + geom_sf(data = map_layers$akland, fill = "grey80") + 
    geom_sf(data = map_layers$bathymetry) + 
    geom_sf(data = map_layers$graticule, 
            color = alpha("grey70", 0.3)) + 
    scale_fill_manual(name = paste0(key.title, "\nCPUE (kg/ha)"), 
                      values = c("white", RColorBrewer::brewer.pal(9, name = "Blues")[c(2, 4, 6, 8, 9)]), 
                      na.translate = FALSE, 
                      drop = FALSE) + 
    scale_x_continuous(breaks = map_layers$lon.breaks) + 
    scale_y_continuous(breaks = map_layers$lat.breaks) + 
    coord_sf(xlim = map_layers$plot.boundary$x, 
             ylim = map_layers$plot.boundary$y) + 
    theme(panel.border = element_rect(color = "black", fill = NA), 
          panel.background = element_rect(fill = NA, color = "black"), 
          legend.key = element_rect(fill = NA, color = "grey70"), 
          legend.position = c(0.12, 0.18), 
          axis.title = element_blank(), 
          axis.text = element_text(size = 10), 
          legend.text = element_text(size = 10), 
          legend.title = element_text(size = 10), 
          plot.background = element_rect(fill = NA, color = NA))
  
  return(list(plot = p1, map_layers = map_layers, extrapolation.grid = extrap.grid, 
              continuous.grid = continuous.grid, region = region, n.breaks = n.breaks, 
              key.title = key.title, crs = out.crs))
}




get_base_layers0 <- function (select.region, 
                              set.crs = "+proj=longlat +datum=NAD83", 
                              use.survey.bathymetry = FALSE) 
{
  if (set.crs == "auto") {
    region.crs <- c("+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=68.1 +lat_2=70.7 +lat_0=69.4 +lon_0=-162.6 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=60.8 +lat_2=67 +lat_0=63.9 +lon_0=-167 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    set.crs <- region.crs[match(select.region, c("bs.south", 
                                                 "sebs", "bs.all", "ebs", "ecs", 
                                                 "ebs.ecs", "bs.north", "nbs"))]
  }
  akland <- sf::st_read(system.file("data", "ak_russia.shp", 
                                    package = "akgfmaps"), quiet = TRUE)
  bathymetry <- sf::st_read(system.file("data", "npac_0-200_meters.shp", 
                                        package = "akgfmaps"), quiet = TRUE)
  if (select.region %in% c("bs.south", "sebs")) {
    survey.area <- sf::st_read(system.file("data", 
                                           "sebs_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", 
                                             "sebs_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177.3, 
                                                                         -154.3), y = c(54.5, 63.15)), out.crs = set.crs)
    graticule <- st_graticule(lat = seq(54, 64, 2), lon = seq(-180, 
                                                              -140, 5), margin = 1e-05)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54, 64, 2)
  }
  if (select.region %in% c("bs.all", "ebs")) {
    survey.area <- sf::st_read(system.file("data", 
                                           "ebs_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", 
                                             "ebs_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177.8, 
                                                                         -154.7), y = c(63.15, 65.1)), out.crs = set.crs)
    graticule <- st_graticule(lat = seq(54, 68, 2), lon = seq(-180, 
                                                              -140, 5), margin = 1e-05)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(54, 66, 2)
  }
  if (select.region %in% c("bs.north", "nbs")) {
    survey.area <- sf::st_read(system.file("data", 
                                           "ebs_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)$geometry[2]
    
    survey.strata <- sf::st_read(system.file("data", 
                                             "ebs_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE) %>% 
      dplyr::filter(STRATUM %in% c(70, 71, 81))
    
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177.3, -160), 
                                                                   y = c(60, 66)), out.crs = set.crs)
    graticule <- st_graticule(lat = seq(64, 68, 2), lon = seq(-180, 
                                                              -140, 5), margin = 1e-05)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(64, 66, 2)
  }
  if (select.region == "ecs") {
    survey.area <- sf::st_read(system.file("data", 
                                           "chukchi_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", 
                                             "chukchi_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-170, 
                                                                         -156), y = c(65, 73)), out.crs = set.crs)
    graticule <- st_graticule(lat = seq(60, 76, 2), lon = seq(-180, 
                                                              -140, 5), margin = 1e-05)
    lon.breaks <- seq(-180, -154, 5)
    lat.breaks <- seq(66, 76, 2)
  }
  if (select.region == "ebs.ecs") {
    survey.area <- sf::st_read(system.file("data", 
                                           "ebs_chukchi_survey_boundary.shp", package = "akgfmaps"), 
                               quiet = TRUE)
    survey.strata <- sf::st_read(system.file("data", 
                                             "ebs_chukchi_strata.shp", package = "akgfmaps"), 
                                 quiet = TRUE)
    bathymetry <- sf::st_read(system.file("data", "npac_0-200_meters.shp", 
                                          package = "akgfmaps"), quiet = TRUE)
    plot.boundary <- akgfmaps::transform_data_frame_crs(data.frame(x = c(-177, 
                                                                         -151), y = c(54.5, 72.5)), out.crs = set.crs)
    graticule <- st_graticule(lat = seq(54, 78, 4), lon = seq(-180, 
                                                              -140, 5), margin = 1e-05)
    lon.breaks <- seq(-180, -150, 5)
    lat.breaks <- seq(54, 78, 4)
  }
  if (tolower(class(set.crs)) != "crs") {
    set.crs <- sf::st_crs(set.crs)
  }
  akland <- akland %>% sf::st_transform(crs = set.crs)
  survey.area <- survey.area %>% sf::st_transform(crs = set.crs)
  survey.strata <- survey.strata %>% sf::st_transform(crs = set.crs)
  bathymetry <- bathymetry %>% sf::st_transform(crs = set.crs)
  place.labels <- read.csv(file = system.file("data", 
                                              "placenames.csv", package = "akgfmaps")) %>% 
    dplyr::filter(region == select.region) %>% akgfmaps::transform_data_frame_crs(out.crs = set.crs)
  return(list(akland = akland, survey.area = survey.area, survey.strata = survey.strata, 
              bathymetry = bathymetry, place.labels = place.labels, 
              graticule = graticule, crs = set.crs, plot.boundary = plot.boundary, 
              lon.breaks = lon.breaks, lat.breaks = lat.breaks))
}


get_survey_bathymetry0 <- function (select.region, set.crs) 
{
  
  if (set.crs == "auto") {
    region.crs <- c("+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=55 +lat_2=60 +lat_0=57.5 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=57 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=68.1 +lat_2=70.7 +lat_0=69.4 +lon_0=-162.6 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=60.8 +lat_2=67 +lat_0=63.9 +lon_0=-167 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs", 
                    "+proj=aea +lat_1=60 +lat_2=63 +lat_0=59 +lon_0=-170 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
    set.crs <- region.crs[match(select.region, c("bs.south", 
                                                 "sebs", "bs.all", "ebs", "ecs", 
                                                 "ebs.ecs", "bs.north", "nbs"))]
  }
  
  if (select.region %in% c("bs.all", "ebs")) {
    bathymetry <- sf::st_read(system.file("data", "ebs_survey_bathymetry.shp", 
                                          package = "akgfmaps"), quiet = TRUE) %>% st_transform(crs = set.crs)
  }
  else if (select.region %in% c("bs.south", "sebs")) {
    bathymetry <- sf::st_read(system.file("data", "ebs_survey_bathymetry.shp", 
                                          package = "akgfmaps"), quiet = TRUE) %>% st_transform(crs = set.crs) %>% 
      dplyr::filter(FNODE_ != 5)
  } else if (select.region %in% c("bs.north", "nbs")) {
    bathymetry <- sf::st_read(system.file("data", "ebs_survey_bathymetry.shp", 
                                          package = "akgfmaps"), quiet = TRUE) %>% st_transform(crs = set.crs) %>% 
      dplyr::filter(FNODE_ == 5)
  }
  else {
    stop(paste0("No survey-specific bathymetry available for ", 
                select.region, ". If using make_idw_map, set use.survey.bathymetry = FALSE."))
  }
  return(bathymetry)
}


#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. J. Dawson, R. Haehn and E. H. Markowitz'
#' purpose: Store functions
#' start date: 2021-09-01
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
  # "here", # For finding the root directory of your scripts and thus, find your files
  # "officer",
  
  # Keeping Organized
  "devtools", # Package development tools for R; used here for downloading packages from GitHub
  # "renv", # saves the packages in the R environment
  
  
  # Graphics
  "ggplot2", # Create Elegant Data Visualisations Using the Grammar of Graphics
  "cowplot",
  "png",
  "extrafont",
  "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette")
  
  # Text
  "NMFSReports", # devtools::install_github("emilymarkowitz-noaa/NMFSReports") # Package of my favorite grammar and file managment functions for writing reproducible reports
  
  # Citations
  "knitcitations", # devtools::install_github("cboettig/knitcitations")
  
  # other tidyverse
  "dplyr",
  "googledrive",
  "magrittr",
  "readr",
  "tidyr",
  
  # Text Management
  "stringr",
  "readtext",
  
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
  # "taxize", 
  
  # For outputting JS files
  # "jsonlite", 
  
  # Tables
  "officer", 
  "flextable", 
  
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

# renv ------------------------------------------------------------------

# renv::snapshot()


# Cite R Packages --------------------------------------------------------

knitr::write_bib(x = PKG,
                 file = paste0(dir_out_rawdata, "bibliography_RPack.bib"))

file.copy(from = paste0(dir_out_rawdata, "bibliography_RPack.bib"),
          to = paste0(dir_cite,"/bibliography_RPack.bib"),
          overwrite = TRUE)

# Functions -------------------------------------------------------------


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


# CapStr <- function(y) {
#   c <- strsplit(y, " ")[[1]]
#   paste(toupper(substring(c, 1,1)), substring(c, 2),
#         sep="", collapse=" ")
# }

# yrofsurvey<-(maxyr-2018)+37
# 
# 
# 
# stndth<-ifelse(grepl(pattern = 1, x = substr(x = (maxyr-2018)+37, start = nchar((maxyr-2018)+37), 
#                                              stop = nchar((maxyr-2018)+37) )), 
#                "st", 
#                ifelse(grepl(pattern = 2, x = substr(x = (maxyr-2018)+37, start = nchar((maxyr-2018)+37), 
#                                                     stop = nchar((maxyr-2018)+37))), "nd", "th")
# )


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

CapFirst <- function(x) {
  xx <- c()
  for (i in 1:length(x)){
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
    xx <- c(xx, s)
  }
  return(xx)
}


# Species -----------------------------------------------


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

#' find values based on strings
#'
#' @param x 
#' @param col 
#' @param str 
#' @param str_not 
#' @param col_out 
#'
#' @return
#' @export
#'
#' @examples
#'   find_codes(x = species, str = "skate", col = "common_name", 
#'                col_out = "common_name")
#'     find_codes(x = species, str = "skate", col = "common_name", 
#'                col_out = "common_name", str_not = "Alaska skate")
#'     find_codes(x = species, str = "skate", col = "common_name")
find_codes <- function(x, col = "common_name", str = NULL, 
                       str_not = NULL, col_str_not = NULL, 
                       col_out = "species_code") {
  out <- x 
  
  if (is.null(col_str_not)) {
    col_str_not <- col
  }
  
  # 1. remove codes that we defintly dont want 
  out <- out %>% 
    dplyr::filter(
      !(grepl(pattern = " egg", 
              x = unlist(out[,col]),
              ignore.case = TRUE)))
  
  # 2. find the codes we do want
  if (!is.null(str)) {
    
    str <- str[!is.na(str)]
    str <- unique(str)
    
    for (i in 1:length(str)){
      
      out <- out %>% 
        dplyr::filter(
          grepl(pattern = str[i], 
                x = as.character(unlist(out[,col])),
                ignore.case = TRUE))
    }
  }
  
  # 3. remove codes that may have been included in codes we want (2)
  if (!is.null(str_not)) {
    
    str_not <- str_not[!is.na(str_not)]
    str_not <- unique(str_not)
    
    for (i in 1:length(str_not)){
      out <- out  %>%
        dplyr::filter(!(grepl(pattern = str_not[i], 
                              x = unlist(out[,col_str_not]),
                              ignore.case = TRUE))) 
    }
  }
  
  # clean codes
  out <- out  %>%
    dplyr::select(all_of(col_out)) %>% 
    unique() %>% 
    unlist() 
  
  names(out) <- NULL
  
  if (length(out) == 0) {
    out <- NA
  } else {
    out <- sort(out)
  }
  
  return(out)
}


species_table <- function(table_list, dat_maxyr_spp, spp_common, SURVEY, SRVY = NA) {
  
  # Edit This:
  header <- paste0("Summary of environmental variables that ", NMFSReports::tolower2(spp_common), " (", spp_sci, ") have been found in across the ", SURVEY, ifelse(SRVY %in% NA, paste0(" (", SRVY, ")"), ""))
  footnote<-""
  
  # Select data and make plot
  cols<-c("start_latitude", "start_longitude",  "weight", "number_fish", "bottom_depth", "gear_temperature", "surface_temperature")
  COLS<-c("Latitude", "Longitude", 
          "Weight", "Abundance", 
          "Bottom Depth", "Bottom Temperature", "Surface Temperature")
  
  # basiccontent0<-c()
  basiccontenttable<-c()
  
  for (ii in 1:length(cols)) {
    basiccontenttable<-rbind.data.frame(basiccontenttable, 
                                        data.frame(metric0 = cols[ii], 
                                                   Metric = COLS[ii], 
                                                   Min = min(dat_maxyr_spp[cols[ii]], na.rm = T), 
                                                   Max = max(dat_maxyr_spp[cols[ii]], na.rm = T), 
                                                   Mean = sum(dat_maxyr_spp[cols[ii]], na.rm = T)/nrow(dat_maxyr_spp)
                                        ))
  }
  
  basiccontenttable_print <- basiccontenttable
  basiccontenttable_print[, c("Min", "Max", "Mean")] <- 
    NMFSReports::mod_number(x = basiccontenttable_print[, c("Min", "Max", "Mean")], 
                            comma_seperator = TRUE, 
                            divideby = 1, 
                            digits = 2)
  basiccontenttable_print$metric0<-NULL
  
  table_raw = basiccontenttable
  table_print = basiccontenttable_print
  
  table_list<-save_tables(table_raw = table_raw, 
                          table_print = table_print, 
                          table_list = table_list, 
                          header = header,
                          footnote = footnote,
                          filename0 = filename0, 
                          cnt_chapt_content = "NA", 
                          cnt = "NA", 
                          path = dir_out_tables, 
                          filename_desc = ifelse(SRVY %in% NA, paste0("_", SRVY), ""))
  
  return(table_list)
  
}





species_text <- function(dat_maxyr, dat_maxyr_1, basiccontenttable_print, 
                         dat_maxyr_spp, dat_maxyr_spp_length, 
                         length_type, 
                         spp_common, spp_code, SRVY, cnt_figures) {
  
  str <- c()
  
  # <!-- how many stations -->
  str <- paste0(str, 
                "Out of the total number of successful hauls (", 
                length(unique(dat_maxyr$hauljoin)), ") ",  
                NMFSReports::tolower2(spp_common), 
                " was found during ", 
                length(unique(dat_maxyr_spp$hauljoin)), 
                " hauls (", 
                formatC(x = (length(unique(dat_maxyr_spp$hauljoin))/length(unique(dat_maxyr$hauljoin)))*100, digits = 1, format = "f"), 
                "% of stations). ")
  
  str <- paste0(str, "

During the ", maxyr, 
" survey, ", 
NMFSReports::tolower2(spp_common), 
" were present at ", 
formatC(x = (length(unique(dat_maxyr_spp$hauljoin))/length(unique(dat_maxyr$hauljoin)))*100, digits = 1, format = "f") , 
"% of stations in the ", SRVY, " (", 
length(unique(dat_maxyr_spp$hauljoin)), " of ", 
length(unique(dat_maxyr$hauljoin)), 
" stations). ")
  
  # <!-- bottom tempature -->
  str <- paste0(str, "

", NMFSReports::tolower2(spp_common, capitalizefirst = TRUE), 
" were found in bottom temperatures as warm as ", 
as.numeric(basiccontenttable_print %>% dplyr::filter(Metric == "Bottom Temperature") %>% dplyr::select(Max)) , 
"°C and as cold as ", 
as.numeric(basiccontenttable_print %>% dplyr::filter(Metric == "Bottom Temperature") %>% dplyr::select(Min)) , 
"°C (Figure ", cnt_figures,"). ")
  
  # <!-- surface temperature -->
  str <- paste0(str, "

", NMFSReports::tolower2(spp_common, capitalizefirst = TRUE), 
" were found in areas where surface temperatures were as warm as ", 
as.numeric(basiccontenttable_print %>% dplyr::filter(Metric == "Surface Temperature") %>% dplyr::select(Max)) , 
"°C and as cold as ", 
as.numeric(basiccontenttable_print %>% dplyr::filter(Metric == "Surface Temperature") %>% dplyr::select(Min)) , 
"°C (Figure ", cnt_figures,"). ")
  
  # <!-- Depth -->
  str <- paste0(str, "

They were found in waters with depths between ", 
as.numeric(basiccontenttable_print %>% dplyr::filter(Metric == "Bottom Depth") %>% dplyr::select(Min)) , 
" m and ", as.numeric(basiccontenttable_print %>% dplyr::filter(Metric == "Bottom Depth") %>% dplyr::select(Max)) , " m. ")
  
  # <!-- Sizes caught  -->
  str <- paste0(str, "

The ", NMFSReports::text_list(length_type$sentancefrag[length_type$code %in% unique(dat_maxyr_spp_length$length_type)]), 
" of ", NMFSReports::tolower2(spp_common, capitalizefirst = TRUE), 
" measured during the survey were between ", min(dat_maxyr_spp_length$length, na.rm = TRUE), 
" and ", max(dat_maxyr_spp_length$length, na.rm = TRUE), " ", 
unique(dplyr::case_when(spp_code %in% 1:31550 ~ 'cm', 
                        spp_code %in% 68000:69930 ~ 'mm'), 
       TRUE ~ 'NO MEASUREMENT'), ". ")
  
  # <!-- weight -->
  str <- paste0(str, "

The total number of ", 
NMFSReports::tolower2(spp_common), 
" estimated to have been caught by the survey is ", 
NMFSReports::xunits(value = sum(dat_maxyr_spp$number_fish, na.rm = TRUE)), 
" individuals, which equates to ", 
NMFSReports::xunits(value = sum(dat_maxyr_spp$weight, na.rm = TRUE)), 
" kg of biomass. ")
  
  str <- paste0(str, "

Compared with ", maxyr-1, ", 
abundance experienced ", 
NMFSReports::pchange(start = sum(dat_maxyr_1_spp$number_fish, na.rm = TRUE), end = sum(dat_maxyr_spp$number_fish, na.rm = TRUE)) ,
" and there was ", 
NMFSReports::pchange(start = sum(dat_maxyr_1_spp$weight, na.rm = TRUE), end = sum(dat_maxyr_spp$weight, na.rm = TRUE)) , 
" in biomass. ")
  
  return(str)
  
}

# Plotting ----------------------------

#' Plot IDW maps in x by x facet_wraps
#'
#' @param yrs The years, as a vector, that subplots should be created for
#' @param dat The data that will be used to create the IDW plots
#' @param cruises # cruise data
#' @param lat The name of the column in dat for latitude
#' @param lon The name of the column in dat for longitude
#' @param var The name of the column in dat for the variable
#' @param key.title A character string that will be used as the legend title
#' @param grid "continuous.grid" or "extrapolation.grid"
#' @set.breaks Suggested. Vector of break points to use for plotting. Feeds the akgfmaps::make_idw_map.  function. 
#' @extrap.box Optional. Vector specifying the dimensions of the extrapolation grid. Elements of the vector should be named to specify the minimum and maximum x and y values c(xmn, xmx, ymn, ymx). If not provided, region will be used to set the extrapolation area. Feeds the akgfmaps::make_idw_map. 
#' @param workfaster TRUE/FALSE. Cuts down work if TRUE. 
#' @param nrow How many rows in the face_wrap. Feeds from ggplot2::facet_wrap. 
#'
#' @return
#' @export
#'
#' @examples
plot_idw_xbyx <- function(
  yrs, 
  dat, 
  cruises, 
  lat,
  lon,
  var,
  key.title, 
  grid,
  extrap.box, 
  set.breaks = "jenks", #seq(from = -2, to = 20, by = 2),
  workfaster = FALSE, 
  nrow = 2, 
  SRVY, 
  col_viridis = "viridis"){
  
  # Select data and make plot
  for (ii in ifelse(workfaster,2,length(yrs)):1) {
    
    region <- "bs.south"
    # if (SRVY == "NEBS"){
    #   temp1 <- cruises %>%
    #     dplyr::filter(year %in% yrs[ii]) %>%
    #     dplyr::select(SRVY) %>%
    #     unlist() %>%
    #     unique()
    #   region <- ifelse(sum(temp1 %in% "NBS")>0, "bs.all", "bs.south")
    # }
    
    temp <- dat %>%
      dplyr::filter(year == yrs[ii]) 
    
    temp0 <- make_idw_map0(#akgfmaps::make_idw_map(
      LATITUDE = as.numeric(unlist(temp[,lat])),
      LONGITUDE = as.numeric(unlist(temp[,lon])),
      CPUE_KGHA = as.numeric(unlist(temp[,var])),
      use.survey.bathymetry = FALSE,
      region = region, 
      out.crs = as.character(crs(reg_dat$bathymetry)),
      extrap.box = extrap.box, 
      set.breaks = set.breaks,
      grid.cell = c(ifelse(workfaster, 0.1, 0.02), 
                    ifelse(workfaster, 0.1, 0.02)), # 0.2x0.2 degree grid cells
      key.title = key.title)
    
    temp0 <- temp0[grid][[1]]  
    
    if (ii == ifelse(workfaster,2,length(yrs))) {
      stars_list <- temp0
      names(stars_list)[names(stars_list) == "var1.pred"] <- paste0("y", yrs[ii])  
    } else {
      stars_list$temp <- temp0$var1.pred
      names(stars_list)[names(stars_list) == "temp"] <- paste0("y", yrs[ii])   
    }
  }
  
  # figure <- plot_xbyx_maps(stars_list = stars_list, title="Bottom Temperature (°C)")
  
  # https://rpubs.com/michaeldorman/646276
  stars_list <- stars_list %>% 
    dplyr::select(names(stars_list)[substr(start = 1, stop = 1, x = names(stars_list)) == "y"])
  names(stars_list)<-gsub(pattern = "y", replacement = "", x = names(stars_list))
  stars_list = st_redimension(stars_list)
  names(stars_list) = "value"
  
  figure <- ggplot() +
    geom_stars(data = stars_list) +
    facet_wrap( ~ new_dim, nrow = nrow) +
    coord_equal() + 
    scale_fill_viridis_c(option = col_viridis, 
                         limits = range(set.breaks),
      na.value = "transparent", 
      breaks = set.breaks,
      labels = set.breaks) + 
    guides(fill=guide_colourbar(title=key.title, 
                                title.position="top", 
                                title.hjust = 0.5)) +
    geom_sf(data = reg_dat$bathymetry, color = "grey50", size = 0.5) +
    geom_sf(data = reg_dat$akland, color = NA, fill = "grey80") +
    scale_x_continuous(name = "Longitude",
                       breaks = reg_dat$lon.breaks) +
    scale_y_continuous(name = "Latitude",
                       breaks = reg_dat$lat.breaks) +
    coord_sf(xlim = reg_dat$plot.boundary$x, 
             ylim = reg_dat$plot.boundary$y) +
    #set legend position and vertical arrangement
    theme(
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      strip.background = element_blank(), 
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 9),
      legend.background = element_rect(colour = "transparent", fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      axis.text = element_blank(),
      legend.position="bottom", 
      legend.box.just = "left",
      legend.key.width = unit(.5, "in"), 
      legend.box = "horizontal"
    )
  
  return(figure)
  
}



# plot_xbyx_maps <- function(
#   stars_list, 
#   nrow = 2, 
#   title){
#   
#   # https://rpubs.com/michaeldorman/646276
#   temp.list <- temp.list %>% 
#     dplyr::select(names(temp.list)[substr(start = 1, stop = 1, x = names(temp.list)) == "y"])
#   names(temp.list)<-gsub(pattern = "y", replacement = "", x = names(temp.list))
#   temp.list = st_redimension(temp.list)
#   names(temp.list) = "value"
#   
#   figure <- ggplot() +
#     geom_stars(data = temp.list) +
#     facet_wrap( ~ new_dim, nrow = nrow) +
#     coord_equal() + 
#     scale_fill_viridis_c(#option = "plasma", 
#       na.value = "transparent", 
#       breaks = seq(from = -2, to = 20, by = 2),
#       labels = seq(from = -2, to = 20, by = 2) ) + 
#     guides(fill=guide_colourbar(title=title, 
#                                 title.position="top", 
#                                 title.hjust = 0.5)) +
#     geom_sf(data = reg_dat$bathymetry, color = "grey50", size = 0.5) +
#     geom_sf(data = reg_dat$akland, color = NA, fill = "grey80") +
#     scale_x_continuous(name = "Longitude",
#                        breaks = reg_dat$lon.breaks) +
#     scale_y_continuous(name = "Latitude",
#                        breaks = reg_dat$lat.breaks) +
#     coord_sf(xlim = reg_dat$plot.boundary$x, 
#              ylim = reg_dat$plot.boundary$y) +
#     #set legend position and vertical arrangement
#     theme(
#       panel.background = element_rect(fill = "white", 
#                                       colour = NA), 
#       panel.border = element_rect(fill = NA, 
#                                   colour = "grey20"), 
#       strip.background = element_blank(), 
#       legend.title = element_text(size = 15),
#       legend.text = element_text(size = 9),
#       legend.background = element_rect(colour = "transparent", fill = "transparent"),
#       legend.key = element_rect(colour = "transparent", 
#                                 fill = "transparent"),
#       axis.text = element_blank(),
#       legend.position="bottom", 
#       legend.box.just = "left",
#       legend.key.width = unit(.5, "in"), 
#       legend.box = "horizontal"
#     )
#   
#   return(figure)
# }



#' Make IDW maps of CPUE for the EBS/NBS
#' 
#' This function can be used to make inverse-distance-weighted plots for the eastern Bering Sea and northern Bering Sea
#' 
#' @param x Data frame which contains at minimum: CPUE, LATITUDE, and LONGITUDE. Can be passed as vectors instead (see below). Default value: \code{NA}
#' @param region Character vector indicating which plotting region to use. Options: bs.south, bs.north, bs.all
#' @param grid.cell Numeric vector of length two specifying dimensions of grid cells for extrpolation grid, in degrees c(lon, lat). Default c(0.05, 0.05)
#' @param COMMON_NAME Common name
#' @param LATITUDE Latitude (degrees north)
#' @param LONGITUDE Longitude (degrees east; Western hemisphere is negative)
#' @param CPUE_KGHA Catch per unit effort in kilograms per hectare
#' @param extrap.box Optional. Vector specifying the dimensions of the extrapolation grid. Elements of the vector should be named to specify the minimum and maximum x and y values c(xmn, xmx, ymn, ymx). If not provided, region will be used to set the extrapolation area.
#' @param set.breaks Suggested. Vector of break points to use for plotting. Alternatively, a character vector indicating which break method to use. Default = "jenks"
#' @param grid.cell Optional. Numeric vector of length two, specifying the resolution for the extrapolation grid in degrees. Default c(0.05,0.05)
#' @param in.crs Character vector containing the coordinate reference system for projecting the extrapolation grid.
#' @param out.crs Character vector containing the coordinate reference system for projecting the extrapolation grid.
#' @param key.title Character vector which will appear in the legend above CPUE (kg/ha). Default = "auto" tries to pull COMMON_NAME from input.
#' @param log.transform Character vector indicating whether CPUE values should be log-transformed for IDW. Default = FALSE.
#' @param idw.nmax Maximum number of adjacent stations to use for interpolation. Default = 8
#' @param idp Inverse distance weighting power. Default = 2 
#' @param use.survey.bathymetry Logical indicating if historical survey bathymetry should be used instead of continuous regional bathymetry. Default = TRUE
#' @param return.continuous.grid If TRUE, also returns an extrapolation grid on a continuous scale.
#' @return Returns a list containing: 
#' (1) plot: a ggplot IDW map;
#' (2) extrapolation.grid: the extrapolation grid with estimated values on a discrete scale;
#' (3) continuous.grid: extrapolation grid with estimates on a continuous scale;
#' (4) region: the region;
#' (5) n.breaks: the number of level breaks;
#' (6) key.title: title for the legend;
#' (7) crs: coordinate reference system as a PROJ6 (WKT2:2019) string; 
#' @author Sean Rohan \email{sean.rohan@@noaa.gov}
#' @export

make_idw_map0 <- function (x = NA, COMMON_NAME = NA, LATITUDE = NA, LONGITUDE = NA, 
                           CPUE_KGHA = NA, region = "bs.south", extrap.box = NA, 
                           set.breaks = "jenks", grid.cell = c(0.05, 0.05), in.crs = "+proj=longlat", 
                           out.crs = "auto", key.title = "auto", log.transform = FALSE, 
                           idw.nmax = 4, use.survey.bathymetry = TRUE, return.continuous.grid = TRUE) 
{
  if (is.na(x)) {
    x <- data.frame(COMMON_NAME = COMMON_NAME, LATITUDE = LATITUDE, 
                    LONGITUDE = LONGITUDE, CPUE_KGHA = CPUE_KGHA)
  }
  if (key.title == "auto") {
    key.title <- x$COMMON_NAME[1]
  }
  if (is.null(extrap.box)) {
    if (region %in% c("bs.south", "sebs")) {
      extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, 
                     ymx = 63)
    }
    if (region %in% c("bs.north", "nbs")) {
      extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, 
                     ymx = 68)
    }
    if (region %in% c("bs.all", "ebs")) {
      extrap.box = c(xmn = -179.5, xmx = -157, ymn = 54, 
                     ymx = 68)
    }
  }
  map_layers <- akgfmaps::get_base_layers(select.region = region, 
                                          set.crs = out.crs)
  out.crs <- map_layers$crs
  if (use.survey.bathymetry) {
    map_layers$bathymetry <- get_survey_bathymetry(select.region = region, 
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
  }
  else {
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
    set.breaks[length(set.breaks)] <- max(stn.predict$var1.pred) + 
      1
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
  p1 <- ggplot() + geom_sf(data = map_layers$survey.area, fill = NA) + 
    geom_stars(data = extrap.grid) + geom_sf(data = map_layers$survey.area, 
                                             fill = NA) + geom_sf(data = map_layers$akland, fill = "grey80") + 
    geom_sf(data = map_layers$bathymetry) + geom_sf(data = map_layers$graticule, 
                                                    color = alpha("grey70", 0.3)) + scale_fill_manual(name = paste0(key.title, 
                                                                                                                    "\nCPUE (kg/ha)"), values = c("white", RColorBrewer::brewer.pal(9, 
                                                                                                                                                                                    name = "Blues")[c(2, 4, 6, 8, 9)]), na.translate = FALSE, 
                                                                                                      drop = FALSE) + scale_x_continuous(breaks = map_layers$lon.breaks) + 
    scale_y_continuous(breaks = map_layers$lat.breaks) + 
    coord_sf(xlim = map_layers$plot.boundary$x, ylim = map_layers$plot.boundary$y) + 
    theme(panel.border = element_rect(color = "black", 
                                      fill = NA), panel.background = element_rect(fill = NA, 
                                                                                  color = "black"), legend.key = element_rect(fill = NA, 
                                                                                                                              color = "grey70"), legend.position = c(0.12, 
                                                                                                                                                                     0.18), axis.title = element_blank(), axis.text = element_text(size = 10), 
          legend.text = element_text(size = 10), legend.title = element_text(size = 10), 
          plot.background = element_rect(fill = NA, color = NA))
  return(list(plot = p1, map_layers = map_layers, extrapolation.grid = extrap.grid, 
              continuous.grid = continuous.grid, region = region, n.breaks = n.breaks, 
              key.title = key.title, crs = out.crs))
}

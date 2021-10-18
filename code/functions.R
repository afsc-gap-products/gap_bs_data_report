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
  "ggpubr",
  "nmfspalette",  # devtools::install_github("nmfs-general-modeling-tools/nmfspalette")
  "lemon",
  
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
  
  # RACE-GAP Specific
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("sean-rohan-noaa/coldpool")
  
  
  # Spatial
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
  "grid", 
  
  # library(rasterVis)
  # library(scales)
  # library(ggthemes) # theme_map()
  
  
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


# Housekeeping -----------------------------------------------------------------

# Keep chapter content in a proper order
cnt_chapt <- "000"
# Automatically name objects with consecutive numbers
cnt_figures <- 0 #  e.g., Figure 1
cnt_tables <- 0 # e.g., Table 1
cnt_equations <- 0 # e.g., Equation 1
# Save object content
list_equations <- list()
list_tables <- list()
list_figures <- list()

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


species_table <- function(haul_spp, spp_common, SURVEY0, SRVY0 = NA) {
  
  # Edit This:
  header <- paste0("Summary of environmental variables that ", NMFSReports::tolower2(spp_common), " (", spp_sci, ") have been found in across the ", SURVEY0, ifelse(SRVY0 %in% NA, paste0(" (", SRVY0, ")"), ""))
  
  # Select data and make plot
  cols<-c("start_latitude", "start_longitude",  "weight", "number_fish", "bottom_depth", "gear_temperature", "surface_temperature")
  COLS<-c("Latitude", "Longitude", 
          "Weight", "Abundance", 
          "Bottom Depth", "Bottom Temperature", "Surface Temperature")
  
  haul_spp <- haul_spp %>% 
    dplyr::filter(SRVY %in% SRVY0)
  
  # basiccontent0<-c()
  table_spp<-c()
  
  for (ii in 1:length(cols)) {
    table_spp<-rbind.data.frame(table_spp, 
                                data.frame(metric0 = cols[ii], 
                                           Metric = COLS[ii], 
                                           Min = min(haul_spp[cols[ii]], na.rm = T), 
                                           Max = max(haul_spp[cols[ii]], na.rm = T), 
                                           Mean = sum(haul_spp[cols[ii]], na.rm = T)/nrow(haul_spp)
                                ))
  }
  
  table_spp_print <- table_spp
  table_spp_print[, c("Min", "Max", "Mean")] <- 
    NMFSReports::mod_number(x = table_spp_print[, c("Min", "Max", "Mean")], 
                            comma_seperator = TRUE, 
                            divideby = 1, 
                            digits = 2)
  table_spp_print$metric0<-NULL
  
  table_raw = table_spp
  table_print = table_spp_print
  
  return(list("header" = header, 
              "raw" = table_raw, 
              "print" = table_print))
}


species_text <- function(haul_maxyr, haul_compareyr_spp, table_spp_print, 
                         haul_maxyr_spp, length_maxyr, 
                         length_type, 
                         spp_common, spp_code, SRVY0, maxyr, compareyr) {
  
  
  haul_maxyr <- haul_maxyr %>% 
    dplyr::filter(SRVY %in% SRVY0)
  
  haul_compareyr_spp <- haul_compareyr_spp %>% 
    dplyr::filter(SRVY %in% SRVY0)  
  
  haul_maxyr_spp <- haul_maxyr_spp %>% 
    dplyr::filter(SRVY %in% SRVY0)  
  
  length_maxyr <- length_maxyr %>% 
    dplyr::filter(SRVY %in% SRVY0)
  
  str <- c()
  
  # <!-- how many stations -->
  str <- paste0(str, 
                "Out of the total number of successful hauls (", 
                length(unique(haul_maxyr$hauljoin)), ") ",  
                NMFSReports::tolower2(spp_common), 
                " was found during ", 
                length(unique(haul_maxyr_spp$hauljoin)), 
                " hauls (", 
                formatC(x = (length(unique(haul_maxyr_spp$hauljoin))/length(unique(haul_maxyr$hauljoin)))*100, digits = 1, format = "f"), 
                "% of stations). ")
  
  str <- paste0(str, "

During the ", maxyr, 
" survey, ", 
NMFSReports::tolower2(spp_common), 
" were present at ", 
formatC(x = (length(unique(haul_maxyr_spp$hauljoin))/length(unique(haul_maxyr$hauljoin)))*100, digits = 1, format = "f") , 
"% of stations in the ", SRVY0, " (", 
length(unique(haul_maxyr_spp$hauljoin)), " of ", 
length(unique(haul_maxyr$hauljoin)), 
" stations). ")
  
  # <!-- bottom tempature -->
  str <- paste0(str, "

", NMFSReports::tolower2(spp_common, capitalizefirst = TRUE), 
" were found in bottom temperatures as warm as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Temperature") %>% dplyr::select(Max)) , 
"°C and as cold as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Temperature") %>% dplyr::select(Min)) , 
"°C (Figure ", cnt_figures,"). ")
  
  # <!-- surface temperature -->
  str <- paste0(str, "

", NMFSReports::tolower2(spp_common, capitalizefirst = TRUE), 
" were found in areas where surface temperatures were as warm as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Surface Temperature") %>% dplyr::select(Max)) , 
"°C and as cold as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Surface Temperature") %>% dplyr::select(Min)) , 
"°C (Figure ", cnt_figures,"). ")
  
  # <!-- Depth -->
  str <- paste0(str, "

They were found in waters with depths between ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Depth") %>% dplyr::select(Min)) , 
" m and ", as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Depth") %>% dplyr::select(Max)) , " m. ")
  
  # <!-- Sizes caught  -->
  str <- paste0(str, "

The ", 
NMFSReports::text_list(length_type$sentancefrag[length_type$length_type_id %in% unique(length_maxyr$length_type)]), 
" of ", NMFSReports::tolower2(spp_common, capitalizefirst = TRUE), 
" measured during the survey were between ", 
NMFSReports::xunits(min(length_maxyr$length, na.rm = TRUE)), 
" and ", NMFSReports::xunits(max(length_maxyr$length, na.rm = TRUE)), " ", 
unique(dplyr::case_when(spp_code %in% 1:31550 ~ 'cm', 
                        spp_code %in% 68000:69930 ~ 'mm'), 
       TRUE ~ 'NO MEASUREMENT'), ". ")
  
  # <!-- weight -->
  str <- paste0(str, "

The total number of ", 
NMFSReports::tolower2(spp_common), 
" estimated to have been caught by the survey is ", 
NMFSReports::xunits(value = sum(haul_maxyr_spp$number_fish, na.rm = TRUE)), 
" individuals, which equates to ", 
NMFSReports::xunits(value = sum(haul_maxyr_spp$weight, na.rm = TRUE)), 
" kg of biomass. ")
  
  str <- paste0(str, "

Compared with ", compareyr, ", 
abundance experienced ", 
NMFSReports::pchange(start = sum(haul_compareyr_spp$number_fish, na.rm = TRUE), 
                     end = sum(haul_maxyr_spp$number_fish, na.rm = TRUE)) ,
" and there was ", 
NMFSReports::pchange(start = sum(haul_compareyr_spp$weight, na.rm = TRUE), 
                     end = sum(haul_maxyr_spp$weight, na.rm = TRUE)) , 
" in biomass. ")
  
  return(str)
  
}

species_content <- function(SURVEY000, 
                            SRVY000, 
                            haul_maxyr, 
                            haul_compareyr_spp, 
                            haul_maxyr_spp, 
                            length_maxyr, 
                            length_type, 
                            spp_common, 
                            spp_code,
                            maxyr, 
                            compareyr) {
  
  
  table_spp_maxyr <- species_table(haul_spp = haul_maxyr_spp, 
                                   spp_common, 
                                   SURVEY0 = SURVEY000, 
                                   SRVY0 = SRVY000) 
  
  table_spp_compareyr <- species_table(haul_spp = haul_compareyr_spp, 
                                       spp_common, 
                                       SURVEY0 = SURVEY000,
                                       SRVY0 = SRVY000) 
  
  
  table_spp <- dplyr::full_join(
    x = table_spp_maxyr$print %>% 
      dplyr::rename(
        Min_maxyr = Min, 
        Max_maxyr = Max, 
        Mean_maxyr = Mean), 
    y = table_spp_compareyr$print %>% 
      dplyr::rename(
        Min_compareyr = Min, 
        Max_compareyr = Max, 
        Mean_compareyr = Mean), 
    by = "Metric")
  
  text_spp <- species_text(haul_maxyr, haul_compareyr_spp, table_spp_print = table_spp_maxyr$print, haul_maxyr_spp, length_maxyr, length_type, spp_common, spp_code, SRVY0, maxyr, compareyr)
  
  return(paste0(table_spp$print, 
                "


", text_spp))
}


# Plotting ----------------------------


# https://coolbutuseless.github.io/package/ggpattern/
# https://github.com/trevorld/gridpattern


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
  grid = "extrapolation.grid",
  extrap.box, 
  set.breaks = "auto", #seq(from = -2, to = 20, by = 2),
  workfaster = FALSE, 
  nrow = 2, 
  SRVY, 
  dist_unit = "nm", # nautical miles
  col_viridis = "mako") {
  
  yrs <- as.numeric(sort(x = yrs, decreasing = T))
  
  if(set.breaks =="auto") {
    
    set.breaks0 <- classInt::classIntervals(var = as.numeric(unlist(dat[,var]))[as.numeric(unlist(dat[,var])) != 0], 
                                            n = 5, style = "jenks")$brks
    set.breaks <- c()
    
    for (i in 1:length(set.breaks0)) {
      
      if (i == length(set.breaks0)) {
        set.breaks<-c(set.breaks, ceiling(x = set.breaks0[i])) #Inf)#round(set.breaks0[i], digits = 0))
      } else if (i == 1) {
        set.breaks<-c(set.breaks, 0)
      } else {    
        set.breaks <- c(set.breaks, 
                        plyr::round_any(x = set.breaks0[i], 
                                        accuracy = ifelse(max(set.breaks0[i])>300, 100, 
                                                          ifelse(max(set.breaks0[i])>100, 50, 
                                                                 ifelse(max(set.breaks0[i])>20, 10, 
                                                                        ifelse(max(set.breaks0[i])>10, 
                                                                               round(set.breaks0[i], digits = 0), 
                                                                               round(set.breaks0[i], digits = 1))))),
                                        f = ceiling))    
      }
    }
    set.breaks <- unique(set.breaks)
  }
  # if(set.breaks =="auto"){
  #   set.breaks <- quantile(as.numeric(unlist(dat[dat$year %in% yrs,var])), probs = c(0, .95))
  #   set.breaks <- plyr::round_any(x = set.breaks, 
  #                                 accuracy = ifelse(max(set.breaks)>300, 100, ifelse(max(set.breaks)>100, 50, 10)),
  #                                 f = ceiling)
  #   set.breaks <- seq(from = min(set.breaks), to = max(set.breaks), length.out = 5)
  # }
  
  # Select data and make plot
  for (ii in ifelse(workfaster,2,length(yrs)):1) {
    
    region <- "bs.south"
    if (SRVY == "NEBS"){
      temp1 <- cruises %>%
        dplyr::filter(year %in% yrs[ii]) %>%
        dplyr::select(SRVY) %>%
        unlist() %>%
        unique()
      region <- ifelse(sum(temp1 %in% "NBS")>0, "bs.all", "bs.south")
    }
    
    temp <- dat %>%
      dplyr::filter(year == yrs[ii]) 
    
    temp1 <- akgfmaps::make_idw_map(
      LATITUDE = as.numeric(unlist(temp[,lat])),
      LONGITUDE = as.numeric(unlist(temp[,lon])),
      CPUE_KGHA = as.numeric(unlist(temp[,var])),
      use.survey.bathymetry = FALSE,
      region = region, 
      out.crs = as.character(crs(reg_dat$bathymetry)),
      extrap.box = extrap.box, 
      set.breaks = set.breaks,
      grid.cell = c(ifelse(workfaster, 1.5, 0.02), 
                    ifelse(workfaster, 1.5, 0.02)), # 0.2x0.2 degree grid cells
      key.title = key.title)
    
    temp0 <- temp1[grid][[1]]  
    
    if (ii == ifelse(workfaster,2,length(yrs))) {
      stars_list <- temp0
      names(stars_list)[names(stars_list) == "var1.pred"] <- paste0("y", yrs[ii])  
    } else {
      stars_list$temp <- temp0$var1.pred
      names(stars_list)[names(stars_list) == "temp"] <- paste0("y", yrs[ii])   
    }
  }
  
  
  # stars_list0<-stars_list
  
  # https://rpubs.com/michaeldorman/646276
  stars_list <- stars_list %>% 
    dplyr::select(names(stars_list)[substr(start = 1, stop = 1, x = names(stars_list)) == "y"])
  names(stars_list)<-gsub(pattern = "y", replacement = "", x = names(stars_list))
  stars_list = stars::st_redimension(stars_list)
  names(stars_list) = "value"
  
  figure <- ggplot() +
    geom_stars(data = stars_list) +
    facet_wrap( ~ new_dim, nrow = nrow) +
    coord_equal() +
    geom_sf(data = reg_dat$survey.strata,
            color = "grey50",
            size = 0.1,
            alpha = 0,
            fill = NA) +
    geom_sf(data = reg_dat$graticule,
            color = "grey80",
            alpha = 0.2) +
    geom_sf(data = reg_dat$akland, 
            color = NA, 
            fill = "grey50") +
    scale_x_continuous(name = "", # "Longitude",
                       breaks = reg_dat$lon.breaks) +
    scale_y_continuous(name = "", # "Latitude",
                       breaks = reg_dat$lat.breaks) +
    coord_sf(xlim = reg_dat$plot.boundary$x, 
             ylim = reg_dat$plot.boundary$y)  +
    
    ggsn::scalebar(data = reg_dat$survey.grid,
                   location = "bottomleft",
                   dist = 150,
                   dist_unit = dist_unit,
                   transform = FALSE,
                   st.dist = .04,
                   height = 0.02,
                   st.bottom = TRUE,
                   st.size = 3,
                   model = reg_dat$crs) 
  
  if (grid == "continuous.grid") {
    figure <- figure + 
      scale_fill_viridis_c(option = col_viridis, 
                           #limits = range(set.breaks),
                           na.value = "transparent", 
                           breaks = set.breaks,
                           labels = set.breaks)  + 
      guides(fill=guide_colourbar(title=key.title, 
                                  title.position="top", 
                                  title.hjust = 0.5))
    
  } else if (grid == "extrapolation.grid") {
    # temp <- factor(x = temp0$var1.pred, levels = levels(temp0$var1.pred), labels = levels(temp0$var1.pred), ordered = T)
    figure <- figure +
      scale_fill_manual(
        values=c("gray90", 
                 viridis::mako(
                   direction = -1, 
                   n = temp1$n.breaks,
                   begin = 0,
                   end = 0.80)), 
        name = key.title,
        levels(temp0$var1.pred), 
        levels(temp0$var1.pred))      
  }
  
  figure <- figure +
    guides(
      fill = guide_legend(title.position="top", 
                          label.position = "bottom",
                          title.hjust = 0.5, nrow = 1)) +
    
    #set legend position and vertical arrangement
    theme( 
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      strip.background = element_blank(), 
      strip.text = element_text(size = 12, face = "bold"),
      legend.title = element_text(size = 12), 
      legend.text = element_text(size = 9),
      legend.background = element_rect(colour = "transparent", fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.title.align = .1, 
      legend.position="bottom", 
      legend.box.just = "left",
      legend.key.width = unit(.5, "in"), 
      legend.box = "horizontal")
  
  
  # #set legend position and vertical arrangement
  # theme(
  #   legend.title.align = 0.5, 
  #   panel.background = element_rect(fill = "white", 
  #                                   colour = NA), 
  #   panel.border = element_rect(fill = NA, 
  #                               colour = "grey20"), 
  #   strip.background = element_blank(), 
  #   strip.text = element_text(size = 12, face = "bold"),
  #   legend.title = element_text(size = 12), 
  #   legend.text = element_text(size = 9), ,
  #   legend.background = element_rect(colour = "transparent", fill = "transparent"),
  #   legend.key = element_rect(colour = "transparent", 
  #                             fill = "transparent"),
  #   # axis.text = element_blank(),
  #   legend.position="bottom", 
  #   legend.box.just = "left",
  #   legend.key.width = unit(.5, "in"), 
  #   legend.box = "horizontal"
  # )
  
  return(figure)
  
}





#' Plot temperature facet grid
#' 
#' Generate multipanel temperature plot from a raster brick.
#' 
#' @param rasterbrick raster layer as a RasterBrick class
#' @param key.title Title for multipanel legend as a character vector or expression 
#' @param reg_data list containing regional survey strata, land polygon, etc. from akgfmaps::get_base_layer()
#' @param colorbar_breaks numeric vector of breaks to use for temperature plots
#' @param viridi_palette_option Viridis palette option passed to viridis::viridis_pal(). Default = "H" (turbo)
#' @export
plot_temps_facet <- function(rasterbrick, 
                             key.title = "Temperature (°C)", 
                             reg_dat, 
                             colorbar_breaks = c(-Inf, seq(from = 0, to = 14, by = 2), Inf),
                             dist_unit = "nm", # nautical miles
                             viridis_palette_option = "H") {
  
  temp <- projectRaster(rasterbrick, crs = crs(reg_dat$akland))
  temp_spdf <- as(temp, "SpatialPixelsDataFrame")
  temp_df <- as.data.frame(temp_spdf)
  temp1 <- gsub(pattern = "[A-Za-z]+", 
                replacement = "", 
                x = names(temp_df[!(names(temp_df) %in% c("x", "y"))]))
  temp1 <- gsub(pattern = "_", replacement = "", x = temp1)
  colnames(temp_df) <- c(temp1, "x", "y")
  temp_df <- temp_df %>% 
    tidyr::pivot_longer(values_to = "value", 
                        names_to = "year", 
                        cols = temp1)
  
  fig_palette <- viridis::viridis_pal(option = viridis_palette_option)(length(colorbar_breaks)-1)
  
  figure <- ggplot() +
    geom_tile(data=temp_df, aes(x=x, y=y, fill=cut(value, breaks = colorbar_breaks)))  +
    facet_wrap( ~ year, 
                nrow = ifelse(length(names(rasterbrick))>=4, 2, 1)) +
    coord_equal() +
    scale_fill_manual(values = fig_palette) +
    geom_sf(data = reg_dat$survey.strata,
            color = "grey50",
            size = 0.1,
            alpha = 0,
            fill = NA) +
    geom_sf(data = reg_dat$graticule,
            color = "grey80",
            alpha = 0.2) +
    geom_sf(data = reg_dat$akland, 
            color = NA, 
            fill = "grey50") +
    scale_x_continuous(name = "",#"Longitude",
                       breaks = reg_dat$lon.breaks) +
    scale_y_continuous(name = "",#Latitude",
                       breaks = reg_dat$lat.breaks) +
    coord_sf(xlim = reg_dat$plot.boundary$x, 
             ylim = reg_dat$plot.boundary$y)  +
    
    ggsn::scalebar(data = reg_dat$survey.grid,
                   location = "bottomleft",
                   dist = 150,
                   dist_unit = dist_unit,
                   transform = FALSE,
                   st.dist = .04,
                   height = 0.02,
                   st.bottom = TRUE,
                   st.size = 3,
                   model = reg_dat$crs) +
    #set legend position and vertical arrangement
    guides(#colour = guide_colourbar(title.position="top", title.hjust = 0.5),
      fill = guide_legend(title.position="top", 
                          label.position = "bottom",
                          title.hjust = 0.5, nrow = 1)) +
    
    theme(
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      strip.background = element_blank(), 
      strip.text = element_text(size = 12, face = "bold"),
      legend.position = "none"
    )
  
  
  #   Turbo represents a tradeoff between interpretability and accessibility. If it seems like that won't be accessible because of how it's distributed (e.g., faxing), then by all means change it, because it doesn't have linear luminosity and chromaticity. For temperatures, I think it's pretty difficult to distinguish shades of blue. So if you're going to choose an alternative palette, I think it would be great if the cold pool is black (where < 0 is black; e.g., Which would be magma or inferno, whcih have a larger luminance gradient)
  
  # Turbo color map:
  # https://github.com/sjmgarnier/viridis/issues/65
  
  # https://stackoverflow.com/questions/50506832/create-discrete-color-bar-with-varying-interval-widths-and-no-spacing-between-le
  # https://stackoverflow.com/questions/64013935/custom-color-palette-for-scale-fill-fermenter
  
  # cold_pool_cbar 
  # https://github.com/sean-rohan-NOAA/coldpool/blob/main/1_cold_pool_index.Rmd#L169 
  cbar_legend <- legend_discrete_cbar(breaks = colorbar_breaks,
                                      colors = fig_palette,
                                      legend_direction = "horizontal",
                                      font_size = 3,
                                      width = 0.1,
                                      expand_size.x = 0.3,
                                      expand_size.y = 0.3,
                                      expand.x = 0.3,
                                      expand.y = 0.9,
                                      spacing_scaling = 1,
                                      text.hjust = 0.5,
                                      text.vjust = 0.5,
                                      font.family = "sans",
                                      neat.labels = FALSE) + 
    annotate("text", 
             x = 1.15, 
             y = mean(colorbar_breaks[!is.infinite(colorbar_breaks)]), 
             label = key.title, 
             size = rel(3.2)) + 
    theme(plot.margin = unit(c(-5,5,5, 5), units = "mm"))
  
  figure_and_legend <- cowplot::plot_grid(figure,
                                          cbar_legend,
                                          nrow = 2,
                                          rel_heights = c(0.8,0.2))
  
  return(figure_and_legend)
  
}



#' Discrete continuous bar
#'
#' Generate a continuous bar plot using ggplot functions
#'
#' @param breaks Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
#' @param palette Character vector indicating the name of the RColorBrewer palette to use. Alternatively, can pass a vector of colors to the colors argument.
#' @param colors A vector of colors
#' @export

legend_discrete_cbar <- function(
  breaks, # Vector of breaks. If +-Inf are used, triangles will be added to the sides of the color bar
  palette = "Greys", #
  direction = 1, # Flip colors? Can be 1 or -1
  colors = RColorBrewer::brewer.pal(length(breaks) - 1, palette),
  spacing = "natural", # Spacing between labels. Can be "natural" or "constant"
  border_color = NA, # NA = no border color
  legend_title = NULL,
  legend_direction = "horizontal", # Can be "horizontal" or "vertical"
  font_size = 5,
  expand_size.x = 1,
  expand_size.y = 1,# Controls spacing around legend plot
  expand.y = 1,
  expand.x = 1,
  spacing_scaling = 1, # Multiplicative factor for label and legend title spacing
  width = 0.1, # Thickness of color bar
  triangle_size = 0.1, # Relative width of +-Inf triangles
  title_pos = NULL,
  text.angle = NULL,
  text.vjust = NULL,
  text.hjust = NULL,
  text.color = "black",
  neat.labels = FALSE,
  font.family = "serif") {
  
  
  require(ggplot2)
  if (!(spacing %in% c("natural", "constant"))) stop("spacing must be either 'natural' or 'constant'")
  if (!(direction %in% c(1, -1))) stop("direction must be either 1 or -1")
  if (!(legend_direction %in% c("horizontal", "vertical"))) stop("legend_direction must be either 'horizontal' or 'vertical'")
  breaks = as.numeric(breaks)
  new_breaks = sort(unique(breaks))
  if (any(new_breaks != breaks)) warning("Wrong order or duplicated breaks")
  breaks = new_breaks
  if (class(colors) == "function") colors = colors(length(breaks) - 1)
  if (length(colors) != length(breaks) - 1) stop("Number of colors (", length(colors), ") must be equal to number of breaks (", length(breaks), ") minus 1")
  if (!missing(colors)) warning("Ignoring RColorBrewer palette '", palette, "', since colors were passed manually")
  
  if (direction == -1) colors = rev(colors)
  
  inf_breaks = which(is.infinite(breaks))
  if (length(inf_breaks) != 0) breaks = breaks[-inf_breaks]
  plotcolors = colors
  
  n_breaks = length(breaks)
  
  labels = breaks
  
  if (spacing == "constant") {
    breaks = 1:n_breaks
  }
  
  r_breaks = range(breaks)
  
  cbar_df = data.frame(stringsAsFactors = FALSE,
                       y = breaks,
                       yend = c(breaks[-1], NA),
                       color = as.character(1:n_breaks)
  )[-n_breaks,]
  
  xmin = 1 - width/2
  xmax = 1 + width/2
  
  cbar_plot = ggplot(cbar_df, aes(xmin=xmin, xmax = xmax, ymin = y * expand.y, ymax = yend * expand.y, fill = factor(color, levels = 1:length(colors)))) +
    geom_rect(show.legend = FALSE,
              color=border_color)
  
  if (any(inf_breaks == 1)) { # Add < arrow for -Inf
    firstv = breaks[1]
    polystart = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(firstv, 2), firstv - diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-1]
    cbar_plot = cbar_plot +
      geom_polygon(data=polystart, aes(x=x, y=y * expand.y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[1],
                   color=border_color)
  }
  if (any(inf_breaks > 1)) { # Add > arrow for +Inf
    lastv = breaks[n_breaks]
    polyend = data.frame(
      x = c(xmin, xmax, 1),
      y = c(rep(lastv, 2), lastv + diff(r_breaks) * triangle_size)
    )
    plotcolors = plotcolors[-length(plotcolors)]
    cbar_plot = cbar_plot +
      geom_polygon(data=polyend, aes(x=x, y=y * expand.y),
                   show.legend = FALSE,
                   inherit.aes = FALSE,
                   fill = colors[length(colors)],
                   color=border_color)
  }
  
  if(is.null(text.angle)) {
    text.angle <- 0
  }
  
  if(is.null(text.hjust)) {
    text.hjust <- 0.5
  }
  
  if(is.null(text.vjust)) {
    text.vjust <- 0.5
  }
  
  if (legend_direction == "horizontal") { #horizontal legend
    mul = 1
    x = xmin
    xend = xmax
    cbar_plot = cbar_plot + coord_flip()
    angle = 0
    if(xmax > 0) {
      legend_position = xmax + 0.1 * spacing_scaling
    } else {
      legend_position = xmax + -0.1 * spacing_scaling
    }
    
  } else { # vertical legend
    mul = -1
    x = xmax
    xend = xmin
    angle = -90
    legend_position = xmax + 0.2 * spacing_scaling
  }
  
  if(neat.labels) {
    labels <- format(labels, nsmall = 1)
  }
  
  # print(c(min(x - 0.05 * mul * spacing_scaling), max(x * mul * spacing_scaling)))
  cbar_plot = cbar_plot +
    geom_segment(data = data.frame(y = breaks * expand.y,
                                   yend = breaks * expand.y),
                 aes(y=y,
                     yend=yend),
                 x = x - 0.05 * mul * spacing_scaling,
                 xend = xend,
                 inherit.aes = FALSE,
                 color = text.color) +
    annotate(geom = 'text',
             x = x - 0.1 * mul * spacing_scaling,
             y = breaks * expand.y,
             label = labels,
             angle = text.angle,
             hjust = text.hjust,
             vjust = text.vjust,
             size = font_size,
             color = text.color,
             family = font.family) +
    scale_x_continuous(expand = c(expand_size.x, expand_size.x)) +
    scale_y_continuous(expand = c(expand_size.y, expand_size.y)) +
    scale_fill_manual(values=plotcolors) +
    theme_void()
  
  if(is.null(title_pos)) {
    title_pos <- mean(r_breaks)
  }
  
  if (!is.null(legend_title)) { # Add legend title
    cbar_plot = cbar_plot +
      annotate(geom = 'text', x = legend_position, y = title_pos,
               label = legend_title,
               angle = angle,
               size = font_size,
               family = font.family)
  }
  
  return(cbar_plot)
}

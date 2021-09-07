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


# Spatial-----------------------------------------------

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




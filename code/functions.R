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
  "NMFSReports", # devtools::install_github("emilyhmarkowitz/NMFSReports") # Package of my favorite grammar and file managment functions for writing reproducible reports

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

# General stuff

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

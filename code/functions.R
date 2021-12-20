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
  "ggridges",
  
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
  
  # check website links
  "pingr",
  "httr",
  
  
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



readtext2 <- function(file, refcontent = FALSE){
  
  # read in document
  insert <- readtext(file = file)$text
  
  if (insert == "") { # if there is nothing in the doc, no problem!
    insert <- ""
  } else { # if there is something in the doc
    
    # remind reviewers what is code and what is not:
    if (refcontent) { 
      insert <- paste0("**Hand written text from google drive**: ", 
                       gsub(pattern = "\n", replacement = "\n\n\n **Hand written text from google drive**:", x = insert), 
                       "")
    } else {
      insert <- gsub(pattern = "\n", replacement = "\n\n\n", x = insert)
    }
    
    # incorportate r code in the text
    # insert <- strsplit(x = insert, split = "`r")[[1]]
    insert <- strsplit(x = insert, split = "`")[[1]]
    insert[substr(start = 1, stop = 2, x = insert) != "r "] <-
      paste0('"', insert[substr(start = 1, stop = 2, x = insert) != "r "], '",')
    # paste0("'", insert[substr(start = 1, stop = 2, x = insert) != "r "], "',")
    insert[substr(start = 1, stop = 2, x = insert) == "r "] <-
      paste0(substr(x = insert[substr(start = 1, stop = 2, x = insert) == "r "] ,
                    start = 3,
                    stop = nchar(insert[substr(start = 1, stop = 2, x = insert) == "r "] )), ",")
    insert <- paste0(insert, collapse = "")
    insert<-paste0(substr(x = insert, start = 1, stop = nchar(insert)-1)) # get rid of ",)" at end of last paragraph
    insert <- paste0("paste0(", insert, ")", collapse = "")
    insert <- eval(parse( text= insert ))
    
  }
  return(insert)
}


library(pingr)

#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the
#'        HTTP status code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value` condition
#'        arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly
#'        to `httr::HEAD()` and/or `httr::GET()`
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE,...) {
  # https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
  suppressPackageStartupMessages({
    require("httr", quietly = FALSE, warn.conflicts = FALSE)
  })
  
  # you don't need thse two functions if you're alread using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.
  
  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)
        
        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }
  
  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }
  
  sHEAD <- safely(httr::HEAD)
  sGET <- safely(httr::GET)
  
  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)
  
  if (is.null(res$result) || 
      ((httr::status_code(res$result) %/% 200) != 1)) {
    
    res <- sGET(x, ...)
    
    if (is.null(res$result)) return(NA) # or whatever you want to return on "hard" errors
    
    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet) warning(sprintf("Requests for [%s] responded but without an HTTP status code in the 200-299 range", x))
      return(non_2xx_return_value)
    }
    
    return(TRUE)
    
  } else {
    return(TRUE)
  }
  
}


# c(
#   "http://content.thief/",
#   "http://rud.is/this/path/does/not_exist",
#   "https://www.amazon.com/s/ref=nb_sb_noss_2?url=search-alias%3Daps&field-keywords=content+theft", 
#   "https://www.google.com/search?num=100&source=hp&ei=xGzMW5TZK6G8ggegv5_QAw&q=don%27t+be+a+content+thief&btnK=Google+Search&oq=don%27t+be+a+content+thief&gs_l=psy-ab.3...934.6243..7114...2.0..0.134.2747.26j6....2..0....1..gws-wiz.....0..0j35i39j0i131j0i20i264j0i131i20i264j0i22i30j0i22i10i30j33i22i29i30j33i160.mY7wCTYy-v0", 
#   "https://rud.is/b/2018/10/10/geojson-version-of-cbc-quebec-ridings-hex-cartograms-with-example-usage-in-r/"
# ) -> some_urls
# 
# data.frame(
#   exists = sapply(some_urls, url_exists, USE.NAMES = FALSE),
#   some_urls,
#   stringsAsFactors = FALSE
# ) %>% dplyr::tbl_df() %>% print()


find_units <- function(unit = "", unt = "", dat, divby = NULL){
  
  # x <- ifelse(unit == "", "s", paste0(" ", unit))
  x <- unit#ifelse(unit != "", paste0(" ", unit), unit)
  x_ <- ifelse(unt =="", "", unt)
  
  # find appropriate units
  
  if (is.null(divby)) {
    min_val <- min(dat, na.rm = TRUE)
    min_val1 <- xunits(min_val, words = TRUE)
  } else {
    min_val <- divby
    min_val1 <- xunits(divby, words = TRUE)
  }
  
  
    if (min_val<1e3) {
      divby <- 1
      unit_word <- ifelse(unit == "", "", paste0(" (", x, ")"))
      unit_wrd <- paste0("", x_)
    } else if (min_val<1e6) {
      divby <- 1e3
      unit_word <- paste0(" (thousand",
                          ifelse(unit == "", "s", paste0(" ", unit)),
                          ")" )
      unit_wrd <- paste0("K", x_)
    } else if (grepl(pattern = "million", x = min_val1)) {
      divby <- 1e6
      unit_word <- paste0(" (million",
                          ifelse(unit == "", "s", paste0(" ", unit)),
                          ")")
      unit_wrd <- paste0("M", x_)
    } else if (grepl(pattern = "billion", x = min_val1)) {
      divby <- 1e9
      unit_word <- paste0(" (billion",
                          ifelse(unit == "", "s", paste0(" ", unit)),
                          ")")
      unit_wrd <- paste0("B", x_)
    } else if (grepl(pattern = "trillion", x = min_val1)) {
      divby <- 1e12
      unit_word <- paste0(" (trillion",
                          ifelse(unit == "", "s", paste0(" ", unit)),
                          ")")
      unit_wrd <- paste0("T", x_)
    }
    
  
  return(list("divby" = divby, 
              "unit_word" = unit_word, 
              "unit_wrd" = unit_wrd))
}

# Converions --------------------------------------------


# https://github.com/geanders/weathermetrics/blob/master/R/temperature_conversions.R
c2f <- function (T.celsius, round = 2) {
    T.fahrenheit <- (9/5) * T.celsius + 32
    T.fahrenheit <- round(T.fahrenheit, digits = round)
    return(T.fahrenheit)
  }

# divkmfornmi <- 
# 
# divnmiforkm
# 
divnmi2forkm2 <- 1/3.429904

divkm2fornmi2 <- 3.429904
# divkm2fornmi2 <- 0.291160601164372384405766883164727405629748185875095639378254783272052516569336

divkm2forha <- 100

divmforft <- 0.3048
  
divftform <- 3.28084
  
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


species_table <- function(haul_spp, 
                          spp_print, 
                          # spp_sci,
                          SURVEY000, 
                          SRVY000 = NA) {
  
  header <- paste0("Summary of environmental variables that ", spp_print, " (", spp_sci, ") have been found in across the ", SURVEY000, ifelse(sum(SRVY000 %in% c("NBS", "EBS"))==2, "NEBS", paste0(" (", SRVY000, ")")))
  
  # Select data and make plot
  cols<-c("start_latitude", "start_longitude",  #"weight", "number_fish", 
          "bottom_depth", "gear_temperature", "surface_temperature")
  COLS<-c("Latitude", "Longitude", 
          # "Weight", "Abundance", 
          "Bottom Depth", "Bottom Temperature", "Surface Temperature")
  
  haul_spp <- haul_spp %>% 
    dplyr::filter(SRVY %in% SRVY000)
  
  # if (nrow(haul_spp)==0) {
  
  # basiccontent0<-c()
  table_spp<-c()
  
  for (ii in 1:length(cols)) {
    table_spp<-rbind.data.frame(table_spp, 
                                data.frame(metric0 = cols[ii], 
                                           Metric = COLS[ii], 
                                           Min = ifelse((nrow(haul_spp)==0), NA, min(haul_spp[,cols[ii]], na.rm = T)), 
                                           Max = ifelse((nrow(haul_spp)==0), NA, max(haul_spp[,cols[ii]], na.rm = T)), 
                                           Mean = ifelse((nrow(haul_spp)==0), NA, sum(haul_spp[,cols[ii]], na.rm = T)/nrow(haul_spp))
                                ))
  }
  
  table_spp_print <- table_spp
  
  if (nrow(haul_spp) != 0) {
    for (ii in c("Min", "Max", "Mean")){
      table_spp_print[,ii] <- 
        trimws(formatC(x = table_spp_print[,ii], 
                       big.mark = ",", 
                       digits = 2, format = "f"))
    }
  }
  table_spp_print$metric0<-NULL
  
  # table_raw = table_spp
  # table_print = table_spp_print
  
  return(list("header" = header, 
              "raw" = table_spp, 
              "print" = table_spp_print))
}


species_text <- function(
  table_spp_print, 
  haul0, 
  biomass_cpue,
  length_maxyr0, 
  # spp_sci,
  spp_print, 
  spp_code, 
  SRVY000, 
  maxyr, 
  compareyr, 
  biomass_cpue_tab_name) {
  
  haul_maxyr0 <- haul0 %>% 
    dplyr::filter(SRVY %in% SRVY000 &
                    year == maxyr) 
  
  haul_maxyr_spp <- haul_maxyr0 %>% 
    dplyr::filter(species_code %in% spp_code)  
  
  haul_compareyr_spp <- haul0 %>% 
    dplyr::filter(species_code %in% spp_code & 
                    SRVY %in% SRVY000 &
                    year == compareyr)  
  
  biomass_cpue <- biomass_cpue %>% 
    dplyr::filter(SRVY %in% SRVY000)    
  
  biomass_cpue_spp <- biomass_cpue %>%
    dplyr::filter(print_name %in% spp_print & 
                    SRVY %in% SRVY000)
  
  str <- c()
  
  # <!-- how many stations -->
  str <- paste0(str, 
                "Out of the total number of successful hauls (", 
                length(unique(haul_maxyr0$hauljoin)), ") ",  
                spp_print, 
                " was found during ", 
                length(unique(haul_maxyr_spp$hauljoin)), 
                " hauls (", 
                formatC(x = (length(unique(haul_maxyr_spp$hauljoin))/length(unique(haul_maxyr0$hauljoin)))*100, digits = 1, format = "f"), 
                "% of stations). ")
  
  # different version of above
  str <- paste0(str, "

During the ", maxyr, " survey, ", spp_print, " were present at ", 
formatC(x = (length(unique(haul_maxyr_spp$hauljoin))/length(unique(haul_maxyr0$hauljoin)))*100, 
        digits = 1, format = "f") , 
"% of stations in the ", 
ifelse(sum(SRVY000 %in% c("NBS", "EBS"))==2, "NEBS", SRVY000), " (", 
length(unique(haul_maxyr_spp$hauljoin)), " of ", 
length(unique(haul_maxyr0$hauljoin)), 
" stations). ")
  
  # <!-- bottom tempature -->
  str <- paste0(str, "

", stringr::str_to_sentence(spp_print), 
" were found in bottom temperatures as warm as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Temperature") %>% dplyr::select(Max)) , 
"\u00B0C and as cold as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Temperature") %>% dplyr::select(Min)) , 
"\u00B0C. ") #  (Figure ", cnt_figures,")
  
  # <!-- surface temperature -->
  str <- paste0(str, "

", stringr::str_to_sentence(spp_print), 
" were found in areas where surface temperatures were as warm as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Surface Temperature") %>% dplyr::select(Max)) , 
"\u00B0C and as cold as ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Surface Temperature") %>% dplyr::select(Min)) , 
"\u00B0C. ") #  (Figure ", cnt_figures,")
  
  # <!-- Depth -->
  str <- paste0(str, "

They were found in waters with depths between ", 
as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Depth") %>% dplyr::select(Min)) , 
" m and ", as.numeric(table_spp_print %>% dplyr::filter(Metric == "Bottom Depth") %>% dplyr::select(Max)) , " m. ")
  
  # <!-- Sizes lengths caught  -->
  
  if (nrow(length_maxyr0) != 0) {
    
    
    unit <- unique(dplyr::case_when(spp_code %in% 1:31550 ~ 'cm', 
                                    spp_code %in% 68000:69930 ~ 'mm'), 
                   TRUE ~ 'NO MEASUREMENT')
    
    str <- paste0(str, "

The ", 
NMFSReports::text_list(unique(length_maxyr0$sentancefrag)), 
" of ", spp_print, 
" measured during the ",maxyr," ",
ifelse(sum(SRVY000 %in% c("NBS", "EBS"))==2, "NEBS", SRVY000),
" survey were between ", 
# NMFSReports::xunits(
(min(length_maxyr0$length, na.rm = TRUE)/ifelse(unit == "cm", 10, 1)), 
" and ", #NMFSReports::xunits
(max(length_maxyr0$length, na.rm = TRUE)/ifelse(unit == "cm", 10, 1)), 
" ", unit, ". ")
  }
  
  
  # <!-- weight -->
  #   str <- paste0(str, "
  # 
  # The total number of ", spp_print,
  # " estimated to have been caught by the survey is ",
  # NMFSReports::xunits(value = sum(haul_maxyr_spp$number_fish, na.rm = TRUE)),
  # " individuals equating to ",
  # NMFSReports::xunits(value = sum(haul_maxyr_spp$weight, na.rm = TRUE)),
  # " kg of catch. ")
  # 
  #   str <- paste0(str, "
  # 
  # Compared with ", compareyr, ", abundance experienced ",
  # NMFSReports::pchange(start = sum(haul_compareyr_spp$number_fish, na.rm = TRUE),
  #                      end = sum(haul_maxyr_spp$number_fish, na.rm = TRUE)) ,
  # " and there was ",
  # NMFSReports::pchange(start = sum(haul_compareyr_spp$weight, na.rm = TRUE),
  #                      end = sum(haul_maxyr_spp$weight, na.rm = TRUE)) ,
  # " in biomass. ")
  
  
  tempyr <- max(nbsyr[!(nbsyr %in% c(maxyr, compareyr))])  # the other year we are comparing to
  
  temp <- function(biomass_cpue, biomass_cpue_spp, maxyr, compareyr, tempyr, 
                   metric = "biomass", metric_long = "biomass", unit = " mt", 
                   SRVY000, spp_print) {
    
    str0 <- paste0("Compared with ", 
                   compareyr, " (", xunits(sum(biomass_cpue_spp[biomass_cpue_spp$year == compareyr, metric], na.rm = TRUE), val_under_x_words = NULL), 
                   unit,"), ",spp_print," ",metric_long," in ", 
                   maxyr, " (", xunits(sum(biomass_cpue_spp[biomass_cpue_spp$year == maxyr, metric], na.rm = TRUE), val_under_x_words = NULL), 
                   unit,") in the ",
                   ifelse(sum(SRVY000 %in% c("NBS", "EBS"))==2, "NEBS", SRVY000)," experienced ",
                   NMFSReports::pchange(start = sum(biomass_cpue_spp[biomass_cpue_spp$year == compareyr, metric], na.rm = TRUE),
                                        end = sum(biomass_cpue_spp[biomass_cpue_spp$year == maxyr, metric], na.rm = TRUE)) , 
                   # compare to the year before these
                   ". Previously, ",spp_print," ",metric_long," in ", 
                   compareyr, " experienced ",
                   NMFSReports::pchange(start = sum(biomass_cpue_spp[biomass_cpue_spp$year == tempyr, metric], na.rm = TRUE),
                                        end = sum(biomass_cpue_spp[biomass_cpue_spp$year == compareyr, metric], na.rm = TRUE)), 
                   " compared to ",metric_long," in ", 
                   tempyr, " (", xunits(sum(biomass_cpue_spp[biomass_cpue_spp$year == tempyr, metric], na.rm = TRUE), val_under_x_words = NULL), unit,").")
    
    return(str0)
  }
  
  
  if ( (nrow(biomass_cpue_spp) != 0) ) {
    ## pchange_biomass
    str <- paste0(str, "

", temp(biomass_cpue, biomass_cpue_spp, maxyr, compareyr, tempyr, metric = "biomass", metric_long = "biomass", unit = " mt", SRVY000, spp_print))
    
    
    
    # pchange cpue
    #     str <- paste0(str, "
    # 
    # ", temp(biomass_cpue, biomass_cpue_spp, maxyr, compareyr, tempyr, metric = "cpue_kgha", metric_long = "CPUE", unit = " kg/ha", SRVY000, spp_print))
    
    
    
    # Yellowfin sole biomass increased by 22% between 2017 and 2019 in the NBS area.
    # In 2019, the Alaska plaice exhibited a slight 0.8% decrease (321,571 mt; Table 1) in the total NBS survey biomass compared with 2017; however, biomass in 2019 was 24% greater than in 2010.
    # Biomass of the purple-orange sea star increased by 26% between 2017 and 2019 and by 40% between 2010 and 2019.
    # The estimated biomass of the northern Neptune snail decreased by 18% (Table 1) between 2017 (327,678 mt) and 2019 (146,344 mt). However, the biomass was 32% greater in 2019 than in 2010.
    # There was a 10% reduction in saffron cod biomass in 2019 from 2010 (Table 1), but a 6% increase in saffron cod biomass from 2017 to 2019.
    # Between 2010 and 2019, there was a 99.8% reduction in Arctic cod biomass in the NBS.
    # This represents a 5,421% increase from the biomass observed in 2010.
    # Biomass of this skate increased 24% from 2010 to 2019.
    # 25% increase (2,827 mt) in the estimated biomass of red king crab compared to 2017 (2,256 mt). The increase in biomass in 2019 relative to 2010 was slightly less (13%) (Table 1). 
    # Blue king crab biomass decreased by 79% from 2017 to 2019. Biomass in 2019 (1,212 mt) was more similar to 2010 (2,133 mt) (Table 1).
    # Pacific halibut showed an estimated 42% increase in total biomass from 2017 (18,507 m) to 2019 (25,722 mt; Table 1). 
    # Biomass of Bering flounder increased by 50% between 2010 and 2019 (12,355 mt to 18,526 mt; Table 1).
    # The relative Pacific herring biomass increased 282% from 23,011 mt in 2010 to 87,918 mt in 2019 (Table 1).
    
    # ## p_mt_of_survey	
    metric <- "biomass"
    metric_long <- "biomass"
    unit <- " mt"
    
    # total biomass
    temp2 <- biomass_cpue  %>%
      dplyr::ungroup() %>% 
      dplyr::select(biomass, year) %>%
      dplyr::group_by(year) %>% 
      dplyr::summarise(biomass = sum(biomass, na.rm = T)) 
    
    str <- paste0(str, "
  
In ",maxyr,", ",spp_print," comprised ",
xunitspct((sum(biomass_cpue_spp[biomass_cpue_spp$year == maxyr, metric], na.rm = TRUE)/
             temp2$biomass[temp2$year == maxyr])*100)," (", 
xunits(sum(biomass_cpue_spp[biomass_cpue_spp$year == maxyr, metric], na.rm = TRUE), val_under_x_words = NULL), unit,", Table",ifelse(length(SRVY000)>1, "s", "")," ",
NMFSReports::text_list(NMFSReports::crossref(list_obj = list_tables, nickname = biomass_cpue_tab_name, sublist = "number")),
") of the ",
ifelse(sum(SRVY000 %in% c("NBS", "EBS"))==2, "NEBS", SRVY000),
" survey biomass. ",

"Previously in ",compareyr,", ",spp_print," comprised ",
xunitspct((sum(biomass_cpue_spp[biomass_cpue_spp$year == compareyr, metric], na.rm = TRUE)/
             temp2$biomass[temp2$year == compareyr])*100)," (", 
xunits(sum(biomass_cpue_spp[biomass_cpue_spp$year == compareyr, metric], na.rm = TRUE), val_under_x_words = NULL), unit,", Table",ifelse(length(SRVY000)>1, "s", "")," ",
NMFSReports::text_list(NMFSReports::crossref(list_obj = list_tables, nickname = biomass_cpue_tab_name, sublist = "number")),
") of the ",
ifelse(sum(SRVY000 %in% c("NBS", "EBS"))==2, "NEBS", SRVY000),
" survey biomass. ")
    
    
    
    # comprising 12% (520,029 mt; Table 1) of the total NBS survey area biomass - YFS
    # In 2019, snow crab comprised 4% (167,124 mt, Table 1) of the NBS survey biomass... In 2017, snow crab comprised 5% (223,216 mt) of the NBS survey biomass
    # This species of sea star made up 10% (414,423 mt, Table 1) of the 2019 total fish and invertebrate biomass in the NBS. 
    # Saffron cod represented almost 2% of the biomass in the 2019 NBS.
    # Arctic cod represented approximately 0.001% of the 2019 NBS biomass, 0.1% of the 2017 biomass and 1% of the 2010 biomass.
    # Pacific cod represented about 9% of the biomass in the 2019 NBS survey; this represents a 29% increase from 2017 NBS Pacific cod biomass.
    # Walleye pollock represented 27% of the total NBS biomass in 2019.
    # 
    # ## size_comp
    # In 2010 and 2017, size compositions were similar with length cohort modes around 24 cm and 36 cm, while in 2019 the size class modes are less distinct and the smallest size class mode appears from 18-22cm (Figure 7).
    # A similar size composition was observed in 2010 and 2017 (Figure 23).
    
    
  }
  
  return(str)
  
}

species_content <- function(SURVEY000, 
                            SRVY000, 
                            haul0, 
                            biomass_cpue,
                            length_maxyr0,
                            # spp_sci,
                            spp_print, 
                            spp_code,
                            maxyr, 
                            compareyr, 
                            biomass_cpue_tab_name = "tab_majortaxa_pchange") {
  
  
  # Data work up
  
  # haul_maxyr0 <- haul0 %>% 
  #   dplyr::filter(SRVY %in% SRVY000 &
  #                   year == maxyr)
  
  
  length_maxyr0 <- length_maxyr0 %>% 
    dplyr::filter(species_code %in% spp_code & 
                    SRVY %in% SRVY000)
  
  # tables from maxyr
  table_spp_maxyr <- 
    species_table(haul_spp = haul0 %>% 
                    dplyr::filter(year == maxyr &
                                    species_code %in% spp_code), 
                  spp_print, 
                  # spp_sci,
                  SURVEY000 = SURVEY000, 
                  SRVY000 = SRVY000) 
  
  # tables from compareyr
  table_spp_compareyr <- 
    species_table(haul_spp =  haul0 %>% 
                    dplyr::filter(year == compareyr &
                                    species_code %in% spp_code), 
                  spp_print, 
                  # spp_sci,
                  SURVEY000 = SURVEY000,
                  SRVY000 = SRVY000) 
  
  
  table_spp <- dplyr::full_join(
    x = table_spp_maxyr$print %>% 
      dplyr::rename(
        min_maxyr = Min, 
        max_maxyr = Max, 
        ean_maxyr = Mean), 
    y = table_spp_compareyr$print %>% 
      dplyr::rename(
        min_compareyr = Min, 
        max_compareyr = Max, 
        mean_compareyr = Mean), 
    by = "Metric") %>% 
    flextable::flextable(.) %>%
    NMFSReports::theme_flextable_nmfstm()
  
  text_spp <- species_text(
    haul0, 
    biomass_cpue,
    length_maxyr0, 
    table_spp_print = table_spp_maxyr$print, 
    # spp_sci,
    spp_print, 
    spp_code, 
    SRVY000, 
    maxyr, 
    compareyr, 
    biomass_cpue_tab_name)
  
  return(list("table_spp" = table_spp, 
              "text_spp" = text_spp))
}



#' Make a string lower case except for stated (and common NOAA) proper nouns.
#'
#' Make a string lower case except for stated (and common NOAA) proper nouns.
#' @param str0 The text string.
#' @param capitalizefirst Default = FALSE
#' @param add_cap A vector of strings that the user does not want capitalized
#' @keywords Text editing
#' @export
#' @examples
#' tolower2(str0 = "notice how there are built-in proper nouns are capitalized:
#' alaska is not in the south atlantic.",
#'          capitalizefirst = TRUE,
#'          add_cap = "Proper nouns")
# tolower2<-function(str0,
#                    capitalizefirst = FALSE,
#                    add_cap = "") {
#   str2<-c()
# 
#   if (str0[1] %in% "") {
#     str<-""
#   } else {
#     for (i in 1:length(str0)) {
#       str1<-gsub(pattern = "\\(", replacement = "\\( ", x = tolower(str0[i]))
#       str1<-gsub(pattern = "\\)", replacement = " \\)", x = str1)
#       str1<-strsplit(x = str1, split = " ")[[1]]
#       # str1<-gsub(pattern = "fw", replacement = "freshwater", x = str1, ignore.case = T)
# 
#       keywords <- c( add_cap, #user added
#                      #State
#                      "Alabama", "Alaska", "California", "Connecticut",
#                      "Delaware", #"East Florida", "West Florida",
#                      "Florida", "Georgia",
#                      "Louisiana", "Maine", "Maryland", "Massachusetts",
#                      "Mississippi", "New Hampshire", "New Jersey", "New York",
#                      "North Carolina", "Oregon", "Rhode Island", "South Carolina",
#                      "Texas",  "Virginia", "Washington",
#                      #Region
#                      "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "Western Pacific",
#                      "New England",
#                      "Mid-Atlantic","Gulf of Mexico",
#                      "South Atlantic",
#                      #For specific Species
#                      "Spanish", "Gulf", "Bringham's", "Von Siebold's", "Pfluger's", "African", "Eurpoean",
#                      "Southern kingfish", "Southern flounder",
#                      # Other
#                      "Atlantic", "American",
#                      # "Atka",
#                      "Chinook", "Great Lakes")
# 
#       # keywords<-c(keywords, paste0("(", keywords), paste0(keywords, ")"))
# 
# 
#       for (ii in 1:length(keywords)) {
#         keywords1<-strsplit(x = keywords[ii], split = " ")[[1]]
#         if (length(keywords1) %in% 1 &
#             sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T))>0) {
#           str1[grep(x = str1, pattern = keywords[ii], ignore.case = T)]<-keywords[ii]
#         } else if (length(keywords1) %in% 2 &
#                    sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T)>0) &
#                    sum(grepl(x = str0, pattern = keywords1[2], ignore.case = T)>0)) {
#           str1[grep(x = str1, pattern = keywords1[1], ignore.case = T)]<-keywords1[1]
#           str1[grep(x = str1, pattern = keywords1[2], ignore.case = T)]<-keywords1[2]
#         } else if (length(keywords1) %in% 3 &
#                    grepl(x = str0, pattern = keywords1[1], ignore.case = T) &
#                    grepl(x = str0, pattern = keywords1[2], ignore.case = T) &
#                    grepl(x = str0, pattern = keywords1[3], ignore.case = T)) {
#           str1[sum(grep(x = str1, pattern = keywords1[1], ignore.case = T)>0)]<-keywords1[1]
#           str1[sum(grep(x = str1, pattern = keywords1[2], ignore.case = T)>0)]<-keywords1[2]
#           str1[sum(grep(x = str1, pattern = keywords1[3], ignore.case = T)>0)]<-keywords1[3]
#         }
#       }
# 
#       # if (str1[1] == "von" & str1[2] == "siebolds") {
#       #   str1<-str1[2:length(str1)]
#       #   str1<-c("VonSiebold's", str1[3])
#       # }
# 
#       # if (sum(grepl(pattern = "*A'u*", x = str1, ignore.case = T))>=1) {
#       #   str1[grepl(pattern = "*A'u*", x = str1, ignore.case = T)]<-"*A\U02BBu*"
#       # }
#       #
#       # if (sum(grepl(pattern = "*O'io*", x = str1, ignore.case = T))>=1) {
#       #   str1[grepl(pattern = "*O'io*", x = str1, ignore.case = T)]<-"*O\U02BBio*"
#       # }
#       #
#       # if (sum(grepl(pattern = "*'Ahi*", x = str1, ignore.case = T))>=1) {
#       #   str1[grepl(pattern = "*'Ahi*", x = str1, ignore.case = T)]<-"*\U02BBAhi*"
#       # }
# 
# 
#       str1<-paste(str1, collapse = " ")
#       str1<-gsub(pattern = "\\( ", replacement = "\\(", x = str1)
#       str1<-gsub(pattern = " \\)", replacement = "\\)", x = str1)
#       if (capitalizefirst==T) {
#         str1<-paste(toupper(substr(str1, 1, 1)), substr(str1, 2, nchar(str1)), sep="")
# 
#       }
# 
#       str1<-gsub(pattern = "&", replacement = "and", x = str1)
# 
#       str2<-c(str2, str1)
#     }
#     str2<-trimws(str2)
#   }
#   return(str2)
# }


add_report_spp <- function(spp_info, 
                           spp_info_codes = "species_code", 
                           report_spp, 
                           report_spp_col, 
                           report_spp_codes = "species_code", 
                           lang = TRUE, 
                           expand = TRUE){
  
  temp <- report_spp %>% 
    dplyr::select(-questions) 
  
  if (!lang) {
    temp <- temp %>%
      dplyr::select(-dplyr::starts_with("lang_"))
  }
  
  if (report_spp_col == "order") {
    
    report_spp_col <- "order1"
    temp$order1<-NA
    temp$order1[!is.na(temp$order)] <- TRUE
    
  }
  
  temp <- temp %>% 
    dplyr::rename(col = all_of(report_spp_col)) %>% 
    dplyr::filter(col == TRUE & 
                    !is.na(species_code)) %>% 
    dplyr::arrange((order)) 

  # expand google spreadsheet
  temp1<-data.frame()
  temp$species_code1 <- temp$species_code
  temp$species_code <- NULL
  for (i in 1:nrow(temp)){
    # if (grepl(pattern = "c(", x = report_spp$species_code[i], fixed = TRUE)) {
    temp2 <- eval(expr = parse(text = temp$species_code1[i]))
    # for (ii in 1:length(temp2)) {
    # temp1<-temp[i,]
    # temp1$species_code <- temp2[ii]
    temp1<-rbind.data.frame(temp1, 
                            cbind.data.frame(temp[i,], 
                                             species_code = temp2))
    # }
  }
  
  temp1$species_code1 <- NULL
  temp1 <- unique(temp1)
  
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
  
  if (sum(temp1$species_code[(duplicated(temp1$species_code))])>0) warning("There are still duplicates in the species split ups!")
  
  
  
  temp0 <-  
    dplyr::left_join(x = temp1 %>% 
                       dplyr::select(-col), 
                     y = spp_info %>% 
                       dplyr::select(species_code, 
                                     genus_taxon, species_taxon) %>% 
                       unique(), 
                     by = "species_code")  %>% 
    dplyr::mutate(taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish", 
      species_code >= 40001 ~ "invert")) %>% 
    dplyr::mutate(temp = trimws(gsub(pattern = "NA", 
                                     replacement = "", 
                                     paste0(genus_taxon, " ", species_taxon)))) %>% 
    dplyr::mutate(temp = ifelse(temp == " ", "", temp)) %>%
    dplyr::mutate(species_name = dplyr::case_when(
      group_sci == "BLANK" ~ "",
      !is.na(group_sci) ~ group_sci, 
      is.na(group_sci) ~ temp, 
      TRUE ~ "other"
    )) %>%
    dplyr::mutate(temp = trimws(paste0(group_sci, " (", print_name, ")"))) %>%
    dplyr::mutate(temp = ifelse(temp == "NA ", "", temp)) %>%
    dplyr::mutate(group = dplyr::case_when(
      is.na(group_sci) ~ temp, 
      TRUE ~ "other"
    )) %>%
    # dplyr::select(file_name, print_name, species_name, 
    #   # report_name_scientific, 
    #   taxon, group, species_code, 
    #   scientific_name_prev, 
    #   dplyr::starts_with("lang_"), dplyr::starts_with("plot_")) %>%
    # dplyr::filter(!grepl(pattern = "other ", x = group) & 
    #                 !grepl(pattern = "egg ", x = group)) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(type = ifelse(
      grepl(pattern = " ", x = species_name, fixed = TRUE),
      # species_name == paste0(genus_taxon, " ", species_taxon),
      "ital", NA)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(species_name0 = species_name, 
                  species_name1 = species_name, 
                  species_name0 = dplyr::if_else(is.na(type == "ital"), species_name0, paste0("*", species_name0, "*")), 
                  species_name0 = gsub(pattern = " spp.*", replacement = "* spp.", x = species_name0, fixed = TRUE), 
                  species_name0 = gsub(pattern = " sp.*", replacement = "* sp.", x = species_name0, fixed = TRUE), 
                  species_name = species_name0) %>% 
    dplyr::select(-type, -temp, -species_name0, -genus_taxon, -species_taxon)
  
  return(temp0)
}





# Plotting ----------------------------


# https://coolbutuseless.github.io/package/ggpattern/
# https://github.com/trevorld/gridpattern


set_breaks <- function(dat, var) {
  set.breaks0 <- classInt::classIntervals(var = as.numeric(unlist(dat[,var]))[as.numeric(unlist(dat[,var])) != 0], 
                                          n = 5, style = "jenks")$brks
  set.breaks <- c()
  
  for (i in 1:length(set.breaks0)) {
    
    if (i == length(set.breaks0)) {
      set.breaks<-c(set.breaks, ceiling(x = set.breaks0[i])) #Inf)# #round(set.breaks0[i], digits = 0))
    } else if (i == 1) {
      set.breaks<-c(set.breaks, 0)
    } else {    
      set.breaks <- c(set.breaks, 
                      plyr::round_any(x = set.breaks0[i], 
                                      accuracy = ifelse(max(set.breaks0[i])>300, 100, 
                                                        ifelse(max(set.breaks0[i])>100, 50, 
                                                               ifelse(max(set.breaks0[i])>20, 10, 
                                                                      1))), 
                                      f = ceiling))    
    }
  }
  set.breaks <- unique(set.breaks)
  
  return(set.breaks)
}



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
#' @param workfaster TRUE/FALSE. Cuts down resolution so figure will render faster if TRUE. 
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
  lat = "latitude",
  lon = "longitude",
  var = "cpue_kgha",
  key.title = "", 
  grid = "extrapolation.grid",
  extrap.box, 
  set.breaks = "auto", #seq(from = -2, to = 20, by = 2),
  workfaster = FALSE, 
  nrow = 2, 
  SRVY, 
  dist_unit = "nm", # nautical miles
  col_viridis = "mako", 
  plot_coldpool = FALSE) {
  
  yrs <- as.numeric(sort(x = yrs, decreasing = T))
  figure <- ggplot()
  
  if (nrow(dat) != 0) {
  if (set.breaks[1] =="auto") {
    set.breaks <- set_breaks(dat, var)
      
    # set.breaks0 <- classInt::classIntervals(var = as.numeric(unlist(dat[,var]))[as.numeric(unlist(dat[,var])) != 0], 
    #                                         n = 5, style = "jenks")$brks
    # set.breaks <- c()
    # 
    # for (i in 1:length(set.breaks0)) {
    #   
    #   if (i == length(set.breaks0)) {
    #     set.breaks<-c(set.breaks, ceiling(x = set.breaks0[i])) #Inf)# #round(set.breaks0[i], digits = 0))
    #   } else if (i == 1) {
    #     set.breaks<-c(set.breaks, 0)
    #   } else {    
    #     set.breaks <- c(set.breaks, 
    #                     plyr::round_any(x = set.breaks0[i], 
    #                                     accuracy = ifelse(max(set.breaks0[i])>300, 100, 
    #                                                       ifelse(max(set.breaks0[i])>100, 50, 
    #                                                              ifelse(max(set.breaks0[i])>20, 10, 
    #                                                                     1))), 
    #                                     f = ceiling))    
    #   }
    # }
    # set.breaks <- unique(set.breaks)
  }
  # if(set.breaks =="auto"){
  #   set.breaks <- quantile(as.numeric(unlist(dat[dat$year %in% yrs,var])), probs = c(0, .95))
  #   set.breaks <- plyr::round_any(x = set.breaks, 
  #                                 accuracy = ifelse(max(set.breaks)>300, 100, ifelse(max(set.breaks)>100, 50, 10)),
  #                                 f = ceiling)
  #   set.breaks <- seq(from = min(set.breaks), to = max(set.breaks), length.out = 5)
  # }
  
  # Select data and make plot
  for (ii in length(yrs):1) {
    
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
      dplyr::filter(year == yrs[ii] # & cpue_kgha > 0
      ) 
    
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
    
    if (ii == length(yrs)) {
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
  
  
  figure <- figure +
    geom_stars(data = stars_list, na.rm = TRUE) 
  } 
  
  if (plot_coldpool) {
    
    temp_break <- 2 # 2*C
    
    # nbs_ebs_layers <- akgfmaps::get_base_layers(select.region = "ebs",
    #                                             set.crs = coldpool:::ebs_proj_crs)
    
    year_vec <- yrs
    
    coords <- raster::coordinates(coldpool:::nbs_ebs_bottom_temperature)
    
    for(i in 1:length(year_vec)) {
      sel_layer_df <- data.frame(x = coords[,1],
                                 y = coords[,2],
                                 temperature = coldpool:::nbs_ebs_bottom_temperature@data@values[,i])
      sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
      sel_layer_df$year <- year_vec[i]
      
      if(i == 1) {
        bt_year_df <- sel_layer_df
      } else{
        bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
      }
    }
    
    figure <- figure +
      ggplot2::geom_tile(data = bt_year_df %>%
                           dplyr::filter(temperature <= temp_break) %>% 
                           dplyr::rename(new_dim = year),
                         aes(x = x,
                             y = y, 
                             group = new_dim),
                         fill = "magenta",#"gray80",
                         alpha = 0.5)
    
  }  
  
  if (nrow(dat) != 0) {
  figure <- figure +
    facet_wrap( ~ new_dim, nrow = nrow) +
    coord_equal() 
  } else {
    grid <- ""
    figure <- figure +
      ggplot2::geom_text(mapping = aes(x = mean(reg_dat$lon.breaks), 
                                       y = mean(reg_dat$lat.breaks), 
                                       label = "No data was available\nfor this species in this\nregion for this year."), 
                         fontface="bold")
  }
  
  figure <- figure + 
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
                   st.dist = .03,
                   height = 0.02,
                   st.bottom = TRUE,
                   st.size = 2,
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
        na.value = "transparent", 
        breaks = levels(temp0$var1.pred), 
        labels = levels(temp0$var1.pred))      
  }
  
  figure <- figure +
    guides(
      fill = guide_legend(title.position = "top", 
                          label.position = "bottom",
                          title.hjust = 0.5,
                          nrow = 1
      )) +
    
    #set legend position and vertical arrangement
    theme( 
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      axis.text = element_text(size = 8),
      
      strip.background = element_blank(), 
      strip.text = element_text(size = 12, face = "bold"), 
      # legend.title = element_text(size = 12), #, vjust = .5, hjust = .3),
      legend.text = element_text(size = 10),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      # legend.title.align = 0,#.1, 
      legend.position = "bottom",
      # legend.box.just = "center",
      # legend.key.width = unit(.5, "in"), 
      legend.box = "horizontal")
  
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
                             viridis_palette_option = "H", 
                             row = 2) {
  
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
                nrow = row) +
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


#' plot_size_comp
#'
#' @param sizecomp0 data.frame with these columns: "year", "taxon", "SRVY", "species_code", "sex", "pop", "length"   
#' @param length_data0 data.frame of sampled lengths
#' @param spp_code numeric. 
#' @param spp_print string. 
#' @param print_n TRUE/FALSE. Default = FALSE. Will print n = number of the species counted. 
#' @param ridgeline plot as ridgeline? Default = FALSE. 
#'
#' @return
#' @export
#'
#' @examples
plot_sizecomp <- function(sizecomp0,
                          length_data0 = NULL,
                          spp_code, 
                          spp_print, 
                          type = "length", 
                          print_n = FALSE, 
                          ridgeline = FALSE){
  
  table_raw <- sizecomp0 %>%
    dplyr::mutate(sex = stringr::str_to_title(
      gsub(pattern = "_", replacement = " ", x = sex, fixed = TRUE))) 
  
  table_raw$year <- factor(
    x = table_raw$year,
    levels = as.character(sort(unique(table_raw$year))),
    labels = as.character(sort(unique(table_raw$year))),
    ordered = TRUE)
  
  table_raw$sex <- factor(
    x = table_raw$sex,
    levels = as.character(sort(unique(table_raw$sex), decreasing = TRUE)),
    labels = as.character(sort(unique(table_raw$sex), decreasing = TRUE)),
    ordered = TRUE)
  
  # find appropriate units
  a<-find_units(unit = "", unt = "", dat = max(table_raw$pop))
  for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }
  pop_unit <- divby
  pop_unit_word <- unit_word
  
  # mm vs cm
  len_unit_word <- ifelse(!grepl(pattern = " crab", x = spp_print, ignore.case = TRUE), 
                          #report_spp$taxon[jj] =="fish", 
                          # max(table_raw$length)-min(table_raw$length)>45, 
                          "cm", "mm")
  table_raw <- table_raw %>%
    dplyr::mutate(pop = pop/pop_unit, 
                  length = round(
                    x = length*ifelse(len_unit_word == "mm", 10, 1), digits = 0)) #%>%
  len_unit_axis <- ifelse(max(table_raw$length)-min(table_raw$length)>150, 50, 
                          ifelse(max(table_raw$length)-min(table_raw$length)>45, 10, 5))
  
  # figure 
  if (!ridgeline) { # facet plot without ridgeline
    
    # if (length(unique(table_raw$SRVY))>1) {
      
      figure <- ggplot(data = table_raw,
                       mapping = aes(x = length,
                                     y = pop,
                                     group = SRVY,
                                     fill = sex)) +
        geom_bar(position="stack", stat="identity", na.rm = TRUE) +
        scale_fill_viridis_d(direction = -1, 
                             option = "mako",
                             begin = .2,
                             end = .6,
                             na.value = "transparent") +
        guides(fill=guide_legend(title="")) +
        scale_y_continuous(name = paste0(spp_print, #"\n
                                         " population",pop_unit_word), 
                           breaks = function(pop) unique(floor(pretty(seq(0, (max(pop) + 1) * 1.1))))) +
        scale_x_continuous(name = stringr::str_to_sentence(paste0(type," (", len_unit_word, ")")), 
                           breaks = function(length) unique(floor(pretty(seq(0, (max(length) + 1) * 1.1))))) +
        facet_grid(year ~ SRVY,
                   scales = "free_x") 
      
    # } else {
    # 
    # figure <- ggplot(data = table_raw,
    #                  mapping = aes(x = length,
    #                                y = pop,
    #                                fill = sex))+
    #   geom_bar(position="stack", stat="identity", na.rm = TRUE) +
    #   scale_fill_viridis_d(direction = -1, 
    #                        option = "mako",
    #                        begin = .2,
    #                        end = .6,
    #                        na.value = "transparent") +
    #   guides(fill=guide_legend(title="")) +
    #   scale_y_continuous(name = paste0(spp_print, "\npopulation",pop_unit_word), 
    #                      breaks = function(pop) unique(floor(pretty(seq(0, (max(pop) + 1) * 1.1))))) +
    #   scale_x_continuous(name = stringr::str_to_sentence(paste0(type," (", len_unit_word, ")")), 
    #                      breaks = function(length) unique(floor(pretty(seq(0, (max(length) + 1) * 1.1))))) +
    #   facet_grid(year ~ .,
    #              scales = "free_x")  
    # }
    figure <- figure +
      guides(
        fill = guide_legend(title.position = "top", 
                            label.position = "bottom",
                            title.hjust = 0.5,
                            nrow = 1
        )) +
      theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill = NA,
                                    colour = "grey20"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 10),
        legend.background = element_rect(colour = "transparent", 
                                         fill = "transparent"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.position="bottom",
        legend.box = "horizontal"
      )
    
    
  } else {
    
    table_raw1 <- table_raw %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(year, #SRVY, 
                      length) %>% 
      dplyr::summarise(pop = sum(pop, na.rm = TRUE))
    
    temp <- setdiff(as.numeric(paste(min(table_raw1$year, na.rm = TRUE))):as.numeric(paste(max(table_raw1$year, na.rm = TRUE))), 
                    as.numeric(paste(unique(table_raw1$year))))
    if (length(temp)>0) {
      table_raw1 <- rbind.data.frame(
        data.frame(year = temp,
                   # SRVY = , 
                   length = 0, 
                   pop = 0), 
        table_raw1)
    }
    
    table_raw1 <- table_raw1 %>% 
      dplyr::arrange(desc(year)) #%>% 
    # dplyr::mutate(
    #   year = as.numeric(paste(table_raw$year)), 
    #   year =  factor(
    # x = year,
    # levels = as.character(sort(unique(year), decreasing = TRUE)),
    # labels = as.character(sort(unique(year), decreasing = TRUE)),
    # ordered = TRUE))
    
    table_raw1$year <- as.numeric(paste(table_raw1$year))
    table_raw1$year <- factor(
      x = table_raw1$year,
      levels = as.character(sort(unique(table_raw1$year), decreasing = TRUE)),
      labels = as.character(sort(unique(table_raw1$year), decreasing = TRUE)),
      ordered = TRUE)
    
    
    figure <- ggplot(data = table_raw1, 
                     mapping = aes(x = length, 
                                   y = year, 
                                   fill = length, 
                                   height = pop/mean(pop, na.rm = TRUE))) +
      ggridges::geom_ridgeline_gradient() +
      scale_fill_viridis_c(name = length, option = "G") +
      ylab(paste0(spp_print, #"\n
                  " population across years")) +
      xlab(stringr::str_to_sentence(paste0(type," (", len_unit_word, ")"))) +
      theme(legend.position = "none", 
            panel.grid.major.x = element_line(colour = "grey80"))
  }
  
figure <- figure + 
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        axis.title = element_text(size = 10, face = "bold"),
        axis.text = element_text(size = 10))
  
  
  if (print_n & !is.null(length_data0)) {
    
    dat_text  <- data.frame(
      label = paste0(c("# measured: ", rep_len(x = "", length.out = (length(unique(length_data0$year))-1))),   
                     length_data0 %>% 
                       dplyr::mutate(year = as.character(year)) %>% 
                       dplyr::ungroup() %>% 
                       dplyr::group_by((year)) %>% 
                       dplyr::summarise(frequency = formatC(x = sum(frequency, na.rm = TRUE), 
                                                            digits = 0, big.mark = ",", format = "f")) %>% 
                       dplyr::select(frequency) %>% 
                       unlist()))
    
    dat_text$label <- gsub("\\s", " ", formatC(x = dat_text$label)) #, width=max(nchar(dat_text$label))))
    
    
    figure <- 
      tag_facet(p = figure, 
                x = Inf, y = Inf, 
                hjust = 1.25,
                tag_pool = dat_text$label, 
                open = "", close = "")
  }
  
  return(figure)
}


plot_timeseries <- function(
  dat, 
  unit = "", 
  unt = "", 
  y_long = "", 
  spp_print = ""){
  
  table_raw <- dat
  pcol <- viridis::mako(n = 2, begin = .2, end = .6, direction = -1)
  
  # find appropriate units
  a<-find_units(unit, unt, dat = table_raw$y)
  for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }
  
  # unit <-1
  
  table_raw <- table_raw %>%
    dplyr::mutate(y = y/divby, 
                  upper = upper/divby, 
                  lower = lower/divby)#, 
  # uppervar = (y-var)/divby, 
  # lowervar = (y+var)/divby)
  
  table_raw_mean <- table_raw %>% 
    dplyr::group_by(SRVY_long, SRVY) %>% 
    dplyr::summarise(y = mean(y, na.rm = TRUE), 
                     minyr = min(year, na.rm = TRUE), 
                     maxyr = max(year, na.rm = TRUE)) %>% 
    dplyr::mutate(SRVY_long1 = paste0(SRVY_long, #"\n
                                      " (mean = ", 
                                      formatC(x = y, digits = 1, big.mark = ",", format = "f"), 
                                      unit_wrd, ")"), 
                  yy = y*divby) %>% 
    dplyr::filter(yy>100) # if there is too little data, don't bother plotting
  
  anno<-NA
  temp<-setdiff(x = unique(table_raw$SRVY), 
                y = unique(table_raw_mean$SRVY))
  if (length(temp) != 0) {
    # Reduce(intersect, 
    #        list(unique(table_raw$SRVY), 
    #             (unique(table_raw_mean$SRVY))))
    # setdiff(x = unique(table_raw$SRVY), 
    #         y = unique(table_raw_mean$SRVY))
    anno <- paste0("Data for this species in the\n", 
                   temp, 
                   " is too limited to plot.")
    
    which(unique(table_raw$SRVY)==temp)
    
    
    pcol_anno <- pcol[which(unique(table_raw$SRVY)==temp)]
    pcol <- pcol[which(unique(table_raw$SRVY)!=temp)]
  }
  
  table_raw <- 
    dplyr::left_join(
      x = table_raw, 
      y = table_raw_mean %>% 
        dplyr::select(SRVY_long, SRVY_long1), 
      by = "SRVY_long") %>% 
    dplyr::filter(SRVY %in% table_raw_mean$SRVY)
  
  
  
  figure <- ggplot(mapping = aes(x = year, y = y, 
                                 color = SRVY_long1, group = SRVY_long1), 
                   data = table_raw) +
    geom_line(size = 1) +
    geom_point(size = 1.5) +
    geom_segment(data = table_raw_mean, 
                 # yintercept=y,
                 mapping = aes(x = minyr, xend = maxyr, y = y, yend = y, 
                               group = SRVY_long1, color = SRVY_long1), 
                 linetype = "dashed", size = 1) +
    
    # geom_errorbar(aes(ymin=lowervar, ymax=uppervar), 
    #               width=.5, size = .5, # http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
    #                position=position_dodge(0.05)) +
    
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=.5, size = .5, # http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization
                  position=position_dodge(0.05)) +
    # scale_color_viridis_d(direction = -1,
    #                       option = "mako",
    #                       begin = .2,
    #                       end = .6,
    #                       na.value = "transparent") %>%
    scale_color_manual(values = pcol, breaks = unique(table_raw$SRVY_long1))
  
  if (!is.na(anno)) {
    figure <- figure +
      ggplot2::geom_text(x = Inf, y = -Inf, 
                         hjust = 1.2, vjust = -.3, 
                         label = anno, show.legend = FALSE, 
                         size = 4, fontface = "italic",
                         color = pcol_anno)
  }
  
  
  figure <- figure +
    guides(color=guide_legend(title="")) +
    xlab("Year") +
    scale_y_continuous(name = paste0(stringr::str_to_sentence(spp_print), "\n", y_long, unit_word), 
                       breaks = function(y) unique(floor(pretty(seq(0, (max(y) + 1) * 1.1))))) + 
    guides(color = guide_legend(nrow = 2)) +
    theme(
      # axis.line=element_line(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "grey95"),
      panel.border = element_rect(fill = NA,
                                  colour = "grey20"),
      # strip.background = element_blank(),
      # strip.text = element_text(size = 12, face = "bold"),
      legend.title = element_blank(), #element_text(size = 15),
      legend.text = element_text(size = 14),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent",
                                fill = "transparent"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 12),
      legend.position = "bottom", # c(.7, .8), #
      # legend.box.just = "left",
      # legend.key.width = unit(.5, "in"),
      legend.box = "horizontal"
    )
  # facet_wrap( ~ SRVY, 
  #             ncol = 1) 
  
  return(figure)
}


# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2
tag_facet <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}

plot_survey_stations <- function(reg_dat, 
                                 station_info, 
                                 haul_cruises_maxyr, 
                                 station_grid = FALSE, 
                                 stratum_no = FALSE, 
                                 station_pts_srvy = TRUE, 
                                 station_pts_vess = FALSE, 
                                 study = FALSE, 
                                 dist_unit = "nm") { 
  
  # survey_reg_col <- c(as.character(nmfspalette::nmfs_cols("supdkgray")), 
  #                     as.character(nmfspalette::nmfs_cols("medgray")))
  
  
  survey_reg_col <- gray.colors(length(unique(reg_dat$survey.area$SURVEY))+2)
  survey_reg_col <- survey_reg_col[-((length(survey_reg_col)-1):length(survey_reg_col))]
  
  figure <- ggplot() 
  
  # if (station_pts_vess) {
  
  if (study) {
    
    stdy <- reg_dat$survey.grid %>% dplyr::filter(!is.na(study))
    
    figure <- figure  +
      geom_sf(data = reg_dat$survey.grid %>% dplyr::filter(!is.na(study)),
              mapping = aes(fill = study),
              show.legend = TRUE,
              color = "black",
              size = .5, 
              alpha = .5,
              na.rm = TRUE) +
      scale_fill_manual(
        name = "", #"Survey Vessels",
        values = unique(reg_dat$survey.grid$study_col),
        breaks = unique(reg_dat$survey.grid$study),
        labels = unique(reg_dat$survey.grid$study_long),
        na.value = "transparent")
  }
  # }
  
  if (stratum_no) {
    figure <- figure  +
      geom_sf(data = reg_dat$survey.strata, fill = NA, color = "grey50")
  } else {
    figure <- figure  +
      geom_sf(data = reg_dat$bathymetry, color = "grey50")    
  }
  
  figure <- figure  +
    geom_sf(data = reg_dat$graticule,
            color = "grey90",
            alpha = 0.5)
  
  
  if (station_pts_vess) {
    
    vess <- reg_dat$survey.grid %>% dplyr::filter(!is.na(vessel_name))
    
    figure <- figure  +
      stat_sf_coordinates(data = vess,
                          mapping = aes(color = vess_col, 
                                        shape = vess_shape),
                          size = 1.5, 
                          show.legend = TRUE, 
                          na.rm = TRUE) + 
      geom_sf(data = reg_dat$survey.area %>%
                dplyr::mutate(SURVEY = dplyr::case_when(
                  SURVEY == "EBS_SHELF" ~ "EBS", 
                  SURVEY == "NBS_SHELF" ~ "NBS")), 
              aes(color = reg_dat$survey.area$SURVEY, 
                  shape = reg_dat$survey.area$SURVEY), 
              fill = NA, 
              size = 1.5,
              show.legend = TRUE)  +
      
      scale_color_manual(
        name = " ", #"Survey Region",
        values = c(alpha(colour = c(survey_reg_col), 0.7), 
                   unique(vess$vess_col)),
        breaks = c(rev(unique(reg_dat$survey.area$SURVEY)), 
                   unique(vess$vess_col)), 
        labels = c(rev(unique(stringr::str_to_title(haul_cruises_maxyr$SRVY_long))), 
                   unique(vess$vessel_name)), 
        na.value = "transparent")  +
      
      scale_shape_manual(
        name = " ", #"Survey Vessels",
        values = c("", "",
                   unique(vess$vess_shape)),
        breaks = c(unique(reg_dat$survey.area$SURVEY),
                   unique(vess$vess_shape)),
        labels = c(rev(unique(stringr::str_to_title(haul_cruises_maxyr$SRVY_long))), 
                   unique(vess$vessel_name))) +
      ggplot2::guides(
        colour = guide_legend(
          # order = 1,# survey regions
          override.aes = list(fill = NA,
                              # color = c(survey_reg_col, NA, NA),
                              linetype = c(
                                rep_len(x = 1, 
                                        length.out = length(unique(haul_cruises_maxyr$SRVY))), 
                                0, 0), # c(1,1,0,0),
                              # shape = c(NA, NA, "A", "V"),
                              size = 3)),
        fill = guide_legend(
          # order = 2,# survey regions
          override.aes = list(
            color = "black", 
            linetype = c(1), 
            shape = NA, 
            size = .5)))
  } else { # station_pts_vess == FALSE
    figure <- figure +
      geom_sf(data = reg_dat$survey.area %>%
                dplyr::mutate(SURVEY = dplyr::case_when(
                  SURVEY == "EBS_SHELF" ~ "EBS", 
                  SURVEY == "NBS_SHELF" ~ "NBS")), 
              aes(color = SURVEY), 
              fill = NA, 
              size = 1.5,
              show.legend = TRUE) +
      scale_color_manual(
        name = "", #"Survey Region",
        values = c(survey_reg_col,
                   survey_reg_col),
        breaks = c(rev(unique(station_info$SRVY)),
                   rev(unique(station_info$SRVY))), #TOLEDO rev
        labels = c(rev(unique(stringr::str_to_title(haul_cruises_maxyr$SRVY_long))),
                   rev(unique(stringr::str_to_title(haul_cruises_maxyr$SRVY_long))))) #+
    # scale_color_manual(
    #   name = " ", #"Survey Region",
    #   values = c(alpha(colour = c(survey_reg_col), 0.7),
    #              unique(vess$vess_col)),
    #   breaks = c(rev(unique(reg_dat$survey.area$SURVEY)),
    #              unique(vess$vess_col)),
    #   labels = c(rev(unique(stringr::str_to_title(haul_cruises_maxyr$SRVY_long))),
    #              unique(vess$vessel_name)),
    #   na.value = "transparent")
  }
  
  figure <- figure +
    geom_sf(data = reg_dat$akland, color = NA, fill = "grey80")
  
  if (station_pts_srvy) {
    figure <- figure +
      stat_sf_coordinates(data = dplyr::left_join( x = reg_dat$survey.grid, 
                                                   y = station_info, 
                                                   by = c("STATIONID" = "stationid"))  %>% 
                            dplyr::filter(in_maxyr == TRUE),
                          mapping = aes(color = SRVY),
                          shape = 16,
                          size = 1.5, 
                          show.legend = FALSE, 
                          na.rm = TRUE)  
  }
  
  if (stratum_no) {
    figure <- figure +
      geom_sf_text(data = reg_dat$survey.strata, 
                   lineheight = 0.7,
                   mapping = aes(label = reg_dat$survey.strata$Stratum),
                   color = "black",
                   size = 5,
                   show.legend = FALSE)
  }
  
  if (station_grid) {
    figure <- figure +
      geom_sf(data = reg_dat$survey.grid, color = "grey20", fill = NA) +
      geom_sf_text(data = reg_dat$survey.grid, 
                   lineheight = 0.7,
                   mapping = aes(label = gsub(x = STATIONID, 
                                              replacement = "\n", 
                                              pattern = "-")),
                   color = "black",
                   size = 1.3,
                   show.legend = FALSE) #+
    
  }
  
  
  figure <- figure +
    coord_sf(xlim = reg_dat$plot.boundary$x, 
             ylim = reg_dat$plot.boundary$y) +
    scale_x_continuous(name = "Longitude", 
                       breaks = reg_dat$lon.breaks) + 
    scale_y_continuous(name = "Latitude", 
                       breaks = reg_dat$lat.breaks) +
    geom_text(data = subset(reg_dat$place.labels, type == "mainland"), 
              aes(x = x, y = y, label = lab), 
              size = 14, group = 99) + 
    geom_shadowtext(data = subset(reg_dat$place.labels, type == "peninsula"), 
                    aes(x = x, y = y, label = lab), size = 5, angle = 40, 
                    bg.color = "white", color = "black", group = 99) + 
    geom_shadowtext(
      data = subset(reg_dat$place.labels, type %in% c("bathymetry", "islands")),
      aes(x = x, y = y, label = lab), 
      bg.color = "white", color = "black", 
      size = 2, group = 99) +
    ggsn::scalebar(data = reg_dat$survey.grid,
                   location = "bottomleft",
                   dist = 150,
                   dist_unit = dist_unit,
                   transform = FALSE,
                   st.dist = .03,
                   height = 0.02,
                   st.bottom = TRUE,
                   st.size = 2,
                   model = reg_dat$crs) + 
    # ggsn::scalebar(data = reg_dat$survey.grid, 
    #                location = "bottomright",
    #                dist = 100, 
    #                dist_unit = "km", 
    #                transform = FALSE, 
    #                st.dist = .02, 
    #                height = 0.02, 
    #                st.bottom = TRUE, 
    #                st.size = 3, 
    #                # transform = TRUE, 
    #                model = reg_dat$crs) +
    #set legend position and vertical arrangement
    theme(#plot.background = element_rect(fill = "white"), 
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      strip.background = element_rect(fill = "grey85", 
                                      colour = "grey20"),
      legend.spacing.y = unit(-0.35, "cm"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 7),
      # strip.background = element_rect(color = NA, fill = NA, size = 0),
      legend.background=element_blank(),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.position = c(.15, .22),
      # legend.justification = c("right"),
      legend.box.just = "left",
      # legend.margin = margin(6, 6, 6, 6), 
      legend.box = "vertical"
    )
  
  return(figure)
}




# Tables -----------------------------------------------------------------------

table_change <- function(dat, 
                         yrs, 
                         maxyr, 
                         compareyr, 
                         remove_all = TRUE, 
                         font = "Times New Roman") { 
  
  # temp <- tidyr::crossing(
  #   haul_cruises_vess %>%
  #     dplyr::filter(SRVY == "NBS"),
  #   dplyr::distinct(
  #     catch_haul_cruises %>%
  #       dplyr::filter(SRVY == "NBS")  %>%
  #       dplyr::left_join(
  #         x = .,
  #         y = spp_info %>%
  #           dplyr::select(species_code, group),
  #         by = "species_code"),
  #     species_code, group)) %>%
  #   dplyr::left_join(
  #     x = .,
  #     y = catch_haul_cruises %>%
  #       dplyr::select("cruisejoin", "hauljoin", "cruisejoin", "species_code",
  #                     "weight", "number_fish",
  #       ),
  #     by = c("species_code", "hauljoin", "cruisejoin")) %>%
  #   #### a check for species with weights greater then 0
  #   ## sum catch weight (by groups) by station and join to haul table (again) to add on relevent haul data
  #   dplyr::group_by(year, stationid, #species_code,
  #                   group, hauljoin, stratum, distance_fished, net_width) %>%
  #   dplyr::summarise(wt_kg_summed_by_station = sum(weight, na.rm = TRUE), # overwrite NAs in assign_group_zeros where data exists
  #                    num_summed_by_station = sum(number_fish, na.rm = TRUE)) %>% # overwrite NAs in
  # 
  #   ## checks catch_and_zeros table for species that are not in groups, if species are not grouped
  #   #### add group to assign_groups table
  #   ## calculates CPUE for each species group by station
  #   mutate(effort = distance_fished * net_width/10) %>%
  #   mutate(CPUE_weight_kgperhect = wt_kg_summed_by_station/effort) %>%
  #   mutate(CPUE_number_perhect = ifelse(wt_kg_summed_by_station > 0 & num_summed_by_station == 0, NA,
  #                                       (CPUE_number = num_summed_by_station/effort))) %>%
  #   #### this is to check CPUEs by group, station and year against the SQL code
  #   ## add area to CPUE table
  #   dplyr::left_join(x = .,
  #                    y = stratum_info %>%
  #                      dplyr::select(stratum, area),
  #                    by = 'stratum') #%>%
  # 
  # # calculates total area by adding up the unique area values (each strata has a different value)
  # total_area <- sum(unique(temp$area))
  # 
  # table_raw <- temp %>%
  #   ## calculates mean CPUE (weight) by year, group, stratum, and area
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(year, group, stratum, area) %>%
  #   dplyr::summarise(CPUE_by_group_stratum = mean(CPUE_weight_kgperhect, na.rm = TRUE)) %>% # TOLEDO - na.rm = T?
  #   ## creates column for meanCPUE per group/stratum/year*area of stratum
  #   dplyr::mutate(mean_cpue_times_area = (CPUE_by_group_stratum * area)) %>%
  #   ## calculates sum of mean CPUE*area (over the 3 strata)
  #   dplyr::ungroup() %>%
  #   dplyr::group_by(year, group) %>%
  #   dplyr::summarise(mean_CPUE_all_strata_times_area =
  #                      sum(mean_cpue_times_area, na.rm = TRUE)) %>% # TOLEDO - na.rm = T?
  #   ## creates column with weighted CPUEs
  #   dplyr::mutate(weighted_CPUE = (mean_CPUE_all_strata_times_area / total_area)) %>%
  #   ### uses WEIGHTED CPUEs to calculate biomass
  #   ## includes empty shells and debris
  #   dplyr::group_by(year, group) %>%
  #   dplyr::mutate(biomass_mt = weighted_CPUE*(total_area*.1)) %>%
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
  table_raw <- dat %>% 
    dplyr::filter(year %in% yrs) %>%
    dplyr::select(SRVY, year, print_name, species_name1, y, taxon) %>%
    ## creates a biomass column for each year
    tidyr::pivot_wider(
      id_cols = c("SRVY", "print_name", "species_name1", "taxon"),
      names_from = "year",
      values_from = c("y") )  %>%
    dplyr::ungroup()
  
  ##  calculate percent change, seperate group (common name) and taxon, filter out groups
  ## change symbols/percents to red text if negative 
  temp <- expand.grid(yrs, yrs)
  temp <- temp[temp$Var1 != temp$Var2,]
  table_raw <- as.data.frame(table_raw)
  for (i in 1:nrow(temp)) {
    table_raw$change <- NMFSReports::pchange(
      start = unlist(table_raw[,names(table_raw) == as.character(temp[i,1])]), 
      end = unlist(table_raw[,names(table_raw) == as.character(temp[i,2])]), 
      value_only = TRUE)
    names(table_raw)[names(table_raw) == "change"] <- paste0("change_", temp[i,1], "_", temp[i,2]) 
  }
  table_raw$change <- table_raw[,paste0("change_", compareyr , "_", maxyr) ]
  
  table_raw <- table_raw %>%
    dplyr::arrange(desc(SRVY), desc(change)) %>%
    dplyr::filter(change != Inf) %>% 
    dplyr::rename(Survey = SRVY)
  
  
  # remove spp with all 0s
  if (length(yrs) == 4) {
  a<-Reduce(intersect, 
            list(which(table_raw[,as.character(yrs)[1]]==0),
                 which(table_raw[,as.character(yrs)[2]]==0),
                 which(table_raw[,as.character(yrs)[3]]==0),
                 which(table_raw[,as.character(yrs)[4]]==0)))
  } else if (length(yrs) == 3) {
    a<-Reduce(intersect, 
              list(which(table_raw[,as.character(yrs)[1]]==0),
                   which(table_raw[,as.character(yrs)[2]]==0),
                   which(table_raw[,as.character(yrs)[3]]==0)))
  } else if (length(yrs) == 2) {
    a<-Reduce(intersect, 
              list(which(table_raw[,as.character(yrs)[1]]==0),
                   which(table_raw[,as.character(yrs)[2]]==0)))
  } else {
    a<-Reduce(intersect, 
              list(which(table_raw[,as.character(yrs)[1]]==0)))
  }
  
  
  if (length(a)!=0) {
    table_raw0 <- table_raw[-a,]
  } else {
    table_raw0<-table_raw
  }
  
  table_print <- table_raw0 %>%
    dplyr::select(-taxon, -starts_with("change_")) %>% 
    dplyr::mutate_if(is.numeric, round, 0) %>%
    dplyr::mutate(change = paste0(prettyNum(change, big.mark=","), "%")) %>% 
    dplyr::mutate(group = stringr::str_to_title(print_name)) 
  
  if (remove_all) {
    table_print$group <- gsub(pattern = "All ", replacement = "", 
                              x = table_print$group, fixed = TRUE)
  }
  
  table_print <- table_print %>% 
    dplyr::mutate(spp = dplyr::case_when(
      grepl(pattern = "sp.", x = species_name1, fixed = TRUE) ~ "sp.", 
      grepl(pattern = "spp.", x = species_name1, fixed = TRUE) ~ "spp.", 
      TRUE ~ "")) %>%
    dplyr::mutate(
      species_name = species_name1, 
      species_name1 = 
        gsub(pattern = " sp.", replacement = "", 
             x = species_name1, fixed = TRUE), 
      species_name1 =
        gsub(pattern = " spp.", replacement = "",
             x = species_name1, fixed = TRUE),
      species_name2 = dplyr::case_when(
        !grepl(pattern = " ", x = species_name, fixed = TRUE) ~ species_name1), 
      species_name1 = dplyr::case_when(
        grepl(pattern = " ", x = species_name, fixed = TRUE) ~ species_name1)) 
  
  table_print <- table_print %>%
    flextable::flextable(data = ., col_keys = c(ifelse(length(unique(table_print$SRVY)) == 1, "", "Survey"), 
                                                "group", "dummy", 
                                                as.character(yrs), "change")) %>%
    compose(j = "dummy", 
            value = as_paragraph(as_i(species_name1), species_name2, " ", spp)) %>% # https://stackoverflow.com/questions/57474647/italic-and-color-in-an-r-flextable
    flextable::color(color = "red", 
                     i = grepl(pattern = "-", x = as.character(table_raw0$change)), 
                     j = which(names(table_print) == "change")) %>% 
    flextable::set_header_labels(.,
                                 group = "Common name",
                                 dummy = "Taxon",
                                 change = paste0("Change (", maxyr, ", ", compareyr, ")" )) %>%
    NMFSReports::theme_flextable_nmfstm(row_lines = FALSE, x = ., font = font)
  
  return(list("table_print" = table_print, 
              "table_raw" = table_raw))
}


table_change_pres <- function(dat, 
                              yrs, 
                              maxyr, 
                              compareyr, 
                              remove_all = TRUE, 
                              font = "Arial", 
                              unit = "", 
                              unt = "", 
                              y_long = "", 
                              divby = NULL, 
                              digits = 1) {
  
  temp <- data.frame(var2 = yrs[-length(yrs)],
                     var1 = yrs[-1] )
  
  table_raw <- dat
  
  a<-find_units(unit = unit, unt = unt, dat = table_raw$y, divby = divby)
  for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }
  
  table_raw <- table_raw %>% 
    dplyr::select(SRVY, year, y, print_name, species_name1, taxon) %>%
    dplyr::mutate(y = y/divby)
  
  
  a<-table_change(dat = table_raw, 
                  yrs = yrs, 
                  maxyr = maxyr, 
                  compareyr = compareyr, 
                  remove_all = remove_all, 
                  font = font) 
  
  table_raw <- a$table_raw %>% 
    dplyr::select(Survey, print_name, 
                  dplyr::all_of(as.character(nbsyr)), 
                  dplyr::all_of(paste0("change_", as.character(temp$var2), "_", as.character(temp$var1)))) %>% 
    dplyr::filter(print_name %in% unique(report_spp1$print_name)) %>% 
    dplyr::mutate_if(is.numeric, formatC, digits = digits, format = "f", big.mark = ",") %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("change"), 
                                prettyNum, big.mark=",")) 
  
  # table_raw$SRVY[table_raw$SRVY == "EBS"] <- "SEBS"
  
  table_print<-table_raw
  
  for (i in 1:nrow(temp)) {
    table_print[,as.character(temp$var1)[i]] <- 
      paste0(unlist(table_print[,as.character(temp$var1)[i]]), 
             " (", 
             unlist(table_print[,paste0("change_", as.character(temp$var2[i]), "_", as.character(temp$var1[i]))]), 
             "%)")
  }
  
  table_print <- table_print %>% 
    dplyr::select(-starts_with("change_")) %>%
    dplyr::filter(tolower(print_name) %in% tolower(report_spp$print_name)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(desc(Survey)) %>%  
    dplyr::arrange((print_name))  
  
  
  # biomass_tab<-list()
  
  # for (i in 1:length(unique(report_spp1$file_name))) {
  
  # spp_print <- unique(report_spp1$print_name)[i]
  
  table_print <- temp0<- table_print %>%
    # dplyr::filter(tolower(print_name) == tolower(spp_print)) %>%
    dplyr::select(-print_name)
  
  table_print <- table_print %>%
    flextable::flextable(data = .)  %>%
    # red
    flextable::color(color = "red",
                     i = grepl(pattern = "(-", x = as.character(temp0[,as.character(nbsyr[2])]), fixed = TRUE),
                     j = as.character(nbsyr[2])) %>%
    flextable::color(color = "red",
                     i = grepl(pattern = "(-", x = as.character(temp0[,as.character(nbsyr[3])]), fixed = TRUE),
                     j = as.character(nbsyr[3])) %>%
    flextable::color(color = "red",
                     i = grepl(pattern = "(-", x = as.character(temp0[,as.character(nbsyr[4])]), fixed = TRUE),
                     j = as.character(nbsyr[4])) %>%
    
    # blue
    flextable::color(color = "blue",
                     i = !(grepl(pattern = "(-", x = as.character(temp0[,as.character(nbsyr[2])]), fixed = TRUE)),
                     j = as.character(nbsyr[2])) %>%
    flextable::color(color = "blue",
                     i = !(grepl(pattern = "(-", x = as.character(temp0[,as.character(nbsyr[3])]), fixed = TRUE)),
                     j = as.character(nbsyr[3])) %>%
    flextable::color(color = "blue",
                     i = !(grepl(pattern = "(-", x = as.character(temp0[,as.character(nbsyr[4])]), fixed = TRUE)),
                     j = as.character(nbsyr[4])) %>%
    
    flextable::bold(x = ., bold = TRUE, part = "all") %>%
    
    flextable::set_header_labels(.,
                                 Survey = paste0(y_long, 
                                                 ifelse(trimws(unit_wrd) == "", "", paste0(" ",trimws(unit_wrd))))) %>%
    NMFSReports::theme_flextable_nmfstm(
      x = ., row_lines = TRUE, body_size = 15, header_size = 25, 
      font = "Arial", pgwidth = 9)  
  
  return(list("table_print" = table_print, 
              "table_raw" = table_raw, 
              "unt" = paste0(#y_long,
                # ifelse(trimws(unit_wrd) == "", "", paste0(" ",
                trimws(unit_wrd)), #)),
              "unit" = paste0(#y_long, 
                # ifelse(trimws(unit_word) == "", "", paste0(" ",
                trimws(unit_word))))#))
  
}


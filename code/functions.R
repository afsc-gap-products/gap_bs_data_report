
# Directories ------------------------------------------------------------------

# Just in case you change the base name for any reason, it will change for anytime you load the files inside the folder, too! (e.g., if you have something against "scripts" being the name of the folder, just let the script know in one place aka right here).

# Directories to all of your current folders
dir_in<-paste0(here::here(), "/")
dirs<-list.dirs(path = dir_in, full.names = FALSE, recursive = FALSE)
dirs<-dirs[!grepl(pattern = "\\..", x = dirs)]
for (i in 1:length(dirs)) {
  assign(x = paste0("dir_", dirs[i]),
         value = paste0(dir_in, (dirs[i]), "/"))
}

# Where we save everything
dir_out_todaysrun <- paste0(dir_output, Sys.Date(),"/")
dir.create(dir_out_todaysrun)
dir_out_todaysrun <- paste0(dir_out_todaysrun, maxyr, "_", report_title, "/")
dir.create(dir_out_todaysrun)

dir_out_figtab <- paste0(dir_output, "figtab_",maxyr,"/")

dirs <- c("chapters", "rawdata")#, "documentation", "code", "figtab", "cite", "ref")
for (i in 1:length(dirs)) {
  if (dir.exists(paste0(dir_out_todaysrun, dirs[i])) == FALSE) {
    dir.create(paste0(dir_out_todaysrun, "/", dirs[i]))
  }
  assign(x = paste0("dir_out_", dirs[i]), value = paste0(dir_out_todaysrun, "/",dirs[i],"/"))
}

# options("citation_format" = "pandoc")

# Install Libraries ------------------------------------------------------------

# Here we list all the packages we will need for this whole process
# We'll also use this in our works cited page!!!
PKG <- c(
  # For creating R Markdown Docs
  "knitr", # A general-purpose tool for dynamic report generation in R
  "rmarkdown", # R Markdown Document Conversion
  "officer", 
  "officedown", # landscpe pges! https://github.com/rstudio/rmarkdown/issues/2034#issuecomment-774494900
  
  # Graphics
  "ggplot2", # Create Elegant Data Visualizations Using the Grammar of Graphics
  "cowplot",
  "png",
  "magick",
  "extrafont",
  "ggpubr",
  "ggridges",
  
  # other tidyverse
  "plyr",
  "dplyr",
  "googledrive",
  "magrittr",
  "readr",
  "tidyr",
  "readxl", 
  "here",
  "viridis",
  "janitor",
  
  # Text Management
  "stringr",
  "readtext",
  
  # RACE-GAP Specific
  "akgfmaps", # devtools::install_github("sean-rohan-noaa/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("sean-rohan-noaa/coldpool")
  
  # Spatial mapping
  "sf",
  "rlist",
  "shadowtext",
  "ggspatial", 
  "digest", 
  "ggsn",
  "ps", 
  "magrittr", 
  "raster",
  "stars",
  # "grid", 
  
  "scales", # nicer lables in ggplot2
  "pingr", # check website links
  "httr", # check website links
  "flextable", # making pretty tables
  "XML", # For editing XML files
  "RODBC"  # Oracle
)


PKG <- unique(PKG)
for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE)}
}

extrafont::loadfonts(device = "win")

# Aesthetics -------------------------------------------------------------------

full_page_portrait_width <- 6.5
full_page_portrait_height <- 7.5
full_page_landscape_width <- 9.5
# https://www.visualisingdata.com/2019/08/five-ways-to-design-for-red-green-colour-blindness/
negative <- "#EDA247"
positive <- "#57C4AD"
neutral <- "#E6E1BC"
# Functions --------------------------------------------------------------------

## Modify Numbers --------------------------------------------------------------

#' Calculate the percent change.
#'
#' Calculate the percent change.
#' @param start The value it started with.
#' @param end The value it ended with.
#' @param ending A text string. Default "".
#' @param percent_first Options: T/F. Puts the percent first in the sentance.
#' @param value_only Options: T/F. Will only provide the value, and no text. percent_first is over-ridden.
#' @keywords Modify number
#' @export
#' @examples
#' pchange(start = 8, end = 1)
#' pchange(start = 3, end = 6, ending = " in fish landings", percent_first = TRUE)
#' pchange(start = 3, end = 4, ending = " in fish landings", percent_first = FALSE)
#' pchange(start = 3, end = 4, ending = " in fish landings", value_only = TRUE)
pchange<-function(start, end,
                  ending="",
                  percent_first = TRUE,
                  value_only = FALSE){
  
  start0<-start
  end0<-end
  final1 <- c()
  
  if(length(start0) != length(end0) &
     !(length(start0) == 1 | length(end0) == 1 )) stop('start and end must be the same length, one can be any length and the other length of 1, or both must be the length of 1')
  
  if (length(start0)>1 & length(end0)==1) {
    end0 <- rep_len(x = end0, length.out = length(start0))
  } else if (length(end0)>1 & length(start0)==1) {
    start0 <- rep_len(x = start0, length.out = length(end0))
  }
  
  #calculate percent change:
  for (i in 1:length(start0)) {
    start <- start0[i]
    end <- end0[i]
    
    if (is.na(start) | is.na(end)) {
      final<- ifelse(value_only, NA, paste0(NA, "%"))
    } else if ((start == 0) & (end == 0)) {
      final <- ifelse(value_only, 0, "0%")
    } else if (value_only == TRUE) {
      start<-sum(as.numeric(start))
      end<-sum(as.numeric(end))
      final <- (100*(end-start)/start)
    } else if (value_only == FALSE) {
      start<-sum(as.numeric(start))
      end<-sum(as.numeric(end))
      p<-round(100*(end-start)/start)
      p<-ifelse(is.nan(p), 0, p)
      # decide direction, Omit if percent = 0:
      x<-p
      if (x<0) {
        txt<-paste0(" decrease",ending)
        p<-paste0("a ", abs(p),"%")
      } else if (x>0) {
        txt<-paste0(" increase",ending)
        p<-paste0("a ", abs(p),"%")
      } else if (round(x)==0){
        txt<-paste0("remains",ending," unchanged")
        p<-"" #ending must be "s" or "ed" here
      }
      
      # decide print order:
      if(percent_first) {
        final<-paste0(p,txt)
      } else {
        final<-paste0(txt," of ",p)
      }
      
      if (round(x)!=0) {
        if (sum(substr(x = numbers2words(abs(x)), start = 0, stop = 1) ==
                c("a", "e", "i", "o", "u"))==T & !(x %in% c(1, 100:199))) {
          final<-sub(pattern = "a ", replacement = "an ", x = final)
        }
      }
    }
    final1 <- c(final1, final)
  }
  return(final1)
}


#' Takes a string of words and combines them into a sentance that lists them.
#'
#' This function alows you to take a string of words and combine them into a sentance list. For example, 'apples', 'oranges', 'pears' would become 'apples, oranges, and pears'. This function uses oxford commas.
#' @param x Character strings you want in your string.
#' @param sep string. default = ", " but "; " or " " might be what you need!
#' @param sep_last2 string. default = " and " but " & " or " , " might be what you need!
#' @param sep_last string. default = " and " but " & " or " , " might be what you need!
#' @keywords strings
#' @export
#' @examples
#' text_list(c(1,2,"hello",4,"world",6))
#' text_list(c(1,"world"))
#' text_list(c(1,2,"hello",4,"world",6), oxford = FALSE)
#' paste0("here is a list of things: ",
#' text_list(paste0("list", 1:5), sep = " ", sep_last = " ")
text_list<-function(x = "",
                    # oxford = TRUE,
                    sep = ", ", 
                    sep_last2 = " and ", 
                    sep_last = ", and ") {
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) {
    str1<-paste(x, collapse = paste0(sep_last2))
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = paste0(sep))
    str1<-paste0(str1,
                 # sep, 
                 sep_last, x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}


#' Find a range of numbers for text
#'
#' This function outputs the range of values (broken or continuous) as you would want to display it in text.
#'
#' @param x A numeric vector of any length. Any duplicates will be removed.
#' @param dash A string that will go between consecutive values in the string output.
#' @param sep Default = ", ". Will only be used if the vector x provided is not continuous. Inherited from text_list().
#' @param sep_last Default = "and ". Will only be used if the vector x provided is not continuous. Inherited from text_list().
#' @param sep_last2 Default = "and ". Will only be used if the vector x provided is not continuous. Inherited from text_list().
#'
#' @return A string with the range of those values as might be included in a sentence ("1-3, 5, and 7-8").
#' @export
#'
#' @examples
#' # a typical example
#' x <- c(2003:2005, 2007, 2010:2012)
#' range_text(x)
#' # example has duplicate values out of order and specifies for a different dash
#' x <- c(1,2,11,3,4,7,NA,8,3)
#' range_text(x, dash = "--")
range_text <- function(x,
                       dash = "-",
                       sep = ", ",
                       sep_last2 = " and ",
                       sep_last = ", and ") {
  x <- x[!(is.na(x))]
  x <- x[!duplicated(x)]
  x <- sort(x)
  y <- min(x):max(x)
  z <- setdiff(y, x)
  if (length(z)>0) { # if x is not continuous
    # https://stat.ethz.ch/pipermail/r-help/2010-April/237031.html
    vec <- y
    vec[(vec %in% z)] <- NA
    
    # remove consecutive NAs
    foo <- function( x ){
      idx <- 1 + cumsum( is.na( x ) )
      not.na <- ! is.na( x )
      split( x[not.na], idx[not.na] )
    }
    ls <- foo(vec)
    
    str <- c()
    for (i in 1:length(ls)) {
      a <- ls[i][[1]]
      if (length(a) == 1){
        str <- c(str, paste0(a))
      } else {
        str <- c(str, paste0(min(a),dash,max(a)))
      }
    }
    str <- text_list(x = str,
                     sep = sep,
                     sep_last2 = sep_last2, 
                     sep_last = sep_last)
  } else {
    str <- paste0(min(x),dash,max(x))
  }
  return(str)
}


#' Determine the appropriate unit for a value.
#'
#' Determine the appropriate unit for a value (e.g., 1000000 = '1 Million'.
#' @param value A numeric value.
#' @param val_under_x_words a numeric that defines what values should be words as opposed to characters in a text. For example, many styles prefer that all values from 0 to 10 are spelled out in words, so you would set this parameter to 10 (which is the default). Set this parameter to NULL for nothing to to be spelled out.
#' @param words T/F. Default = TRUE. If TRUE, "1000000" would become "1 Million" and if FALSE would become "1,000,000".
#' @keywords Modify number, units
#' @export
#' @examples
#' xunits(value = NA)
#' xunits(value = c(0, 1))
#' xunits(value = c(0, 1), val_under_x_words = 0)
#' xunits(value = 12)
#' xunits(value = c(1.2345, 1.6, 1), words = FALSE)
#' xunits(value = c(12345, 123456, 1234567))
#' xunits(value = 123456789)
#' xunits(value = 123456789, words = FALSE)
xunits<-function(value,
                 val_under_x_words = 10,
                 words = TRUE
                 #, combine=TRUE # #' @param combine Should this be combined in a single string (T) or as two seperate strings in a list (F). Default = T.
                 
) {
  
  f = file()
  sink(file=f)
  
  combine <- TRUE
  
  out<-c()
  
  for (iii in 1:length(value)){
    
    value0<-sum(as.numeric(value[iii]))
    
    if (is.na(value0)) {
      out0 <- NA
    } else {
      
      if (words == FALSE) {
        unit<-""
        x<-formatC(x = value0, big.mark = ",", digits = 0, format = "f")
      } else {
        sigfig <- formatC(x = value0, digits = 3, format = "e")
        sigfig0 <- as.numeric(strsplit(x = sigfig, split = "e", fixed = TRUE)[[1]][2])
        
        if (sigfig0==0) {
          unit<-""
          x<-formatC(x = value0, big.mark = ",", digits = 0, format = "f")
          if (!is.null(val_under_x_words)) {
            if (as.numeric(value0) <= val_under_x_words & as.numeric(value0) >= 0) {
              x <- numbers2words(x = as.numeric(value0))
            }
          }
        } else if (sigfig0<=5) {
          # if (sigfig0<4) {
          unit<-""
          x<-formatC(x = value0, big.mark = ",", digits = 0, format = "f")
          # } else if (sigfig0>=4 & sigfig0<6) {
          #   unit<-" thousand"
          # x<-round(value0/1e3, digits = 1)
          # } else if (sigfig0==5) {
          #   unit<-" thousand"
          #   x<-round(value0/1e3, digits = 0)
        } else if (sigfig0>=6 & sigfig0<9) {
          unit<-" million"
          x<-round(value0/1e6, digits = 1)
        } else if (sigfig0>=9 & sigfig0<12) {
          unit<-" billion"
          x<-round(value0/1e9, digits = 1)
        } else if (sigfig0>=12) {
          unit<-" trillion"
          x<-round(value0/1e12, digits = 1)
        }
      }
      out0<-ifelse(combine==TRUE, paste0(x, unit), list(x, unit))
    }
    
    out<-c(out, out0)
  }
  
  sink()
  close(f)
  
  return(out)
}


xunitspct<-function(value, sign = TRUE) {
  out0<-c()
  for (iii in 1:length(value)){
    if (is.na(value)) {
      temp<-NA
    } else if (value > -1 & value <= 0 | #negative values between 0 and -1
               value < 1 & value >= 0) { #positive values between 1 and 0
      temp<-as.numeric(formatC(x = value, digits = 0, big.mark = ",", 
                               format = "f"))
    } else {
      temp<-as.numeric(round(value, digits = 0))
    }
    
    if (sign == F | is.na(value)) {
      out<-temp
    } else {
      out<-paste0(temp, "%")
    }
    out0<-c(out0, out)
  }
  
  return(out0)
  
}

#' Convert number to text string.
#'
#' Function by John Fox found here: http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html and https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
#' @param x The numbers that need to be converted to string.
#' @keywords Text editing
#' @export
#' @examples
#' numbers2words(x = 1890)
#' numbers2words(x = 3)
#' numbers2words(x = 1800090)
numbers2words <- function(x){
  # Function by John Fox found here: http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html and https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
  out <- c()
  for (ii in 1:length(x)) {
    if(x==0){
      print( "zero")
    } else{
      helper <- function(x){
        
        digits <- rev(strsplit(as.character(x), "")[[1]])
        nDigits <- length(digits)
        if (nDigits == 1) as.vector(ones[digits])
        else if (nDigits == 2)
          if (x <= 19) as.vector(teens[digits[1]])
        else trim(paste(tens[digits[2]],
                        Recall(as.numeric(digits[1]))))
        else if (nDigits == 3) trim(paste(ones[digits[3]], "hundred and",
                                          Recall(makeNumber(digits[2:1]))))
        else {
          nSuffix <- ((nDigits + 2) %/% 3) - 1
          if (nSuffix > length(suffixes)) stop(paste(x, "is too large!"))
          trim(paste(Recall(makeNumber(digits[
            nDigits:(3*nSuffix + 1)])),
            suffixes[nSuffix],"," ,
            Recall(makeNumber(digits[(3*nSuffix):1]))))
        }
      }
      trim <- function(text){
        #Tidy leading/trailing whitespace, space before comma
        text=gsub("^\ ", "", gsub("\ *$", "", gsub("\ ,",",",text)))
        #Clear any trailing " and"
        text=gsub(" and$","",text)
        #Clear any trailing comma
        gsub("\ *,$","",text)
      }
      makeNumber <- function(...) as.numeric(paste(..., collapse=""))
      #Disable scientific notation
      opts <- options(scipen=100)
      on.exit(options(opts))
      ones <- c("", "one", "two", "three", "four", "five", "six", "seven",
                "eight", "nine")
      names(ones) <- 0:9
      teens <- c("ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                 "sixteen", " seventeen", "eighteen", "nineteen")
      names(teens) <- 0:9
      tens <- c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty",
                "ninety")
      names(tens) <- 2:9
      x <- round(x)
      suffixes <- c("thousand", "million", "billion", "trillion")
      if (length(x) > 1) return(trim(sapply(x, helper)))
      
      
      out <- c(out, helper(x))
    }
  }
  return(out)
}


#' Convert number to text string.
#'
#' Convert number to text string to the 'st, 'nd, 'rd, or 'th.
#' @param x The numbers that need to be converted to string.
#' @param type How the numbers should be converted. Default = "word" (which produces "fifty-third"), but you can also select "val_th" (which produces "53rd").
#' @keywords Text editing
#' @export
#' @examples
#' numbers2words_th(x = 3, type = "val_th")
#' numbers2words_th(x = 3, type = "word")
numbers2words_th<-function(x, type = "word"){
  
  # type = col name = c("val_th", "word")
  
  # First
  first2twen<-data.frame(matrix(data = c("first",	"1st",
                                         "second",	"2nd",
                                         "third",	"3rd",
                                         "fourth",	"4th",
                                         "fifth",	"5th",
                                         "sixth",	"6th",
                                         "seventh",	"7th",
                                         "eighth",	"8th",
                                         "ninth",	"9th",
                                         "tenth",	"10th",
                                         "eleventh",	"11th",
                                         "twelfth",	"12th",
                                         "thirteenth",	"13th",
                                         "fourteenth",	"14th",
                                         "fifteenth",	"15th",
                                         "sixteenth",	"16th",
                                         "seventeenth",	"17th",
                                         "eighteenth",	"18th",
                                         "nineteenth",	"19th",
                                         "twentieth",	"20th"), ncol = 2, byrow =  T))
  names(first2twen)<-c("word", "val_th")
  first2twen$val<-1:20
  
  # Tens
  tens<-data.frame(matrix(data = c("twentieth", 20,
                                   "thirtieth", 30,
                                   "fortieth",	40,
                                   "fiftieth",	50,
                                   "sixtieth",	60,
                                   "seventieth",	70,
                                   "eightieth",	80,
                                   "ninetieth",	90), ncol = 2, byrow =  T))
  names(tens)<-c("word", "val")
  tens$word0<-paste0(substr(x = tens$word, start = 1, stop = nchar(tens$word)-4), "y")
  tens$val_th<-paste0(tens$val, "th")
  
  # Hundred
  hund<-data.frame(matrix(data = c(
    "hundredth", 100,
    "thousandth", 1000,
    "millionth",	1000000,
    "billionth",	1000000000,
    "trillionth",	1000000000000), ncol = 2, byrow =  T))
  names(hund)<-c("word", "val")
  hund$word0<-paste0(substr(x = hund$word, start = 1, stop = nchar(hund$word)-2), "")
  tens$val_th<-paste0(tens$val, "th")
  
  if (x %in% 1:20) {
    xx<-first2twen[first2twen$val %in% x, type]
  } else if (substr(x = x, start = nchar(x), stop = nchar(x)) %in% 0) {
    xx<-tens[tens$val %in% round(x = x, digits = -1), type]
  } else {
    
    if (type %in% "word") {
      xx<-paste0(tens[tens$val %in% as.numeric(paste0(substr(x = x, start = 1, stop = 1), 0)), "word0"],
                 "-",
                 first2twen[(first2twen$val %in% (x-as.numeric(paste0(substr(x = x, start = 1, stop = 1), 0)))), type])
    } else {
      x1<-substr(x = x, start = nchar(x), stop = nchar(x))
      stndrdth<-"th"
      if (x1 %in% 1) {
        stndrdth<-"st"
      } else if (x1 %in% 2) {
        stndrdth<-"nd"
      } else if (x1 %in% 3) {
        stndrdth<-"rd"
      }
      xx<-paste0(x, stndrdth)
      
    }
    
    
  }
  
  return(xx)
  
}

date_format<- "%B %d, %Y" # "%B %#d, %Y"

date_formatter <- function(date = "1998-09-02", date_format = "%B %d, %Y") {
  return(gsub("(\\D)0", "\\1", format(as.Date(date), date_format)))
}

# SameColNames<-function(df.ls) {
#   #All column names
#   colnames0<-c()
#   for (i in 1:length(df.ls)){
#     df0<-df.ls[[i]]
#     # colnames(df0)<-toupper(colnames(df0))
#     df0<-janitor::clean_names(df0)
#     df.ls[[i]]<-df0
#     colnames0<-c(colnames0, (colnames(df0)))
#   }
#   colnames0<-sort(unique(colnames0), decreasing = T)
#   
#   #New df's
#   df.ls0<-list()
#   df.rbind0<-c()
#   for (i in 1:length(df.ls)){
#     df0<-df.ls[[i]]
#     colnames.out<-colnames0[!(colnames0 %in% colnames(df0))]
#     if (length(colnames.out) != 0) {
#       for (ii in 1:length(colnames.out)){
#         df0[,(ncol(df0)+1)]<-NA
#         names(df0)[ncol(df0)]<-colnames.out[ii]
#       }
#     }
#     df0<-df0[,match(table =  colnames(df0), x = colnames0)]
#     df.ls0[[i]]<-df0
#     names(df.ls0)[i]<-names(df.ls)[i]
#     df.rbind0<-rbind.data.frame(df.rbind0, df0)
#   }
#   return(df.rbind0)
# }


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

#' Find the 'st, 'nd, or 'th of a value
#'
#' @param x a value you want the 'st, 'nd, or 'th of
#'
#' @return a character string of the appropriate 'st, 'nd, or 'th
#' @export
#'
#' @examples
#' stndth(3)
#' stndth(11)
#' stndth(112)
#' stndth(x = c(1120, 12))
stndth <- function(x) {
  out <- c()
  for (i in 1:length(x)){
    if (grepl(pattern = 1,
              x = substr(x = x[i],
                         start = nchar(x[i]),
                         stop = nchar(x[i])))) {
      stndth0 <- "st"
    } else if (grepl(pattern = 2,
                     x = substr(x = x[i],
                                start = nchar(x[i]),
                                stop = nchar(x[i])))) {
      stndth0 <- "nd"
    } else if (grepl(pattern = 3,
                     x = substr(x = x[i],
                                start = nchar(x[i]),
                                stop = nchar(x[i])))) {
      stndth0 <- "rd"
    } else {
      stndth0 <- "th"
    }
    out <- c(out, stndth0)
  }
  return(out)
}

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

## Load data files -------------------------------------------------------------

# Conversions -------------------------------------------------------------------

find_units <- function(unit = "", unt = "", dat, divby = NULL){
  
  # x <- ifelse(unit == "", "s", paste0(" ", unit))
  x <- unit#ifelse(unit != "", paste0(" ", unit), unit)
  x_ <- ifelse(unt =="", "", unt)
  
  # find appropriate units
  
  if (is.null(divby)) {
    min_val <- min(dat, na.rm = TRUE)
    min_val1 <- xunits(value = min_val, words = TRUE)
  } else {
    min_val <- divby
    min_val1 <- xunits(value = divby, words = TRUE)
  }
  
  # unit_word <- ""
  # unit_wrd <- ""
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



# https://github.com/geanders/weathermetrics/blob/master/R/temperature_conversions.R
c2f <- function (T.celsius, round = 2) {
  T.fahrenheit <- (9/5) * T.celsius + 32
  T.fahrenheit <- round(T.fahrenheit, digits = round)
  return(T.fahrenheit)
}

divnmi2forkm2 <- 1/3.429904
divkm2fornmi2 <- 3.429904
divkm2forha <- 100
divmforft <- 0.3048
divftform <- 3.28084

# Species -----------------------------------------------

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
    dplyr::select(-questions) %>% 
    dplyr::arrange((order))
  
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
                                     genus, species) %>% 
                       unique(), 
                     by = "species_code")  %>% 
    dplyr::mutate(taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish", 
      species_code >= 40001 ~ "invert")) %>% 
    dplyr::mutate(temp = trimws(gsub(pattern = "NA", 
                                     replacement = "", 
                                     paste0(genus, " ", species)))) %>% 
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
      # species_name == paste0(genus, " ", species),
      "ital", NA)) %>%
    dplyr::ungroup() %>% 
    dplyr::mutate(species_name0 = species_name, 
                  species_name1 = species_name, 
                  species_name0 = dplyr::if_else(is.na(type == "ital"), species_name0, paste0("*", species_name0, "*")), 
                  species_name0 = gsub(pattern = " spp.*", replacement = "* spp.", x = species_name0, fixed = TRUE), 
                  species_name0 = gsub(pattern = " sp.*", replacement = "* sp.", x = species_name0, fixed = TRUE), 
                  species_name = species_name0) %>% 
    dplyr::select(-type, -temp, -species_name0, -genus, -species)
  
  return(temp0)
}

# Plotting ---------------------------------------------------------------------


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
#' @param dat The data that will be used to create the IDW plots, containing the data used in lat, lon, and var. 
#' @param lat The name of the column in dat for latitude
#' @param lon The name of the column in dat for longitude
#' @param year The name of the column in dat for year
#' @param key.title A character string that will be used as the legend title
#' @param row0 How many rows in the face_wrap. Feeds from ggplot2::facet_wrap. 
#' @param region Defualt = "bs.south". Inherited from akgfmaps::make_idw_map. 
#' @param viridis_palette_option Defauly = "mako". Inherited from ggplot2::scale_fill_viridis_c. This will be the color scheme for the inverse distance weighted plot. 
#' @param plot_coldpool Default = TRUE Logical. Will plot cold pool outlines on the plot. 
#' @param plot_stratum Default = FALSE. Logical. This will plot stratun areas on the plot. 
#'
#' @return
#' @export
#'
#' @examples
#' figure <- plot_idw_xbyx(
#'   yrs = c(2017), 
#'   dat = akgfmaps:::YFS2017, 
#'   lat = "LATITUDE",
#'   lon = "LONGITUDE",
#'   year = "YEAR",
#'   key.title = "Yellowfin sole (kg/km2)", 
#'   grid = "extrapolation.grid",
#'   grid.cell = c(1.5, 1.5), # will take less time
#'   row0 = 1, 
#'   region = "bs.south") 
#' figure
plot_pa_facet <- function(
    yrs, 
    dat, 
    lat,
    lon,
    year,
    key.title = "", 
    row0 = 2, 
    reg_dat,
    viridis_palette_option = "mako", 
    plot_coldpool = FALSE, 
    plot_stratum = FALSE, 
    plot_bubble = FALSE, 
    legend_srvy_reg = TRUE) {
  
  yrs <- as.numeric(sort(x = yrs, decreasing = T))
  
  if (plot_bubble){
    dat0 <- dat %>%
      dplyr::rename(year = as.character(year), 
                    lat = as.character(lat), 
                    lon = as.character(lon)) %>% 
      dplyr::select(year, lat, lon, cpue_kgha) %>% 
      dplyr::mutate(year = as.numeric(year), 
                    latdd = as.numeric(lat), 
                    londd = as.numeric(lon), 
                    cpue_kgha = as.numeric(cpue_kgha))
    d <- dat0[,c("londd", "latdd", "year", "cpue_kgha")]
  } else {
    dat0 <- dat %>%
      dplyr::rename(year = as.character(year), 
                    lat = as.character(lat), 
                    lon = as.character(lon)) %>% 
      dplyr::select(year, lat, lon) %>% 
      dplyr::mutate(year = as.numeric(year), 
                    latdd = as.numeric(lat), 
                    londd = as.numeric(lon))
    d <- dat0[,c("londd", "latdd", "year")]
  }
  
  
  coordinates(d) <- c("londd", "latdd")
  sp::proj4string(d) <- CRS("+proj=longlat +datum=WGS84")
  dd <- data.frame(sp::spTransform(d, CRS(as.character(reg_dat$crs)[1])))
  # dd <- as(res, "SpatialPoints") ## For a SpatialPoints object rather than a SpatialPointsDataFrame
  
  
  figure <- ggplot() +
    geom_sf(data = reg_dat$akland,
            color = NA,
            fill = "grey50")  +  
    geom_sf(data = reg_dat$graticule,
            color = "grey80",
            alpha = 0.2) +
    ggplot2::scale_y_continuous(name = "", #"Latitude", 
                                limits = reg_dat$plot.boundary$y,
                                breaks = reg_dat$lat.breaks) +
    ggplot2::scale_x_continuous(name = "", #"Longitude", 
                                limits = reg_dat$plot.boundary$x,
                                breaks = reg_dat$lon.breaks) +
    # ggplot2::geom_sf(
    #   data = reg_dat$survey.area, 
    #   mapping = aes(color = SURVEY, 
    #                 geometry = geometry), 
    #   fill = "transparent", 
    #   # shape = NA, 
    #   linewidth = ifelse(row0 > 2, 1.5, 1), # size
    #   show.legend = legend_srvy_reg) +
    # ggplot2::scale_color_manual(
    #   name = " ", 
    #   values = reg_dat$survey.area$color,
  #   breaks = reg_dat$survey.area$SURVEY,
  #   labels = reg_dat$survey.area$SRVY) 
  ggplot2::geom_sf(
    data = reg_dat$survey.area, 
    mapping = aes(color = SURVEY, 
                  geometry = geometry), 
    fill = "transparent", 
    # shape = NA, 
    linewidth = ifelse(row0 > 2, 1.5, 1), # size
    show.legend = legend_srvy_reg) +
    ggplot2::scale_color_manual(
      name = " ", 
      values = reg_dat$survey.area$color,
      breaks = reg_dat$survey.area$SURVEY,
      labels = reg_dat$survey.area$SRVY) 
  
  # figure <- ggplot() +
  #   geom_sf(data = reg_dat$akland,
  #           color = NA,
  #           fill = "grey50") #+
  # # geom_sf(data = reg_dat$graticule,
  # #         color = "grey80",
  # #         alpha = 0.2)
  # 
  # if (length(length(reg_dat$survey.area$color))>1 ) {
  if (plot_bubble) {
    figure <- figure   + 
      geom_point(data = dd, 
                 mapping = aes(x = londd, y = latdd,
                               size = cpue_kgha,
                               group = as.factor(year)), 
                 color = mako(n = 1, begin = .25, end = .75),
                 shape = 16,
                 # size = 1.5,
                 show.legend = TRUE,
                 na.rm = TRUE) +
      scale_size_continuous(
        name = paste0(key.title, "weight CPUE (kg/ha)"), 
        range = c(1,4))
  } else {
    figure <- figure   + 
      geom_point(data = dd, 
                 mapping = aes(x = londd, y = latdd,
                               # shape = key.title,
                               group = as.factor(year)), 
                 color = mako(n = 1, begin = .25, end = .75),
                 shape = 16,
                 size = 1.5,
                 show.legend = FALSE,
                 na.rm = TRUE)    
  }
  
  # figure <- figure  +
  #   geom_sf(data = reg_dat$survey.area, # %>% 
  #           # dplyr::filter(SRVY %in% SRVY1), 
  #           mapping = aes(color = SURVEY), 
  #           fill = NA, 
  #           shape = NA, 
  #           size = ifelse(row0 > 2, 0.25, 0.75),
  #           show.legend = TRUE) +
  #   scale_color_manual(
  #     name = "", # key.title,
  #     values = reg_dat$survey.area$color,
  #     breaks = rev(reg_dat$survey.area$SURVEY), 
  #     labels = rev((reg_dat$survey.area$SRVY)))
  # } else {
  #   figure <- figure   + 
  #     geom_point(data = dd, 
  #                mapping = aes(x = londd, y = latdd, 
  #                              shape = key.title,
  #                              group = as.factor(year)), 
  #                color = mako(n = 1, begin = .25, end = .75),
  #                # shape = 16,
  #                size = 2,
  #                show.legend = TRUE,
  #                na.rm = TRUE) 
  # }
  
  # if (plot_coldpool) {
  #   temp_break <- 2 # 2*C
  #   
  #   if (unique(dat$SRVY) %in% "EBS") {
  #     cp <- coldpool::ebs_bottom_temperature
  #   } else if (unique(dat$SRVY) %in% "NBS") {
  #     cp <- coldpool::nbs_ebs_bottom_temperature
  #   }
  #   
  #   coords <- raster::coordinates(cp)
  #   
  #   for(i in 1:length(yrs)) {
  #     sel_layer_df <- data.frame(x = coords[,1],
  #                                y = coords[,2],
  #                                temperature = cp@data@values[,i])
  #     sel_layer_df <- sel_layer_df[!is.na(sel_layer_df$temperature),]
  #     sel_layer_df$year <- yrs[i]
  #     
  #     if(i == 1) {
  #       bt_year_df <- sel_layer_df
  #     } else{
  #       bt_year_df <- dplyr::bind_rows(bt_year_df, sel_layer_df)
  #     }
  #   }
  #   
  #   figure <- figure +
  #     ggplot2::geom_tile(data = bt_year_df %>%
  #                          dplyr::filter(temperature <= temp_break), #%>% 
  #                          # dplyr::rename(new_dim = year),
  #                        aes(x = x,
  #                            y = y, 
  #                            group = year),
  #                        fill = "magenta", 
  #                        alpha = 0.25, 
  #                        show.legend = FALSE)
  #   
  # }  
  
  if (plot_coldpool) {
    temp_break <- 2 # 2*C
    
    if (sum(dat$SRVY %in% "EBS")>0) {
      cp <- coldpool::ebs_bottom_temperature
    } else if (unique(dat$SRVY) %in% "NBS") {
      cp <- coldpool::nbs_ebs_bottom_temperature
    }
    
    temp <- c()
    outline <- c()
    for (i in 1:length(yrs)){
      #   temp <- c(temp, which(grepl(pattern = yrs[i], x = names(cp))))
      # }
      temp <- which(grepl(pattern = yrs[i], x = names(cp)))
      
      cp0 <- cp[[temp]]#[[which(grepl(x = names(cp), pattern = 2019))]] # cp[[temp[2]]]
      values(cp0)[values(cp0) <= temp_break] <- 1
      values(cp0)[values(cp0) > temp_break] <- NA
      pp <- rasterToPolygons(x = cp0, na.rm = TRUE, dissolve=TRUE)
      
      outline <- rbind(outline, 
                       pp %>% 
                         sp::geometry(obj = .) %>% 
                         sf::st_as_sf(x = .) %>% 
                         dplyr::mutate(new_dim  = yrs[i]))
      
    }
    
    figure <- figure +
      geom_sf(data = outline %>%
                sf::st_cast(x = ., to = "MULTIPOLYGON"), 
              size = 1, 
              fill = NA, # alpha(colour = "red", alpha = 0.3),
              color = alpha(colour = "red", alpha = 0.3))
    # fill = alpha(colour = "yellow", alpha = 0.3), 
    # color = alpha(colour = "yellow", alpha = 0.3))
  }
  
  if (length(yrs) == 0) { # if there is no data to plot
    grid <- ""
    figure <- figure +
      ggplot2::geom_text(mapping = aes(x = mean(reg_dat$lon.breaks), 
                                       y = mean(reg_dat$lat.breaks), 
                                       label = "No data was available\nfor this species in this\nregion for this year."), 
                         fontface="bold")
  } else if (length(yrs)>1) { # if there is data to plot
    figure <- figure +
      facet_wrap( ~ year, nrow = row0) +
      coord_sf() # coord_equal() 
  }
  
  
  if (plot_stratum) {
    figure <- figure +
      geom_sf(data = reg_dat$survey.strata,
              color = "grey50",
              size = 0.1,
              # alpha = 0,
              fill = NA)
  }
  
  if (plot_bubble) {
    figure <- figure +
      guides(
        size = guide_legend(order = 1, 
                            title.position = "top", 
                            label.position = "top",
                            title.hjust = 0.5, 
                            nrow = 1), 
        color = guide_legend(order = 2, 
                             label.position = "right",
                             title.hjust = 0.5,
                             nrow = 1)) 
  } else {
    figure <- figure +
      # guides(
      #   color = guide_legend(title = key.title, 
      #                        title.position = "top",  
      #                        label.position = "right",
      #                        title.hjust = 0.5,
      #                        nrow = 1)) 
      ggplot2::guides(
        # size = guide_legend(override.aes = list(size = 10)),
        fill = guide_legend(
          # order = 1,
          title.position = "top",
          label.position = "right",
          title.hjust = 0.5,
          override.aes = list(color = NA),
          nrow = 1),
        color = guide_legend(
          title = key.title, 
          order = 2, 
          label.position = "right",
          override.aes = list(#lwd = 10, 
            fill = reg_dat$survey.area$color, 
            color = reg_dat$survey.area$color), 
          title.hjust = 0.5,
          nrow = 1))
    
  }
  
  figure <- figure +
    theme(  #set legend position and vertical arrangement
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      axis.text = element_text(size = ifelse(length(yrs)>4 & row0 == 1, 6, 8)),
      strip.background = element_blank(), 
      strip.text = element_text(size = 10, face = "bold"), 
      # legend.title = ,element_blank(),
      legend.text = element_text(size = 9),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.position = "bottom",
      legend.box = "horizontal")# ifelse(plot_bubble, "vertical", "horizontal"))
  
  
  return(figure)
  
}

#' Plot temperature facet grid
#' 
#' Generate multipanel temperature plot from a raster brick.
#'
#' @param raster_nebs 
#' @param raster_ebs 
#' @param key.title Title for multipanel legend as a character vector or expression 
#' @param reg_data list containing regional survey strata, land polygon, etc. from akgfmaps::get_base_layer()
#' @param colorbar_breaks numeric vector of breaks to use for temperature plots
#' @param yrs 
#' @param viridi_palette_option Viridis palette option passed to viridis::viridis_pal(). Default = "H" (turbo)
#' @param row0 
#' @param title0 
#' @param legend_seperate 
#' @param temperature_zscore Logical. Default  = F. This will add a + to denote when that year's EBS temperatures were a standard deviation above or a "-" for below. 
#' @param out_crs
#'
#' @return
#' @export
#'
#' @examples
plot_temperature_map <- function(raster_nebs, 
                                   raster_ebs, 
                                   key.title = "Temperature (°C)", 
                                   reg_dat, 
                                   colorbar_breaks = c(-Inf, seq(from = 0, to = 8, by = 2), Inf),
                                   yrs,
                                   yrs_nbs, 
                                   viridis_palette_option = "H", 
                                   row0 = 2, 
                                   title0 = NULL, 
                                   legend_seperate = FALSE, 
                                   temperature_zscore = FALSE, 
                                   legend_font_size = 12, 
                                   out_crs = "EPSG:3338") {
  
  for (ii in 1:length(yrs)) {
    
    if (!(2020 %in% yrs[ii])) {
      
      if (yrs[ii] %in% yrs_nbs) {
        sel_raster_layer <- raster_nebs[[which(names(raster_nebs) == yrs[ii])]]
        mask_layer <- reg_dat0$survey.area
      } else {
        sel_raster_layer <- raster_ebs[[which(names(raster_ebs) == yrs[ii])]]
        mask_layer <- dplyr::filter(reg_dat0$survey.area, SURVEY == "EBS_SHELF")
      }
      
      
      temp_sf <- sel_raster_layer |>
        terra::mask(mask_layer, touches = TRUE) |>
        as.data.frame(na.rm = TRUE, xy = TRUE) |>
        dplyr::arrange(x, y) |>
        sf::st_as_sf(coords = c("x", "y"),
                     crs = out_crs) |>
        tidyr::pivot_longer(cols = 1) |>
        dplyr::rename(year = name,
                      bt = value)  |>
        stars::st_rasterize()
      
      temp_sf$bt <- cut(temp_sf$bt, colorbar_breaks)
      
      temp_sf <- sf::st_as_sf(temp_sf) |>
        dplyr::group_by(bt) |>
        dplyr::summarise(n = n()) |>
        dplyr::select(-n) |>
        sf::st_intersection(mask_layer) |>
        rmapshaper::ms_simplify(keep_shapes = TRUE,
                                keep = 0.04)
      
      temp_sf$year <- yrs[ii]
      
      if(ii == 1) {
        temperature_sf <- temp_sf
      } else {
        temperature_sf <- dplyr::bind_rows(temperature_sf, temp_sf)
      }
    }
  }
  
  if (2020 %in% yrs) {
    temperature_sf <- dplyr::bind_rows(temperature_sf %>% 
                                         head(n = 1) %>% 
                                         dplyr::mutate(bt = NA, 
                                                       year = 2020) , 
                                       temperature_sf)
  }
  
  temperature_zscore1 <- coldpool::cold_pool_index %>% 
    dplyr::rename(var = dplyr::all_of(ifelse(case == "bottom", "MEAN_GEAR_TEMPERATURE", "MEAN_SURFACE_TEMPERATURE")), 
                  year = YEAR) %>%
    dplyr::select(year, var) %>% 
    dplyr::filter(year <= maxyr) %>%
    dplyr::arrange(var) %>% 
    dplyr::mutate(
      sd = sd(var, na.rm = TRUE), 
      mean = mean(var, na.rm = TRUE), 
      zscore  = ((var-mean)/sd), # z = (x-μ)/σ
      sign = dplyr::case_when(
        zscore <= -1 ~ "-", 
        zscore >= 1 ~ "+"), 
      color = dplyr::case_when(
        sign == "+" ~ negative, 
        sign == "-" ~ positive)) %>% 
    dplyr::arrange(year) %>% 
    dplyr::filter(year %in% yrs) 
  
  # Setup data.frame for 2020 year with no survey
  panel_extent <- reg_dat$plot.boundary |>
    sf::st_as_sf(coords = c("x", "y")) |>
    sf::st_buffer(dist = 1e6) |>
    sf::st_bbox()
  
  panel_extent <- data.frame(x = panel_extent[c('xmin', 'xmax')],
                             y = panel_extent[c('ymin', 'ymax')])
  
  fig_palette <- viridis::viridis_pal(end = 0.9, 
                                      option = viridis_palette_option)(length(colorbar_breaks)-1)
  
  figure <- ggplot() +
    ggplot2::geom_sf(data = reg_dat$akland,
                     color = NA,
                     fill = "grey50")+  
    ggplot2::geom_sf(data = reg_dat$graticule,
                     color = "grey80",
                     alpha = 0.2)  +
    ggplot2::geom_sf(data = temperature_sf,
            mapping = aes(fill = bt),
            color = NA) +
    ggplot2::facet_wrap( ~ year, 
                         nrow = row0) +
    ggplot2::scale_fill_manual(values = fig_palette, 
                               drop = FALSE, 
                               na.translate = FALSE) +
    ggplot2::geom_sf(data = reg_dat$survey.area,
                     color = "grey50",
                     fill = NA,
                     size = rel(0.2)) + 
    ggplot2::scale_y_continuous(name = "", #"Latitude", 
                                limits = reg_dat$plot.boundary$y, 
                                breaks = reg_dat$lat.breaks) +
    ggplot2::scale_x_continuous(name = "", #"Longitude", 
                                limits = reg_dat$plot.boundary$x,
                                breaks = reg_dat$lon.breaks) + 
    #set legend position and vertical arrangement
    ggplot2::guides(
      fill = guide_legend(title.position="top",
                          label.position = "bottom",
                          title.hjust = 0.5, nrow = 1)) +
    
    ggplot2::theme(
      legend.text = element_text(size = legend_font_size-2),
      panel.background = element_rect(fill = "white",
                                      colour = NA),
      panel.border = element_rect(fill = NA,
                                  colour = "grey20"),
      axis.text = element_text(size = legend_font_size-2),
      strip.background = element_blank(),
      strip.text = element_text(size = legend_font_size, face = "bold"),
      legend.position = "none", 
      plot.margin=grid::unit(c(0,0,0,0), "mm") )
  
  if (temperature_zscore) {
    
    figure <- figure +
      ggplot2::geom_label(data = temperature_zscore1, 
                          aes(label = sign, 
                              color = sign,
                              x = quantile(x = reg_dat$plot.boundary$x, .96),
                              y = quantile(x = reg_dat$plot.boundary$y, .96) ), 
                          fontface = "bold", 
                          fill = "grey20",
                          label.size = NA,
                          show.legend = FALSE) +
      ggplot2::scale_colour_manual(
        na.value = "transparent",
        breaks = (unique(temperature_zscore1$sign)), 
        labels = (unique(temperature_zscore1$sign)), 
        values = (unique(temperature_zscore1$color)))
  }
  
  if (2020 %in% temperature_sf$year)  {
    
    label_2020 <- data.frame(x = mean(panel_extent$x),
                             y = mean(panel_extent$y),
                             label = "No\nSurvey",
                             year = 2020)
    
    figure <- figure +
      ggplot2::geom_label(data = label_2020,
                          aes(x = x,
                              y = y,
                              label = label),
                          label.size = NA, 
                          fill = NA)
  }
  
  if (!is.null(title0)) {
    figure <- figure +
      ggplot2::ggtitle(label = title0)
  }
  
  cbar_legend <- coldpool::legend_discrete_cbar(breaks = colorbar_breaks,
                                                  colors = fig_palette,
                                                  legend_direction = "horizontal",
                                                  font_size = 3,
                                                  width = 0.1,
                                                  expand_size.x = 0.3,
                                                  expand_size.y = 0.3,
                                                  expand.x = 0.3,
                                                  expand.y = 0.9,
                                                  spacing_scaling = 1.2,
                                                  text.hjust = 0.5,
                                                  font.family = "sans",
                                                  neat.labels = FALSE) + 
    ggplot2::annotate("text", 
                      y = mean(colorbar_breaks[c(2:(length(colorbar_breaks)-1))]),
             x = 1.15,
             # y = 3.5,
             label =  key.title,
             size = rel(3.2))
  
  if (legend_seperate) { 
    figure_and_legend <- list("figure" = figure, 
                              "legend" = cbar_legend)
    
  } else {
    figure_and_legend <- cowplot::plot_grid(figure,
                                            cbar_legend,
                                            nrow = 2,
                                            rel_heights = c(0.85,0.15))
  }
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
#' @param lengths0 data.frame of sampled lengths
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
                          lengths0 = NULL,
                          spp_code, 
                          spp_print, 
                          type = "length", 
                          print_n = FALSE, 
                          ridgeline = FALSE, 
                          unit0 = NULL, 
                          legend_font_size = 8){
  
  table_raw <- sizecomp0 %>%
    dplyr::arrange(year, SRVY, sex, length_mm) 
  
  # find appropriate units
  a <- find_units(unit = "", unt = "", dat = max(table_raw$population_count, na.rm = TRUE))
  for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }
  pop_unit <- divby
  pop_unit_word <- unit_word
  
  if (is.null(unit0)){ # mm vs cm
    len_unit_word0 <- ifelse(!grepl(pattern = " crab", x = spp_print, ignore.case = TRUE),  "cm", "mm")
  } else {
    len_unit_word0 <- unit0
  }
  
  table_raw <- table_raw %>%
    dplyr::mutate(population_count = population_count/pop_unit, 
                  length_mm = round(
                    x = length_mm/ifelse(len_unit_word0 == "mm", 1, 10), digits = 0)) %>%
    dplyr::ungroup()
  
  len_unit_axis <- ifelse(max(table_raw$length_mm)-min(table_raw$length_mm)>150, 50, 
                          ifelse(max(table_raw$length_mm)-min(table_raw$length_mm)>45, 10, 5))
  
  dat_text <- lengths0 %>% 
    dplyr::group_by(SRVY, year) %>% 
    dplyr::summarise(frequency = formatC(x = sum(frequency, na.rm = TRUE), 
                                         digits = 0, big.mark = ",", format = "f")) %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(year %in% unique(as.numeric(paste(table_raw$year)))) %>% 
    dplyr::mutate(
      label = paste0(c("# measured: ", rep_len(x = "", length.out = (nrow(.)-1))), 
                     frequency), 
      label = gsub("\\s", " ", formatC(x = label))) %>% 
    dplyr::select(-frequency) %>% 
    dplyr::ungroup()
  
  table_raw <- dplyr::left_join(table_raw, dat_text)
  
  if (!ridgeline) { # facet plot without ridgeline
    
    figure <- ggplot(data = table_raw,
                     mapping = aes(x = length_mm,
                                   y = population_count,
                                   # group = SRVY_long,
                                   fill = sex)) +
      geom_bar(position="stack", stat="identity", na.rm = TRUE) +
      scale_fill_viridis_d(direction = -1, 
                           option = "mako",
                           begin = .2,
                           end = .6,
                           na.value = "transparent") +
      guides(fill=guide_legend(title="")) +
      scale_y_continuous(name = paste0(spp_print, " population ",pop_unit_word), 
                         breaks = function(population_count) unique(floor(pretty(seq(0, (max(population_count) + 1) * 1.1))))) +
      scale_x_continuous(name = stringr::str_to_sentence(paste0(type," (", len_unit_word0, ")")), 
                         breaks = function(length_mm) unique(floor(pretty(seq(0, (max(length_mm) + 1) * 1.1))))) +
      facet_grid(year ~ SRVY_long,
                 scales = "free_x")  +
      ggplot2::guides(
        fill = guide_legend(title.position = "top", 
                            title.hjust = 0.5,
                            nrow = 1 )) +
      ggplot2::theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill = NA,
                                    colour = "grey20"),
        legend.title = element_blank(), 
        legend.text = element_text(size = legend_font_size),
        legend.background = element_rect(colour = "transparent", 
                                         fill = "transparent"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.position = "bottom",
        legend.box = "horizontal" )
    
  } else {
    table_raw1 <- table_raw %>% 
      dplyr::ungroup() %>% 
      dplyr::group_by(year, length_mm, SRVY_long) %>% 
      dplyr::summarise(population_count = sum(population_count, na.rm = TRUE))
    
    temp <- setdiff(min(table_raw1$year, na.rm = TRUE):max(table_raw1$year, na.rm = TRUE), 
                    unique(table_raw1$year))
    if (length(temp)>0) {
      table_raw1 <- dplyr::bind_rows(
        data.frame(year = temp,
                   length_mm = 0, 
                   population_count = 0, 
                   SRVY_long = unique(table_raw1$SRVY_long)), 
        table_raw1)
    }
    
    table_raw1 <- table_raw1 %>% 
      dplyr::arrange(desc(year)) 
    
    table_raw1$year <- as.numeric(paste(table_raw1$year))
    table_raw1$year <- factor(
      x = table_raw1$year,
      levels = as.character(sort(unique(table_raw1$year), decreasing = TRUE)),
      labels = as.character(sort(unique(table_raw1$year), decreasing = TRUE)),
      ordered = TRUE)
    
    every_nth = function(n, true1) {
      if (true1) {
        return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
      } else {
        return(function(x) {x[c(FALSE, rep(TRUE, n - 1))]})
      }
    }
    
    figure <- ggplot(data = table_raw1, 
                     mapping = aes(x = length_mm, 
                                   y = year, 
                                   fill = length_mm, 
                                   height = population_count/mean(population_count, na.rm = TRUE))) +
      ggridges::geom_ridgeline_gradient() +
      scale_fill_viridis_c(option = "G") +
      scale_x_continuous(name = stringr::str_to_sentence(paste0(type," (", len_unit_word0, ")")), 
                         breaks = function(length_mm) unique(floor(pretty(seq(0, (max(length_mm) + 1) * 1.1))))) +
      scale_y_discrete(name = paste0(spp_print, " population across years"), 
                       breaks = every_nth(n = 2, true1 = ((max(table1$year, na.rm = TRUE) %% 2) == 1))) + 
      theme(legend.position = "none", 
            panel.grid.major.x = element_line(colour = "grey80")) +
      facet_wrap(vars(SRVY_long))
  }
  
  figure <- figure + 
    ggplot2::theme(panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(colour = "grey80"),
          panel.grid.minor.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank(),
          strip.text = element_text(size = legend_font_size, face = "bold"),
          legend.text = element_text(size = 9),
          legend.key = element_rect(colour = "transparent",
                                    fill = "transparent"),
          axis.title = element_text(size = legend_font_size, face = "bold"),
          axis.text = element_text(size = legend_font_size))
  
  if (print_n & !is.null(lengths0)) {
    
    figure <- figure +
      ggplot2::geom_text(mapping = aes(label = label, 
                                       x = (quantile(x = range(table_raw$length_mm), .8))[[1]], 
                                       y = (quantile(x = range(table_raw$population_count), .93))[[1]]), 
                         check_overlap = TRUE) 
  }
  
  return(figure)
}


plot_timeseries <- function(
    dat, 
    unit = "", 
    unt = "", 
    y_long = "", 
    error_bar = TRUE, 
    spp_print = "", 
    mean_in_legend = TRUE, 
    yrs_plotted = NULL, 
    legend_font_size = 12){
  
  a <- find_units(unit, unt, dat = dat$y) 
  for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) } 
  
  table_raw <- dat %>% 
    dplyr::arrange(-year) %>% 
    dplyr::mutate(y = y/a$divby) 
  
  table_raw_mean <- table_raw %>% 
    dplyr::group_by(SRVY_long, SRVY) %>% 
    dplyr::summarise(y = mean(y, na.rm = TRUE), 
                     minyr = min(year, na.rm = TRUE), 
                     maxyr = max(year, na.rm = TRUE))  %>% 
    dplyr::mutate(SRVY_long1 = SRVY_long,
                  yy = y*divby) #%>% 
    # dplyr::filter(yy>100)
  
  if (error_bar) {
    table_raw <- table_raw %>% 
      dplyr::mutate(y_ci_dw = y_ci_dw/divby, 
                    y_ci_up = y_ci_up/divby)
  }
  yr_missing <- data.frame()
  for (i in 1:length(unique(table_raw$SRVY))){
    temp <- table_raw[table_raw$SRVY %in% unique(table_raw$SRVY)[i], ]
    yr_missing <- dplyr::bind_rows(
      yr_missing, 
      data.frame(yr_missing = setdiff((min(temp$year):max(temp$year)),
                                      unique(temp$year)),
                 col = unique(table_raw$col)[i], 
                 SRVY = unique(table_raw$SRVY)[i], 
                 SRVY_long = unique(table_raw$SRVY_long[table_raw$SRVY == unique(table_raw$SRVY)[i]])))
  }
  
  if (nrow(yr_missing) > 0) {
    temp <- unique(table_raw[,c("print_name")])
    
    temp1 <- data.frame(matrix(data = NaN, nrow = nrow(temp)*nrow(yr_missing),
                               ncol = ncol(table_raw)))
    names(temp1) <- names(table_raw)
    temp1$print_name <- temp$print_name
    temp1 <- dplyr::arrange(temp1, SRVY)
    temp1$year <- yr_missing$yr_missing
    temp1$SRVY <- yr_missing$SRVY
    temp1$SRVY_long <- yr_missing$SRVY_long
    temp1$col <- yr_missing$col
    temp1$common_name <- as.character(temp)
    temp1$species_name <- unlist(unique(table_raw[,c("species_name")]))
    temp1$taxon <- unlist(unique(table_raw[,c("taxon")]))
    table_raw <- dplyr::bind_rows(temp1, table_raw)
  }
  
  if (mean_in_legend){
    table_raw_mean <- table_raw_mean %>% 
      dplyr::mutate(SRVY_long1 = paste0(SRVY_long, #"\n
                                        " (mean = ", 
                                        formatC(x = y, digits = 1, big.mark = ",", format = "f"), " ",
                                        unit_wrd, ")"))
  }
  anno<-NA
  temp<-setdiff(x = unique(table_raw$SRVY), 
                y = unique(table_raw_mean$SRVY))
  if (length(temp) != 0) {
    anno <- paste0("Data for this species in the\n", 
                   temp, 
                   " is too limited to plot.")
    
    # which(unique(table_raw$SRVY)==temp)
    
    col_anno <- table_raw$col[which(unique(table_raw$SRVY)==temp)]
    col <- table_raw$col[which(unique(table_raw$SRVY)!=temp)]
  }
  
  table_raw <- 
    dplyr::left_join(
      x = table_raw, 
      y = table_raw_mean %>% 
        dplyr::select(SRVY_long, SRVY_long1), 
      by = "SRVY_long") %>% 
    dplyr::filter(SRVY %in% table_raw_mean$SRVY)
  
  if (is.null(yrs_plotted)) {
    yrs_plotted <- min(table_raw$year, na.rm = TRUE):max(table_raw$year, na.rm = TRUE)
  }
  
  figure <-
    ggplot(data = table_raw %>% 
             dplyr::filter(year >= min(yrs_plotted, na.rm = TRUE) &
                             year <= max(yrs_plotted, na.rm = TRUE)), 
           mapping = aes(x = year, 
                         y = y, 
                         color = SRVY_long1, 
                         group = SRVY_long1)) 
  
  if (mean_in_legend){
    figure <- figure  +
      ggplot2::geom_segment(
        data = table_raw_mean,
        mapping = aes(x = min(yrs_plotted), 
                      xend = max(yrs_plotted), 
                      y = y, 
                      yend = y),
        alpha = .5,
        linetype = "dashed", 
        linewidth = 2) 
  }
  
  figure <- figure +
    geom_line(size = 1) +
    geom_point(size = 1.5) + 
    ggplot2::scale_x_continuous(labels = scales::label_number(accuracy = 1, big.mark = ""), 
                                limits = range(yrs_plotted, na.rm = TRUE))  +
    ggplot2::scale_color_manual(values = unique(table_raw$col))
  
  if (error_bar) {
    figure <- figure +
      geom_errorbar(aes(ymin=y_ci_dw, ymax=y_ci_up), width=.2,
                    # position=position_dodge(.9), 
                    alpha = 0.5)
  }
  
  
  
  if (!is.na(anno)) {
    figure <- figure +
      ggplot2::geom_text(x = Inf, 
                         y = -Inf, 
                         hjust = 1.2, 
                         vjust = -.3, 
                         label = anno, 
                         show.legend = FALSE, 
                         size = 4, 
                         fontface = "italic",
                         color = col_anno)
  }
  
  figure <- figure +
    ggplot2::scale_y_continuous(
      name = paste0(stringr::str_to_sentence(spp_print), " ", tolower(y_long), "\n", unit_word), 
      labels = scales::comma)
  
  figure <- figure +
    ggplot2::guides(color = guide_legend(title="", nrow = 2)) +
    ggplot2::xlab(label = "Year") +
    ggplot2::theme(
      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_line(colour = "grey95"),
      panel.border = element_rect(fill = NA, colour = "grey20"),
      legend.title = element_blank(), 
      legend.text = element_text(size = legend_font_size),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent",
                                fill = "transparent"),
      axis.title = element_text(size = legend_font_size, face = "bold"),
      axis.text = element_text(size = legend_font_size),
      legend.position = "bottom", 
      legend.box = "horizontal"
    )
  
  return(figure)
}


plot_survey_stations <- function(reg_dat, 
                                 survey_outline = TRUE, 
                                 stratum_grid = FALSE, 
                                 stratum_no = FALSE, 
                                 station_pts = "stn", # c("pts", "ves", "names")
                                 bathymetry = FALSE, 
                                 study = FALSE, 
                                 place_labels = TRUE
) {
  
  figure <- ggplot()  +
    ggplot2::geom_sf(data = reg_dat$akland, 
            color = NA, 
            fill = "grey80") +
    ggplot2::geom_sf(data = reg_dat$graticule, 
            color = NA,
            fill = "grey50")+
    ggplot2::scale_y_continuous(name = "Latitude", 
                                limits = reg_dat$plot.boundary$y,
                                breaks = reg_dat$lat.breaks) +
    ggplot2::scale_x_continuous(name = "Longitude", 
                                limits = reg_dat$plot.boundary$x,
                                breaks = reg_dat$lon.breaks) + 
    ggplot2::theme( # set legend position and vertical arrangement
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      strip.background = element_rect(fill = "grey85", 
                                      colour = "grey20"),
      legend.spacing.y = unit(-0.35, "cm"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 7),
      legend.background=element_blank(),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.position = c(.15, .15),
      legend.box.just = "left",
      legend.box = "vertical"
    )
  
  if (study) {
    
    study <- reg_dat$survey.grid %>% dplyr::filter(!is.na(study))
    
    figure <- figure  +
      geom_sf(data = reg_dat$survey.grid %>% dplyr::filter(!is.na(study)),
              mapping = aes(fill = study),
              show.legend = TRUE,
              color = "black",
              size = .5, 
              # alpha = .5,
              na.rm = TRUE) +
      scale_fill_manual(
        name = "", #"Survey Vessels",
        values = unique(reg_dat$survey.grid$study_col),
        breaks = unique(reg_dat$survey.grid$study),
        labels = unique(reg_dat$survey.grid$study_long),
        na.value = "transparent")
  }
  
  if (stratum_grid) {
    figure <- figure  +
      geom_sf(data = reg_dat$survey.strata, 
              fill = NA, 
              color = "grey50")
  }
  
  if (bathymetry) { 
    figure <- figure  +
      geom_sf(data = reg_dat$bathymetry, 
              fill = NA, 
              color = "grey50")
  }
  
  if (survey_outline) { 
    figure <- figure +
      ggplot2::geom_sf(data = reg_dat$survey.area, 
                       aes(color = SURVEY), 
                       fill = NA, 
                       linewidth = 2, 
                       show.legend = TRUE) +
      ggplot2::scale_color_manual(
        name = "",
        values = reg_dat$survey.area$color,
        breaks = reg_dat$survey.area$SURVEY,
        labels = stringr::str_to_title(reg_dat$survey.area$SRVY_long))
  }
  
  if (station_pts == "names") {
    figure <- figure +
      geom_sf(data = reg_dat$survey.grid, 
              color = "grey20", 
              fill = NA) +
      geom_sf_text(data = reg_dat$survey.grid, 
                   lineheight = 0.7,
                   mapping = aes(label = gsub(x = STATIONID, 
                                              replacement = "\n", 
                                              pattern = "-")),
                   color = "black", 
                   size = 1.5, 
                   show.legend = FALSE) 
    
  } else if (station_pts == "pts") {
    figure <- figure +
      stat_sf_coordinates(
        data = reg_dat$survey.grid,
        mapping = aes(shape = in_maxyr),
        color = "grey50",
        size = 2, 
        show.legend = FALSE, 
        na.rm = TRUE)  + 
      ggplot2::scale_shape_manual(
        name = "", 
        values = c(16, 1), 
        breaks = c(TRUE, FALSE)) + 
      ggplot2::guides(shape = "none")
    
  } else if (station_pts == "vess") {
    figure <- figure  +
      geom_sf(data = reg_dat$survey.area, 
              aes(color = reg_dat$survey.area$SURVEY, 
                  shape = reg_dat$survey.area$SURVEY ), 
              fill = NA, 
              linewidth = 2, # size = 2,
              show.legend = TRUE) +
      stat_sf_coordinates(data = reg_dat$survey.grid,
                          mapping = aes(color = vess_col, 
                                        shape = vess_shape),
                          size = 2, 
                          show.legend = TRUE, 
                          na.rm = TRUE) + 
      scale_color_manual(
        name = " ", #"Survey Region",
        values = c(reg_dat$survey.area$color, 
                   unique(reg_dat$survey.grid$vess_col)),
        breaks = c(rev(unique(reg_dat$survey.area$SURVEY)), 
                   unique(reg_dat$survey.grid$vess_col)), 
        labels = c(rev(unique(stringr::str_to_title(reg_dat$survey.area$SRVY_long))), 
                   unique(reg_dat$survey.grid$vessel_name)), 
        na.value = "transparent")  +
      scale_shape_manual(
        name = " ", #"Survey Vessels",
        values = c(rep_len(x = "", length.out = length(unique(reg_dat$survey.area$SURVEY))),
                   unique(reg_dat$survey.grid$vess_shape)),
        breaks = c(unique(reg_dat$survey.area$SURVEY),
                   unique(reg_dat$survey.grid$vess_shape)),
        labels = c(rev(unique(stringr::str_to_title(reg_dat$survey.area$SRVY_long))), 
                   unique(reg_dat$survey.grid$vessel_name))) +
      ggplot2::guides(
        colour = guide_legend(
          # order = 1,# survey regions
          override.aes = list(
            fill = NA,
            linetype = c(
              rep_len(x = 1, 
                      length.out = length(unique(reg_dat$survey.area$SRVY))), 
              rep_len(x = 0, 
                      length.out = length(unique(reg_dat$survey.grid$vessel_id)))), 
            size = 6)),
        fill = guide_legend(
          # order = 2,# survey regions
          override.aes = list(
            color = "black", 
            linetype = 1, 
            shape = NA, 
            size = .5)))
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
  
  if (place_labels) {
    figure <- figure +
      ggplot2::geom_text(data = subset(reg_dat$place.labels, 
                              type == "mainland"), 
                aes(x = x, y = y, label = lab), 
                size = 7, group = 99) + 
      shadowtext::geom_shadowtext(data = subset(reg_dat$place.labels, 
                                    type == "peninsula"), 
                      aes(x = x, y = y, label = lab), size = 4, angle = 30, 
                      bg.color = "white", color = "black", group = 99) + 
      shadowtext::geom_shadowtext(
        data = subset(reg_dat$place.labels, 
                      type %in% c("bathymetry", "islands")),
        aes(x = x, y = y, label = lab), 
        bg.color = "white", color = "black", 
        size = 3, group = 99)
  }
  
  return(figure)
}

plot_coldpool_area <- function(coldpool_ebs_bin_area, maxyr, minyr = 1982) {
  
  # maxyr <- 2021
  # 
  # > coldpool_ebs_bin_area
  # year variable  value area_km2  label   proportion        perc
  # 1   1982   ≤ -1°C    425      425 ≤ -1°C 0.0008611844  0.08611844
  # 2   1983   ≤ -1°C    750      750 ≤ -1°C 0.0015197372  0.15197372
  # 3   1984   ≤ -1°C  17650    17650 ≤ -1°C 0.0357644815  3.57644815
  
  table_raw <- coldpool_ebs_bin_area#[!(coldpool_ebs_bin_area$year %in% 1990:1992),]
  
  yr_missing <- setdiff((min(table_raw$year):max(table_raw$year)), unique(table_raw$year))
  
  if (length(yr_missing)>0) {
    temp <- unique(table_raw[,c("variable", "label")])
    
    temp1 <- data.frame(matrix(data = NaN, nrow = nrow(temp)*length(yr_missing), ncol = ncol(table_raw)))
    names(temp1) <- names(table_raw)
    temp1$variable <- temp$variable
    temp1$label <- temp$label
    temp1 <- dplyr::arrange(temp1, label)
    temp1$year <- yr_missing
    
    table_raw <- dplyr::bind_rows(temp1, table_raw)
  }
  
  table_raw$ymax <-table_raw$proportion
  table_raw$ymin <- 0
  zl <- rev(levels(table_raw$variable))
  
  for (i in 2:length(zl)) {
    zi <- table_raw$variable==zl[i]
    zi_1 <- table_raw$variable==zl[i-1]
    table_raw$ymin[zi] <- table_raw$ymax[zi_1]
    table_raw$ymax[zi] <- table_raw$ymin[zi] + table_raw$ymax[zi]
  }
  
  if (is.na(table_raw$proportion[table_raw$year == (maxyr-1)][1])) {
    table_raw <- dplyr::bind_rows( 
      table_raw[table_raw$year == (maxyr),] %>% 
        dplyr::mutate(year = maxyr+.5), 
      table_raw)
  }
  
  # table_raw1 <- table_raw %>% 
  #   dplyr::arrange(desc(variable)) %>%
  #   dplyr::mutate(
  #        variable =  factor(x = as.character(variable), 
  #        levels = unique(as.character(variable)), 
  #        labels = unique(as.character(variable)), ordered = TRUE))
  
  
  # ggplot(table_raw, aes(x=x,ymax=ymax,ymin=ymin, fill=z)) + geom_ribbon()
  
  figure <- ggplot2::ggplot(data = table_raw, 
                            mapping = aes(x = year,
                                          ymax = ymax, 
                                          ymin = ymin, 
                                          fill = variable)) + 
    geom_ribbon() + 
    scale_fill_manual(name = "Temperature", 
                      values = viridis::mako(4, direction = -1, begin = 0.2, end = .8)) +  
    scale_y_continuous(name = "Proportion of EBS Shelf Survey Area",
                       limits = c(0, 1),
                       expand = c(0, 0),
                       breaks = seq(0,1,0.1)) +
    scale_x_continuous(name = "Year", 
                       limits = c(min(table_raw$year)-0.5,
                                  maxyr + ifelse((is.na(table_raw$proportion[table_raw$year == (maxyr-1)][1])), 1, .5)), 
                       expand = c(0, 0),
                       breaks = c(seq(1980, maxyr, 5))) +
    guides(fill = guide_legend(label.position = "right")) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_rect(fill = NA, colour = "grey20"), 
      panel.grid.minor = element_blank(),
      strip.background = element_blank(), 
      strip.text = element_text(size = 12, face = "bold"), 
      legend.text = element_text(size = 10),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.position = c(0.1, 0.8),
      legend.title = element_blank(),
      legend.box = "vertical")
  
  return(figure)
}

plot_mean_temperatures <- function(maxyr, SRVY){
  
  # EBS
  sebs_temperatures <- coldpool:::cold_pool_index %>%
    dplyr::filter(YEAR <= maxyr) %>%
    dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::rename(Bottom = MEAN_GEAR_TEMPERATURE, 
                  Surface = MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::mutate(region = "Eastern Bering Sea") %>% 
    tidyr::pivot_longer(cols = c("Bottom", "Surface"), 
                        names_to = "variable", 
                        values_to = "value")
  
  temp0 <- setdiff(min(sebs_temperatures$YEAR):max(sebs_temperatures$YEAR), unique(sebs_temperatures$YEAR))
  temp <- data.frame(matrix(data = NA, 
                            ncol = ncol(sebs_temperatures), 
                            nrow = length(temp0)*2 )) 
  names(temp) <- names(sebs_temperatures)
  temp <- temp %>% 
    dplyr::mutate(YEAR = c(temp0, temp0), 
                  region = "Eastern Bering Sea", 
                  variable = c(rep_len(x = "Bottom", length.out = length(temp0)), 
                               rep_len(x = "Surface", length.out = length(temp0))))
  
  sebs_temperatures <- dplyr::bind_rows(sebs_temperatures, temp)
  
  # NBS
  nbs_temperatures <- coldpool:::nbs_mean_temperature %>%
    dplyr::filter(YEAR <= maxyr) %>%
    dplyr::select(YEAR, MEAN_GEAR_TEMPERATURE, MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::rename(Bottom = MEAN_GEAR_TEMPERATURE, 
                  Surface = MEAN_SURFACE_TEMPERATURE) %>%
    dplyr::mutate(region = "Northern Bering Sea") %>% 
    tidyr::pivot_longer(cols = c("Bottom", "Surface"), 
                        names_to = "variable", 
                        values_to = "value")  
  
  temp0 <- setdiff(min(nbs_temperatures$YEAR):max(nbs_temperatures$YEAR), unique(nbs_temperatures$YEAR))
  temp <- data.frame(matrix(data = NA, 
                            ncol = ncol(nbs_temperatures), 
                            nrow = length(temp0)*2 )) 
  names(temp) <- names(nbs_temperatures)
  temp <- temp %>% 
    dplyr::mutate(YEAR = c(temp0, temp0), 
                  region = "Northern Bering Sea", 
                  variable = c(rep_len(x = "Bottom", length.out = length(temp0)), 
                               rep_len(x = "Surface", length.out = length(temp0))))
  
  nbs_temperatures <- dplyr::bind_rows(nbs_temperatures, temp)
  
  # mean temp
  mean_temp <- data.frame(region = c(rep("Eastern Bering Sea", 2),
                                     rep("Northern Bering Sea", 2)),
                          variable = rep(c("Bottom", "Surface"), 2),
                          value = c(mean(sebs_temperatures$value[sebs_temperatures$variable == "Bottom"], 
                                         na.rm = TRUE),
                                    mean(sebs_temperatures$value[sebs_temperatures$variable == "Surface"],
                                         na.rm = TRUE),
                                    mean(nbs_temperatures$value[nbs_temperatures$variable == "Bottom"], 
                                         na.rm = TRUE),
                                    mean(nbs_temperatures$value[nbs_temperatures$variable == "Surface"], 
                                         na.rm = TRUE)))
  
  if (SRVY == "NEBS") {
    all_temperatures <- dplyr::bind_rows(sebs_temperatures,
                                         nbs_temperatures)
  } else if (SRVY == "EBS") {
    all_temperatures <- dplyr::bind_rows(sebs_temperatures)
    mean_temp <- mean_temp[mean_temp$region == "Eastern Bering Sea",]
  } else if (SRVY == "NBS") {
    all_temperatures <- dplyr::bind_rows(nbs_temperatures)
    mean_temp <- mean_temp[mean_temp$region == "Northern Bering Sea",]
  }
  
  all_temperatures$variable <- factor(all_temperatures$variable, 
                                      levels=c('Surface','Bottom'))
  
  col <- viridis::mako(2, direction = -1, begin = .3, end = .5)
  color_sst <- col[2] 
  color_bt <- col[1] 
  
  figure <- ggplot(data = all_temperatures,
                   mapping = aes(x = YEAR,
                                 y = value,
                                 color = variable,
                                 shape = variable, 
                                 group = paste0(variable))) +
    facet_wrap(~region, scales = "free") +
    geom_point(size = rel(2)) +
    geom_line() +
    geom_hline(data = mean_temp,
               aes(yintercept = value,
                   color = variable),
               linetype = 2) +
    scale_color_manual(values = c(color_bt, color_sst)) +
    scale_y_continuous(name = "Average temperature (°C)", #expression(bold("Average temperature "~(degree*C))), 
                       limits = c(0,11.2),
                       breaks = seq(0,12,2),
                       expand = c(0,0))  + 
    scale_x_continuous(breaks = scales::pretty_breaks(), 
                       name = "Year") +
    guides(
      fill = guide_legend(title.position = "top", 
                          label.position = "bottom",
                          title.hjust = 0.25,
                          nrow = 1)) +
    theme_bw() +
    theme(
      panel.background = element_rect(fill = "white", 
                                      colour = NA), 
      panel.border = element_rect(fill = NA, 
                                  colour = "grey20"), 
      panel.grid.minor = element_blank(),
      strip.background = element_blank(), 
      strip.text = element_text(size = 12, face = "bold"), 
      legend.title = element_blank(), #, vjust = .5, hjust = .3),
      legend.text = element_text(size = 10),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent", 
                                fill = "transparent"),
      legend.position = "bottom",
      legend.box = "horizontal")
  
  return(list("figure" = figure, 
              "table_raw" = all_temperatures))
  
}


# Tables -----------------------------------------------------------------------

# Adapted from flextable::theme_vanilla()

#' @importFrom officer fp_border fp_par
#' @export
#' @title Apply vanilla theme
#' @description Apply theme vanilla to a flextable:
#' The external horizontal lines of the different parts of
#' the table (body, header, footer) are black 2 points thick,
#' the external horizontal lines of the different parts
#' are black 0.5 point thick. Header text is bold,
#' text columns are left aligned, other columns are
#' right aligned.
#' @param x a flextable object
#' @param pgwidth a numeric. The width in inches the table should be. Default = 6, which is ideal for A4 (8.5x11 in) portrait paper.
#' @param row_lines T/F. If True, draws a line between each row.
#' @param font0 String. Default = "Times New Roman". Instead, you may want "Arial".
#' @param body_size Numeric. default = 11.
#' @param header_size Numeric. default = 11.
#' @param spacing table spacing. default = 0.8.
#' @param pad padding around each element. default = 0.1
#' @family functions related to themes
#' @examples
#' ft <- flextable::flextable(head(airquality))
#' ft <- theme_flextable_nmfstm(ft)
#' ft
#' @section Illustrations:
#'
#' \if{html}{\figure{fig_theme_vanilla_1.png}{options: width=60\%}}
theme_flextable_nmfstm <- function(x,
                                   pgwidth = 6.5,
                                   row_lines = TRUE,
                                   body_size = 11,
                                   header_size = 11,
                                   font0 = "Arial Narrow",
                                   spacing = 0.6,
                                   pad = 5) {
  
  if (!inherits(x, "flextable")) {
    stop("theme_flextable_nmfstm supports only flextable objects.")
  }
  
  FitFlextableToPage <- function(x, pgwidth = 6){
    # https://stackoverflow.com/questions/57175351/flextable-autofit-in-a-rmarkdown-to-word-doc-causes-table-to-go-outside-page-mar
    ft_out <- x %>% flextable::autofit()
    
    ft_out <- flextable::width(ft_out, width = dim(ft_out)$widths*pgwidth /(flextable::flextable_dim(ft_out)$widths))
    return(ft_out)
  }
  
  std_b <- officer::fp_border(width = 2, color = "grey10")
  thin_b <- officer::fp_border(width = 0.5, color = "grey10")
  
  x <- flextable::border_remove(x)
  
  if (row_lines == TRUE) {
    x <- flextable::hline(x = x, border = thin_b, part = "body")
  }
  x <- flextable::hline_top(x = x, border = std_b, part = "header")
  x <- flextable::hline_bottom(x = x, border = std_b, part = "header")
  x <- flextable::hline_bottom(x = x, border = std_b, part = "body")
  x <- flextable::bold(x = x, bold = TRUE, part = "header")
  x <- flextable::align_text_col(x = x, align = "left", header = TRUE)
  x <- flextable::align_nottext_col(x = x, align = "right", header = TRUE)
  x <- flextable::padding(x = x, padding.left = pad, padding.right = pad, 
                          padding.top = 0, padding.bottom = 0, 
                          part = "all") # remove all line spacing in a flextable
  x <- flextable::font(x = x, fontname = font0, part = "all")
  x <- flextable::fontsize(x = x, size = body_size-2, part = "footer")
  x <- flextable::fontsize(x = x, size = body_size, part = "body")
  x <- flextable::fontsize(x = x, size = header_size, part = "header")
  # x <- flextable::fit_to_width(x = x,
  #                         max_width = pgwidth,
  #                         unit = "in")
  x <- FitFlextableToPage(x = x, pgwidth = pgwidth)
  # x <- flextable::line_spacing(x = x, space = spacing, part = "all")
  
  x <- flextable::fix_border_issues(x = x)
  x <- flextable::valign(x = x, valign = "top", part = "header")
  
  return(x)
}

# Save Tables and Figures ------------------------------------------------------

save_figures<-function(figure,
                       header = "",
                       footnotes = "",
                       filename0 = "x",
                       path = "./",
                       width = 6,
                       height = 6,
                       output_type = c("pdf", "png"),
                       type = "Figure",
                       alttext = "",
                       filename_desc = "",
                       nickname = "",
                       raw = NULL, 
                       bg = "white"
){
  
  header<-trimws(header)
  header<-paste0(ifelse(substr(x = header,
                               start = nchar(header),
                               stop = nchar(header)) %in%
                          c(".", "!", "?", "...", "...."),
                        header, paste0(header, ".")))
  footnotes<-trimws(footnotes)
  caption<-ifelse(sum(footnotes %in% "") != 0,
                  header,
                  paste0(header, paste(paste0("^[", footnotes, "]"),
                                       collapse = " ^,^ ")))
  
  # Save
  if (!is.null(path)){
    
    # Save Graphic/Figure
    for (i in 1:length(output_type)){
      ggplot2::ggsave( # save your plot
        path = path,
        dpi = 1200,
        bg = bg,
        filename = paste0(nickname, ".", output_type[i]), # Always save in pdf so you can make last minute edits in adobe acrobat!
        plot = figure, # call the plot you are saving
        width = width, height = height, units = "in") #recall, A4 pages are 8.5 x 11 in - 1 in margins
      
    }
    
    # raw
    
    # Save raw file (no rounding, no dividing)
    if (!(is.null(raw)) &
        (is.data.frame(raw) | is.matrix(raw))) {
      # for (i in 1:length(output_type)){
      utils::write.table(x = raw,
                         file = paste0(path, nickname,
                                       ".csv"),
                         sep = ",",
                         row.names=FALSE, col.names = TRUE, append = F)
      # }
    } else {
      raw <- ""
    }
    
  }
  
  write.table(x = caption, 
              file = paste0(path, nickname, ".txt"), 
              row.names = FALSE, 
              col.names = FALSE, 
              quote = FALSE)
  
  # Save Graphic/Figure as .rdata
  obj <- list("figure" = figure,
              "raw" = raw,
              "caption" = caption,
              "header" = header,
              "nickname" = nickname,
              "alttext" = alttext,
              "footnotes" = footnotes,
              "filename" = nickname)
  
  save(obj, 
       file = paste0(path, nickname, ".rdata"))
  
}


#' Systematically save your report tables for your report
#'
#' @param table_raw Optional. The data.frame that has no rounding and no dividing of numbers (good to save this for record keeping). Default = NA.
#' @param table_print The data.frame as table will be seen in the report.
#' @param list_tables Save tables in a list
#' @param header The name and title of the figure. Default = "".
#' @param footnotes Any footnote you want attached to this figure.
#' @param filename0 The filename set at the begining of the chapter
#' @param cnt The figure number
#' @param path The path the file needs to be saved to. Default = "NULL", meaning it wont save anything and will override all other saving elements.
#' @param output_type Default = c("csv"). Can be anything supported by utils::write.table.
#' @param type Default = "Table", but can be anything that the element needs to be called (e.g., "Graphic", "Fig.", "Graph") to fit in the phrase "Table 1. This is my spreadsheet!".
#' @param alttext String with what the alternative text is.
#' @param filename_desc Additional description text for the filename that will be added at the name of file before the filename extention, before the "_raw" or "_print". Default = "". Can be use to add a species name, location, or anything else that would make it easier to know what that file shows.
#' @param nickname A unique name that can be used to identify the figure so it can be referenced later in the report.
#' @param message TRUE/FALSE. Default = FALSE. If TRUE, it will print information about where your plot has been saved to.
#' @importFrom magrittr %>%
#' @export
#' @examples
#' # Select data and make plot
#' table_raw<-data.frame(x = rnorm(n = 10),
#'                       y = rnorm(n = 10),
#'                       col = rep_len(x = c("a", "b"), length.out = 5))
#' table_print <- table_raw
#' table_print[,c("x", "y")] <- mod_number(table_print[,c("x", "y")],
#'                                                      divideby = 1,
#'                                                      comma_seperator = TRUE,
#'                                                      digits = 2)
#' save_tables(table_raw = table_raw,
#'            table_print=table_print,
#'            header = "Here is a table!",
#'            footnote = "A footnote for this table!")
save_tables<-function(table_raw = NULL,
                      table_print = NULL,
                      header = "",
                      footnotes = "",
                      filename0 = "x",
                      path = NULL,
                      output_type = c("csv"),
                      alttext = "",
                      filename_desc = "",
                      nickname = "") {
  
  # Title
  header<-trimws(header)
  header<-paste0(ifelse(substr(x = header,
                               start = nchar(header),
                               stop = nchar(header)) %in%
                          c(".", "!", "?", "...", "...."),
                        header, paste0(header, ".")))
  footnotes<-trimws(footnotes)
  caption<-ifelse(sum(footnotes %in% "") != 0,
                  header,
                  paste0(header, paste(paste0("^[", footnotes, "]"),
                                       collapse = " ^,^ ")))
  # Save
  # if (!is.null(path)){
  
  # raw
  
  # Save raw file (no rounding, no dividing)
  if (!(is.null(table_raw))) {
    for (i in 1:length(output_type)){
      utils::write.table(x = table_raw,
                         file = paste0(path, nickname,
                                       ".", output_type[i]),
                         sep = ",",
                         row.names=FALSE, col.names = TRUE, append = F)
    }
    # } else {
    #   table_raw <- ""
    # }
  }
  # write.table can only save files that are 1) extant or 2) in a data.frame or matrix
  #   if (!(is.null(table_print))) {
  #     if ((class(table_print) %in% c("data.frame", "matrix"))) {
  #       for (i in 1:length(output_type)){
  #         utils::write.table(x = table_print,
  #                            file = paste0(path, nickname,
  #                                          "-print.", output_type[i]),
  #                            sep = ",",
  #                            row.names=FALSE, col.names = F, append = F)
  #       }
  #     } else { # save non-matrix or data.frames
  #       save(table_print,
  #            file = paste0(path, nickname, "-print.Rdata"))
  #     }
  #   } else {
  #     table_print <- ""
  #   }
  # }
  
  write.table(x = caption, 
              file = paste0(path, nickname, ".txt"), 
              row.names = FALSE, 
              col.names = FALSE, 
              quote = FALSE)
  
  # Save flextable etc as .rdata
  obj <- list("raw" = table_raw,
              "print" = table_print,
              "caption" = caption,
              "header" = header,
              "nickname" = nickname,
              "alttext" = alttext,
              "footnotes" = footnotes,
              "filename" = nickname)
  
  save(obj, 
       file = paste0(path, nickname, ".rdata"))
  
}




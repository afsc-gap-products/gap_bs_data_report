
font0 <- "Arial Narrow"
font_size0 <- 12

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
if (dir.exists(dir_out_figtab) == FALSE) {
  dir.create(dir_out_figtab)
}

dirs <- c("rawdata")#, "chapters", "documentation", "code", "figtab", "cite", "ref")
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
  # "magrittr",
  "readr",
  "tidyr",
  "readxl", 
  "here",
  "viridis",
  "janitor",
  "ggplot2", 
  
  # Text Management
  "stringr",
  "readtext",
  
  # RACE-GAP Specific
  "akgfmaps", # devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
  "coldpool", # devtools::install_github("afsc-gap-products/coldpool")
  "crabpack", # 
  
  # Spatial mapping
  "sf",
  "rlist",
  "shadowtext",
  "ggspatial", 
  "digest", 
  "ps", 
  "magrittr", 
  "raster",
  "stars",

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
  
  # calculate percent change:
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
      if (is.infinite(p)) {
        txt<-paste0(" was zero")
        p<-paste0("")
      } else if (x<0) {
        txt<-paste0(" decrease",ending)
        p<-paste0("a ", formatC(x = abs(p), format = "f", big.mark = ",", digits = 0), "%")
      } else if (x>0) {
        txt<-paste0(" increase",ending)
        p<-paste0("a ", formatC(x = abs(p), format = "f", big.mark = ",", digits = 0),"%")
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
      tens <- c("twenty-", "thirty-", "forty-", "fifty-", "sixty-", "seventy-", "eighty-",
                "ninety-")
      names(tens) <- 2:9
      x <- round(x)
      suffixes <- c("thousand", "million", "billion", "trillion")
      if (length(x) > 1) return(trim(sapply(x, helper)))
      
      
      out <- c(out, gsub(pattern = "- ", replacement = "-", x = helper(x)))
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

# LowerFirstPhrase <- function(x) {
#   substr(x, 1, 1) <- tolower(substr(x, 1, 1))
#   return(x)
# }

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
  out <- out |> 
    dplyr::filter(
      !(grepl(pattern = " egg", 
              x = unlist(out[,col]),
              ignore.case = TRUE)))
  
  # 2. find the codes we do want
  if (!is.null(str)) {
    
    str <- str[!is.na(str)]
    str <- unique(str)
    
    for (i in 1:length(str)){
      
      out <- out |> 
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
      out <- out  |>
        dplyr::filter(!(grepl(pattern = str_not[i], 
                              x = unlist(out[,col_str_not]),
                              ignore.case = TRUE))) 
    }
  }
  
  # clean codes
  out <- out  |>
    dplyr::select(all_of(col_out)) |> 
    unique() |> 
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
  
  temp <- report_spp |> 
    dplyr::select(-questions) |> 
    dplyr::arrange((order))
  
  if (!lang) {
    temp <- temp |>
      dplyr::select(-dplyr::starts_with("lang_"))
  }
  
  if (report_spp_col == "order") {
    
    report_spp_col <- "order1"
    temp$order1<-NA
    temp$order1[!is.na(temp$order)] <- TRUE
    
  }
  
  temp <- temp |> 
    dplyr::rename(col = all_of(report_spp_col)) |> 
    dplyr::filter(col == TRUE & 
                    !is.na(species_code)) |> 
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
    dplyr::left_join(x = temp1 |> 
                       dplyr::select(-col), 
                     y = spp_info |> 
                       dplyr::select(species_code, 
                                     genus, species) |> 
                       unique(), 
                     by = "species_code")  |> 
    dplyr::mutate(taxon = dplyr::case_when(
      species_code <= 31550 ~ "fish", 
      species_code >= 40001 ~ "invert")) |> 
    dplyr::mutate(temp = trimws(gsub(pattern = "NA", 
                                     replacement = "", 
                                     paste0(genus, " ", species)))) |> 
    dplyr::mutate(temp = ifelse(temp == " ", "", temp)) |>
    dplyr::mutate(species_name = dplyr::case_when(
      group_sci == "BLANK" ~ "",
      !is.na(group_sci) ~ group_sci, 
      is.na(group_sci) ~ temp, 
      TRUE ~ "other"
    )) |>
    dplyr::mutate(temp = trimws(paste0(group_sci, " (", print_name, ")"))) |>
    dplyr::mutate(temp = ifelse(temp == "NA ", "", temp)) |>
    dplyr::mutate(group = dplyr::case_when(
      is.na(group_sci) ~ temp, 
      TRUE ~ "other"
    )) |>
    # dplyr::select(file_name, print_name, species_name, 
    #   # report_name_scientific, 
    #   taxon, group, species_code, 
    #   scientific_name_prev, 
    #   dplyr::starts_with("lang_"), dplyr::starts_with("plot_")) |>
    # dplyr::filter(!grepl(pattern = "other ", x = group) & 
    #                 !grepl(pattern = "egg ", x = group)) |> 
    dplyr::distinct() |> 
    dplyr::mutate(type = ifelse(
      grepl(pattern = " ", x = species_name, fixed = TRUE),
      # species_name == paste0(genus, " ", species),
      "ital", NA)) |>
    dplyr::ungroup() |> 
    dplyr::mutate(species_name0 = species_name, 
                  species_name1 = species_name, 
                  species_name0 = dplyr::if_else(is.na(type == "ital"), species_name0, paste0("*", species_name0, "*")), 
                  species_name0 = gsub(pattern = " spp.*", replacement = "* spp.", x = species_name0, fixed = TRUE), 
                  species_name0 = gsub(pattern = " sp.*", replacement = "* sp.", x = species_name0, fixed = TRUE), 
                  species_name = species_name0) |> 
    dplyr::select(-type, -temp, -species_name0, -genus, -species)
  
  return(temp0)
}

# Plotting ---------------------------------------------------------------------

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
    dat0 <- dat |>
      dplyr::rename(year = as.character(year), 
                    lat = as.character(lat), 
                    lon = as.character(lon)) |> 
      dplyr::select(year, lat, lon, cpue_kgha) |> 
      dplyr::mutate(year = as.numeric(year), 
                    latdd = as.numeric(lat), 
                    londd = as.numeric(lon), 
                    cpue_kgha = as.numeric(cpue_kgha))
    d <- dat0[,c("londd", "latdd", "year", "cpue_kgha")]
  } else {
    dat0 <- dat |>
      dplyr::rename(year = as.character(year), 
                    lat = as.character(lat), 
                    lon = as.character(lon)) |> 
      dplyr::select(year, lat, lon) |> 
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
            fill = "grey90")  +  
    ggplot2::geom_sf(data = reg_dat$graticule,
                     color = "grey80",
                     alpha = 0.2)  +
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
  #   breaks = reg_dat$survey.areasrvy_long,
  #   labels = reg_dat$survey.area$srvy) 
  ggplot2::geom_sf(
    data = reg_dat$survey.area, 
    mapping = aes(color = srvy_long, 
                  geometry = geometry), 
    fill = "transparent", 
    # shape = NA, 
    linewidth = ifelse(row0 > 2, 1.5, 1), # size
    show.legend = legend_srvy_reg) +
    ggplot2::scale_color_manual(
      name = " ", 
      values = reg_dat$survey.area$color,
      breaks = reg_dat$survey.area$srvy_long,
      labels = reg_dat$survey.area$srvy) 
  
  # figure <- ggplot() +
  #   geom_sf(data = reg_dat$akland,
  #           color = NA,
  #           fill = "grey90") #+
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
        name = paste0(key.title, "CPUE (kg/ha)"), 
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
  #   geom_sf(data = reg_dat$survey.area, # |> 
  #           # dplyr::filter(srvy %in% srvy1), 
  #           mapping = aes(color = SURVEY), 
  #           fill = NA, 
  #           shape = NA, 
  #           size = ifelse(row0 > 2, 0.25, 0.75),
  #           show.legend = TRUE) +
  #   scale_color_manual(
  #     name = "", # key.title,
  #     values = reg_dat$survey.area$color,
  #     breaks = rev(reg_dat$survey.areasrvy_long), 
  #     labels = rev((reg_dat$survey.area$srvy)))
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
  #   if (unique(dat$srvy) %in% "EBS") {
  #     cp <- coldpool::ebs_bottom_temperature
  #   } else if (unique(dat$srvy) %in% "NBS") {
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
  #     ggplot2::geom_tile(data = bt_year_df |>
  #                          dplyr::filter(temperature <= temp_break), #|> 
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
    
    if (sum(dat$srvy %in% "EBS")>0) {
      cp <- coldpool::ebs_bottom_temperature
    } else if (unique(dat$srvy) %in% "NBS") {
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
                       pp |> 
                         sp::geometry(obj = .) |> 
                         sf::st_as_sf(x = .) |> 
                         dplyr::mutate(new_dim  = yrs[i]))
      
    }
    
    figure <- figure +
      geom_sf(data = outline |>
                sf::st_cast(to = "MULTIPOLYGON"), 
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
      panel.background = element_rect(fill = "white", colour = NA), 
      panel.border = element_rect(fill = NA, colour = "grey20"), 
      axis.text = element_text(size = ifelse(length(yrs)>4 & row0 == 1, 6, 8)),
      strip.background = element_blank(), 
      strip.text = element_text(size = 10, face = "bold"), 
      legend.text = element_text(size = 9),
      legend.background = element_rect(colour = "transparent", fill = "transparent"),
      legend.key = element_rect(colour = "transparent", fill = "transparent"),
      legend.position = "bottom",
      legend.box = "horizontal")
  
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
        mask_layer <- report_types$NEBS$reg_dat$survey.area
      } else {
        sel_raster_layer <- raster_ebs[[which(names(raster_ebs) == yrs[ii])]]
        mask_layer <- dplyr::filter(report_types$NEBS$reg_dat$survey.area, srvy == "EBS")
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
        rmapshaper::ms_simplify(keep_shapes = TRUE, keep = 0.04)
      
      temp_sf$year <- yrs[ii]
      
      if(ii == 1) {
        temperature_sf <- temp_sf
      } else {
        temperature_sf <- dplyr::bind_rows(temperature_sf, temp_sf)
      }
    }
  }
  
  if (2020 %in% yrs) {
    temperature_sf <- dplyr::bind_rows(temperature_sf |> 
                                         head(n = 1) |> 
                                         dplyr::mutate(bt = NA, year = 2020) , 
                                       temperature_sf)
  }
  
  temperature_zscore1 <- coldpool::cold_pool_index |> 
    dplyr::rename(var = dplyr::all_of(ifelse(case == "bottom", "MEAN_GEAR_TEMPERATURE", "MEAN_SURFACE_TEMPERATURE")), 
                  year = YEAR) |>
    dplyr::select(year, var) |> 
    dplyr::filter(year <= maxyr) |>
    dplyr::arrange(var) |> 
    dplyr::mutate(
      sd = sd(var, na.rm = TRUE), 
      mean = mean(var, na.rm = TRUE), 
      zscore  = ((var-mean)/sd), # z = (x-μ)/σ
      sign = dplyr::case_when(
        zscore <= -1 ~ "-", 
        zscore >= 1 ~ "+"), 
      color = dplyr::case_when(
        sign == "+" ~ negative, 
        sign == "-" ~ positive)) |> 
    dplyr::arrange(year) |> 
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
                     fill = "grey90")+  
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
      axis.text = element_text(size = legend_font_size-4),
      strip.background = element_blank(),
      strip.text = element_text(size = legend_font_size, face = "bold"),
      legend.position = "none", 
      plot.margin=grid::unit(c(0,0,0,0), "mm") )
  
  if (temperature_zscore) {
    
    figure <- figure +
      ggplot2::geom_label(data = temperature_zscore1, 
                          aes(label = sign,
                              # x = quantile(x = reg_dat$plot.boundary$x, .96),
                              # y = quantile(x = reg_dat$plot.boundary$y, .96), 
                              color = sign ), 
                          x = Inf, 
                          y = Inf, 
                          hjust = 1, 
                          vjust = 1, 
                          fontface = "bold", 
                          fill = "grey20",
                          label.size = NA, 
                          label.r = unit(0, "lines"), 
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
                      y = mean(colorbar_breaks[c(2:(length(colorbar_breaks)-1))]), # y = 3.5,
             x = 1.15,
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


#' plot_size_comp
#'
#' @param sizecomp0 data.frame with these columns: "year", "taxon", "srvy", "species_code", "sex", "pop", "length"   
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
                          legend_font_size = 8, 
                          divscale = TRUE){
  
  table_raw <- sizecomp0 |>
    dplyr::arrange(year, srvy, sex, length_mm) 
  
  # find appropriate units
  if (divscale) {
  a <- find_units(unit = "", unt = "", dat = max(table_raw$population_count, na.rm = TRUE))
  } else {
    a <- list("divby" = 1, 
              "unit_word" = "", 
              "unit_wrd" = "")
  }
  for (jjj in 1:length(a)) { assign(names(a)[jjj], a[[jjj]]) }
  pop_unit <- divby
  pop_unit_word <- unit_word
  
  if (is.null(unit0)){ # mm vs cm
    len_unit_word0 <- ifelse(!grepl(pattern = " crab", x = spp_print, ignore.case = TRUE),  "cm", "mm")
  } else {
    len_unit_word0 <- unit0
  }
  
  table_raw <- table_raw |>
    dplyr::mutate(population_count = population_count/pop_unit, 
                  length_mm = round(
                    x = length_mm/ifelse(len_unit_word0 == "mm", 1, 10), digits = 0)) |>
    dplyr::ungroup()
  
  len_unit_axis <- ifelse(max(table_raw$length_mm)-min(table_raw$length_mm)>150, 50, 
                          ifelse(max(table_raw$length_mm)-min(table_raw$length_mm)>45, 10, 5))
  
  dat_text <- lengths0 |> 
    dplyr::group_by(srvy, year) |> 
    dplyr::summarise(frequency = formatC(x = sum(frequency, na.rm = TRUE), 
                                         digits = 0, big.mark = ",", format = "f")) |> 
    dplyr::ungroup() |> 
    dplyr::filter(year %in% unique(as.numeric(paste(table_raw$year)))) 
  
  dat_text <- dat_text |> 
    dplyr::mutate(
      label = paste0(c("# measured: ", rep_len(x = "", length.out = (nrow(dat_text)-1))), 
                     frequency), 
      label = gsub("\\s", " ", formatC(x = label))) |> 
    dplyr::select(-frequency) |> 
    dplyr::ungroup()
  
  table_raw <- dplyr::left_join(table_raw, dat_text)
  
  if (!ridgeline) { # facet plot without ridgeline
    
    figure <- ggplot(data = table_raw,
                     mapping = aes(x = length_mm,
                                   y = population_count,
                                   fill = sex)) +
      ggplot2::geom_bar(position="stack", stat="identity", na.rm = TRUE) +
      ggplot2::scale_fill_viridis_d(direction = -1, 
                           option = "mako",
                           begin = .2,
                           end = .6,
                           na.value = "transparent", 
                           drop = FALSE) +
      ggplot2::scale_y_continuous(name = paste0("Population", pop_unit_word), 
                                  limits = c(0, max(table_raw$population_count)),
                                  labels = scales::label_comma(accuracy = 1)) +
      ggplot2::scale_x_continuous(name = stringr::str_to_sentence(paste0(type," (", len_unit_word0, ")")), 
                                  labels = scales::label_comma(accuracy = 1))  +
      ggplot2::labs(fill = spp_print) +
      ggplot2::guides(
        fill = guide_legend(title.position = "top",
                            title.hjust = 0.5,
                            title.vjust = -0.5)) +
      ggplot2::theme(
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(fill = NA,
                                    colour = "grey20"),
        legend.text = element_text(size = legend_font_size),
        legend.background = element_rect(colour = "transparent", 
                                         fill = "transparent"),
        legend.key = element_rect(colour = "transparent",
                                  fill = "transparent"),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.spacing = unit(0, "pt"))
    
    if (length(unique(table_raw$year)) > 1) {
      figure <- figure +
        ggplot2::facet_grid(year ~ srvy_long,
                            scales = "free_x")
    }
    
    # are there any plots with no data?
      figure <- figure + 
        # ggplot2::labs(caption = "+ None of this species was lengthed. ") +
        # ggplot2::theme(plot.caption = element_text(hjust=0, size = 10)) +
        ggplot2::geom_text(data = table(year = table_raw$year, srvy_long = table_raw$srvy_long) |> 
                             data.frame() |> 
                             dplyr::mutate(sex  = "Males", # had to pick something and this is the most generalizable across species
                                           sign = ifelse(Freq == 0, "* No specimens lengthed", NA), 
                                           srvy_long = stringr::str_to_title(srvy_long)), 
                            mapping = aes(label = sign), 
                            fontface = "italic", 
                           size = 2.5, 
                            color = "grey20",
                            x = Inf, 
                            y = -Inf, 
                            show.legend = FALSE,
                            hjust = 1.1,
                            vjust = -.5)
    
  } else {
    table_raw1 <- table_raw |> 
      dplyr::ungroup() |> 
      dplyr::group_by(year, length_mm, srvy_long) |> 
      dplyr::summarise(population_count = sum(population_count, na.rm = TRUE))
    
    temp <- setdiff(min(table_raw1$year, na.rm = TRUE):max(table_raw1$year, na.rm = TRUE), 
                    unique(table_raw1$year))
    if (length(temp)>0) {
      table_raw1 <- dplyr::bind_rows(
        data.frame(year = temp,
                   length_mm = 0, 
                   population_count = 0, 
                   srvy_long = unique(table_raw1$srvy_long)), 
        table_raw1)
    }
    
    table_raw1 <- table_raw1 |> 
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
      facet_wrap(vars(srvy_long))
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
      ggplot2::geom_text(mapping = aes(label = label), 
                                       x = Inf, 
                                       y = Inf, 
                                       hjust = 1.1, 
                                       vjust = 2,
                         check_overlap = TRUE) 
  }

  return(figure)
}


plot_survey_stations <- function(reg_dat, 
                                 survey_outline = TRUE, 
                                 stratum_grid = FALSE, 
                                 stratum_no = FALSE, 
                                 station_pts = "stn", # c("pts", "ves", "names")
                                 bathymetry = FALSE, 
                                 study = FALSE, 
                                 place_labels = TRUE) {
  
  figure <- ggplot()  +
    ggplot2::geom_sf(data = reg_dat$akland,
            color = NA,
            fill = "grey90") +
    ggplot2::geom_sf(data = reg_dat$graticule,
                     color = "grey80",
                     alpha = 0.2)  +
    ggplot2::scale_y_continuous(name = "Latitude", 
                                limits = reg_dat$plot.boundary$y,
                                breaks = reg_dat$lat.breaks) +
    ggplot2::scale_x_continuous(name = "Longitude", 
                                limits = reg_dat$plot.boundary$x,
                                breaks = reg_dat$lon.breaks) + 
    ggplot2::theme( # set legend position and vertical arrangement
      panel.background = element_rect(fill = "white", colour = "grey20"), 
      strip.background = element_rect(fill = "grey85",colour = "grey20"), 
      legend.spacing.y = unit(-0.35, "cm"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 7),
      legend.background=element_blank(),
      legend.key = element_rect(colour = "transparent", fill = "transparent"),
      legend.position = c(.15, .15),
      legend.box.just = "left",
      legend.box = "vertical" )
  
  if (study) {
    
    study <- reg_dat$survey.grid |> dplyr::filter(!is.na(study))
    
    figure <- figure  +
      geom_sf(data = reg_dat$survey.grid |> dplyr::filter(!is.na(study)),
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
                       aes(color = srvy_long), 
                       fill = NA, 
                       linewidth = 1.5, 
                       show.legend = TRUE) +
      ggplot2::scale_color_manual(
        name = "",
        values = reg_dat$survey.area$color,
        breaks = reg_dat$survey.area$srvy_long,
        labels = stringr::str_to_title(reg_dat$survey.area$srvy_long))
  }
  
  if (station_pts == "names") {
    figure <- figure +
      geom_sf(data = reg_dat$survey.grid, 
              color = "grey20", 
              fill = NA) +
      geom_sf_text(data = reg_dat$survey.grid, 
                   lineheight = 0.7,
                   mapping = aes(label = gsub(x = station, 
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
              aes(color = reg_dat$survey.area$srvy_long, 
                  shape = reg_dat$survey.area$srvy_long ), 
              fill = NA, 
              linewidth = 2, # size = 2,
              show.legend = TRUE) +
      ggplot2::stat_sf_coordinates(data = reg_dat$survey.grid,
                          mapping = aes(color = vess_col, 
                                        shape = vess_shape, 
                                        geometry = geometry),
                          size = 2, 
                          show.legend = TRUE, 
                          na.rm = TRUE) + 
      ggplot2::scale_color_manual(
        name = " ", #"Survey Region",
        values = c(reg_dat$survey.area$color, 
                   unique(reg_dat$survey.grid$vess_col)),
        breaks = c(rev(unique(reg_dat$survey.area$srvy_long)), 
                   unique(reg_dat$survey.grid$vess_col)), 
        labels = c(rev(unique(stringr::str_to_title(reg_dat$survey.area$srvy_long))), 
                   unique(reg_dat$survey.grid$vessel_name)), 
        na.value = "transparent")  +
      ggplot2::scale_shape_manual(
        name = " ", #"Survey Vessels",
        values = c(rep_len(x = "", length.out = length(unique(reg_dat$survey.area$srvy_long))),
                   unique(reg_dat$survey.grid$vess_shape)),
        breaks = c(unique(reg_dat$survey.area$srvy_long),
                   unique(reg_dat$survey.grid$vess_shape)),
        labels = c(rev(unique(stringr::str_to_title(reg_dat$survey.area$srvy_long))), 
                   unique(reg_dat$survey.grid$vessel_name))) +
      ggplot2::guides(
        colour = guide_legend(
          # order = 1,# survey regions
          override.aes = list(
            fill = NA,
            linetype = c(
              rep_len(x = 1, 
                      length.out = length(unique(reg_dat$survey.area$srvy))), 
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
                   mapping = aes(label = reg_dat$survey.strata$stratum),
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
    ft_out <- x |> flextable::autofit()
    
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
#' @importFrom magrittr |>
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




################COMMON FUNCTIONS TO BE USED IN ALL SECTIONS#########################

#####LIBRARY FUNCTIONS#####
#Seperating species by taxonomic group
# install.packages("remotes")
# remotes::install_github("ropensci/taxize")
library(taxize)

# Data Managment
library(here)
library(tidyr)
library(reshape2)
library(tidyverse)
library(filesstrings)
library(data.table) # := to tag species codes
require(plyr)  #ddply function
library(sas7bdat)

#List Management
library(rlist)
library(pipeR)

#RMarkdown
library(rmarkdown)
library(knitr)
library(gridExtra)

#Excel File Management
library(officer)
library(xlsx)

#Visuals
library(ggplot2)
library(dichromat)
library(readxl)

#Package Management
library(roxygen2)
library(devtools)

#Fonts
library(extrafont)
loadfonts()
# extrafont::font_import()
#windowsFonts()
# install.packages("jsonlite", repos="http://cran.r-project.org")

library(jsonlite) #https://www.opencpu.org/posts/jsonlite-a-smarter-json-encoder/

options(java.parameters = "-Xmx1000m")
options(scipen=10000)

ln<-log #tricky, tricky, Base R! Didn't fool me this time!!!


####COLOR PALLET#####
#mostly for figures
NOAALightBlue<-"#C9E1E6"
NOAADarkBlue<-"#0098A6"
NOAADarkGrey<-"#56575A" #text
NOAABlueScale<-colorRampPalette(colors = c(NOAALightBlue, NOAADarkBlue))

counter0<-"000"

#######FIND PARENT DIRECTORIES############
sectname0<-paste0("FEUS", maxyr, sectname)
date00<-paste0(Sys.Date())
dir.in<-getwd()
dir.parent<-dirname(dir.in)
dir.scripts<-paste0(dir.in, "/rscripts/")
# dir.create(dir.scripts)
dir.output<-paste0(dir.in, "/output/")
# dir.create(dir.output)
dir.data<-paste0(dir.in, "/data/")
dir.rscripts<-paste0(dir.in, "/rscripts/")
dir.data.common<-paste0(dir.common, "/data/")
dir.rscripts.common<-paste0(dir.common, "/rscripts/")

######CREATE DIRECTORIES#####
dir.out<-paste0(dir.output, sectname0, "_", date00, 
                ifelse(exists("NMFSorITIS"), paste0("_", NMFSorITIS), ""), 
                ifelse(designflowin == T, "_Design", ""), "/")
dir.create(dir.out)

dir.create(paste0(dir.out, "/metadata/")) #Save metadata
dir.create(paste0(dir.out, "/policy/")) #Save word files
dir.create(paste0(dir.out, "/chapters/")) #Save word files for policy subsection
dir.chapters<-paste0(dir.out, "/chapters/")
dir.create(paste0(dir.out, "/rawdata/")) #Save word files for policy subsection
dir.create(paste0(dir.out, "/rscripts/")) #scripts

dir.outputtables<-paste0(dir.out, "/outputtables/")
dir.create(dir.outputtables)
dir.create(paste0(dir.outputtables, "/webtool/")) #Save files for FEUS web tool
dir.create(paste0(dir.outputtables, "/nationaltables/")) #Save files for FEUS web tool
dir.nattables<-paste0(dir.outputtables, "/nationaltables/")

######SAVE WORKING FILES########

#Word styles file (from Common)
if (FEUS0 == T){
  file.copy(from = paste0(dir.parent, "/FEUS",maxyr,"Common/word-styles-reference.docx"), 
            to = paste0(dir.out, "/rscripts/word-styles-reference.docx"), 
            overwrite = T)
}
#From Specific Files
listfiles<-list.files(path = dir.scripts) 
listfiles0<-c(listfiles[grepl(pattern = "\\.r", 
                              x = listfiles, ignore.case = T)], 
              listfiles[grepl(pattern = "\\.docx", 
                              x = listfiles, ignore.case = T)])
listfiles0<-listfiles0[!(grepl(pattern = "~",ignore.case = T, x = listfiles0))]

if (FEUS0 == T){
  for (i in 1:length(listfiles0)){
    file.copy(from = paste0(dir.scripts, listfiles0[i]), 
              to = paste0(dir.out, "/rscripts/", listfiles0[i]), 
              overwrite = T)
  }
}
#From Common Files
listfiles0<-c(list.files(path = paste0(dir.parent, "/FEUS",maxyr,"Common/"), 
                         pattern = "\\.r", ignore.case = T)) 
listfiles0<-listfiles0[!(grepl(pattern = "\\.rproj", 
                               x = listfiles0, ignore.case = T))]

if (FEUS0 == T){
  for (i in 1:length(listfiles0)){
    file.copy(from = paste0(dir.parent, "/FEUS",maxyr,"Common/", listfiles0[i]), 
              to = paste0(dir.out, "/rscripts/", listfiles0[i]), 
              overwrite = T)
  }
} 

#Common workbook files
listfiles0<-list.files(path = paste0(dir.parent, "/FEUS",maxyr,"Common/"), pattern = ".csv") 

if (FEUS0 == T){
  for (i in 1:length(listfiles0)){
    file.copy(from = paste0(paste0(dir.parent, "/FEUS",maxyr,"Common/"), listfiles0[i]), 
              to = paste0(dir.out, "/rawdata/", listfiles0[i]), 
              overwrite = T)
  }
}



##***STATE INFO####
# General Info
# State <- c("Alabama", "Alaska", "California", "Connecticut",
#            "Delaware", "East Florida", "West Florida", "Georgia", "Hawai`i",
#            "Louisiana", "Maine", "Maryland", "Massachusetts",
#            "Mississippi", "New Hampshire", "New Jersey", "New York",
#            "North Carolina", "Oregon", "Rhode Island", "South Carolina",
#            "Texas",  "Virginia", "Washington", "Pacific At-Sea Processors") # states in order
# 
# State1<-State
# State1[grep(pattern = "Florida", x = State)]<-"Florida"
# State1[grep(pattern = "Hawai`i", x = State)]<-"Hawaii"
# State1[grep(pattern = "Pacific At-Sea Processors", x = State)]<-"At-Sea Process, Pac."
# 
# Region <- c("Gulf of Mexico", "North Pacific", "Pacific", "New England",
#             "Mid-Atlantic", "South Atlantic", "Gulf of Mexico", "South Atlantic",
#             "Western Pacific (Hawai`i)",
#             "Gulf of Mexico", "New England", "Mid-Atlantic", "New England",
#             "Gulf of Mexico", "New England", "Mid-Atlantic", "Mid-Atlantic",
#             "South Atlantic", "Pacific", "New England", "South Atlantic",
#             "Gulf of Mexico",  "Mid-Atlantic", "Pacific", "Pacific") # states in order
# 
# fips <- c(1 ,2 ,6 ,9 ,10, 12, 12, 13, 15, 22,23,24,25,28,33,34,36,37,41,44,45,48,51,53, 0)
# xstate<-c("1", "1", "1", "1", "1", "1", "2", "2", "1", "3", "2", "2", "3", "4",
#           "4", "3", "4", "3", "2", "5", "4", "5",  "5", "3", "4") #state chapter order
# xreg<-c("7", "1", "2", "4", "5", "6", "7", "6", "3", "7", "4", "5", "4", "7",
#         "4", "5", "5", "6", "2", "4", "6", "7",  "5", "2", "2") #region chapter order
# abbvst <- c("AL", "AK", "CA", "CT", "DE", "EFL", "WFL", "GA", "HI",
#           "LA", "ME", "MD", "MA", "MI", "NH", "NJ", "NY",
#           "NC", "OR", "RI", "SC", "TX",  "VA", "WA", "Proc") #
# abbvreg <- c("GOM", "NP", "Pac", "NE",
#             "MA", "SA", "GOM", "SA", "WP",
#             "GOM", "NE", "MA", "NE",
#             "GOM", "NE", "MA", "MA",
#             "SA", "Pac", "NE", "SA",
#             "GOM",  "MA", "Pac", "Pac") #
# 
# State.no <- c(1, 2, 5, 7, 8 ,10 ,11 ,13 ,14 ,21 ,22 ,23 ,24 ,27 ,32 ,33 ,35 ,36 ,40 ,42 ,43 ,46 ,49 ,50 ,98) # states in order
# 
# Region.no <- c(5, 7, 6, 1, 2, 4, 5, 4, 8, 5, 1, 2, 1, 5, 1, 2, 2, 4, 6, 1, 4, 5, 2, 6, 6) # states in order
# subreg<-NA
# 
# statereg <- data.frame(State, State1, fips, Region, abbvst, abbvreg, xstate, xreg, State.no, Region.no, subreg)
# write.csv(x = statereg, file = paste0(dir.data.common, "statereg.csv"))


#####KNOWNS#####
states.all.coastal<-c("Hawai`i", "Rhode Island", "Delaware", "West Florida", "East Florida")

yearvars <- as.character(minyr:maxyr)

state.fips<- data.frame(c("1" ,"2", "6", "9", "10", "12", "13", "15", "22","23","24","25", "28",  "33",  "34",
                          "36",  "37",  "41",  "44",  "45",  "48",  "51",  "53")  ,
                        c("Alabama", "Alaska","California", "Connecticut","Delaware", "Florida","Georgia","Hawaii","Louisiana",  "Maine","Maryland",  "Massachusetts","Mississippi","New Hampshire","New Jersey",
                          "New York","North Carolina",  "Oregon","Rhode Island",  "South Carolina","Texas","Virginia","Washington"),
                        stringsAsFactors=FALSE
)
names(state.fips)<- c("Fips","State")

Footnotes.list.Common<-list(
  #text
  "ft_Data" = "See data sources section for more information about where each region or state's data comes from." ,
  "ft_999" = "In this table, '<1' = 0-999 fish, and '1' = 1,000-1,499 fish.",
  "ft_na" = "'NA' = not available.", 
  "ft_FEUStool" = "Summary data is available online in the FEUS webtool. [Available at: https://www.fisheries.noaa.gov/data-tools/fisheries-economics-united-states-interactive-tool.]"
)

reg.order<-c("North Pacific", "Pacific", "Western Pacific (Hawai`i)", "New England", "Mid-Atlantic", "South Atlantic", "Gulf of Mexico") 
reg.order1<-c("Alaska", "Pacific", "Hawaii", "New England", "Mid-Atlantic", "South Atlantic", "Gulf of Mexico") 
reg.order0<-1:length(reg.order)

###FUNCTIONS####

###***Footnotes####

issomethinginthismatrix<-function(x, searchfor) {
  xx<-c()
  for (r in 1:nrow(x)) {
    if (is.na(searchfor)) {
      xx<-c(xx, sum(is.na(x[r,]), na.rm = T))
    } else {
      xx<-c(xx, sum(x[r,] == searchfor, na.rm = T))
    }
  }
  if (sum(xx)==0) {
    return(F)
  } else {
    return(T)
  }
}

addfootnotes<-function(df.dat, table0 = NA, state0 = NA, region0 = NA, species0 = NA, footnote0) {
  
  #find all of the places where this footnote should be placed
  if (!is.na(species0[1])) { #for HR tables
    idx<-df.dat$keyspecies %in% species0
  } else if (is.na(region0[1])) { 
    idx<-df.dat$Table %in% table0 & (df.dat$State %in% state0)
  } else if (is.na(state0[1])) {
    idx<-df.dat$Table %in% table0 & (df.dat$Region %in% region0)
  } else { 
    idx<-df.dat$Table %in% table0 & (df.dat$State %in% state0 & df.dat$Region %in% region0)
  }
  
  #If the cell is blank, overwrite with new data. Otherwise, append. 
  idx<-which(idx %in% T)
  for (i in 1:length(idx)){
    if (df.dat$Footnotes[idx[i]][1] %in% c("", NA)) {
      a<-as.character(footnote0)
    } else {
      a<-paste0(df.dat$Footnotes[idx[i]], " 123456789 ", footnote0)
    }
    df.dat$Footnotes[idx[i]]<-a
  }  
  return(data.frame(df.dat))
}

list2string.webtool<-function(x) {
  xx<-rep_len(x = '""', length.out = length(x))
  for (nro in 1:length(x)) {
    
    str0<-as.character(x[nro])
    # str0<-gsub(pattern = "\'", replacement = "\\'", x = str0, fixed = T)
    
    if (grepl(pattern = " 123456789 ", x = str0)) {
      # str0<-gsub(pattern = "'", replacement = "/'", x = x[nro], fixed = T)
      str0<-strsplit(x = str0, split = " 123456789 ")
      str0<-str0[[1]]
    } else { 
      str0<-str0#x[nro]
    }
    
    str0<-unique(str0)
    
    # str0<-gsub(pattern = "'", replacement = "\'", x = str0)
    
    xx[nro]<-ifelse(str0[1] %in% c("", "[]", "null"), 
                    'null', 
                    paste0('"', paste(str0, collapse = '|'), '"'))
    # paste0('["', paste(str0, collapse = '", "'), '"]'))
    # gsub(pattern = "'", replacement = '"', x = a)
  }
  return(as.character(xx))
}


list2string.ft<-function(x) {
  xx<-rep_len(x = "", length.out = length(x))
  for (nro in 1:length(x)) {
    str0<-x[nro]
    # str0<-gsub(pattern = "'", replacement = "/'", x = x[nro], fixed = T)
    
    if (grepl(pattern = " 123456789 ", x = str0)) {
      # str0<-gsub(pattern = "'", replacement = "/'", x = x[nro], fixed = T)
      str0<-strsplit(x = str0, split = " 123456789 ")
      str0<-str0[[1]]
    } else { 
      str0<-str0#x[nro]
    }
    
    str0<-unique(str0)
    str0[str0 %in% c("", "[]", "null")]<-""
    
    xx[nro]<-ifelse(str0[1] == "", "", 
                    paste0("^[", paste(str0, collapse = "] ^,^ ^["), "]"))
  }
  return(as.character(xx))
}


# my.file.rename <- function(from, to) {
#   todir <- dirname(to)
#   if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
#   file.rename(from = from,  to = to)
# }

#Make data sets above look like this .js file:
# https://www.st.nmfs.noaa.gov/data-and-tools/FEUS/datatablesStrings9.js

funct_df2js<-function(df.dat, minyr, maxyr) {
  
  df.dat$Footnotes<-as.character(df.dat$Footnotes)
  df.dat$Footnotes[df.dat$Footnotes %in% c("", "[]")]<-"null"
  
  # df.dat<-lapply(X = df.dat, FUN = as.character)
  for (col in 1:(ncol(df.dat)-1)){ #not footnotes
    df.dat[,col]<-as.character(df.dat[,col])
    for (row in 1:nrow(df.dat)){
      df.dat[row,col]<-trimws(df.dat[row,col])
      df.dat[row,col]<-gsub(pattern = "\\*", replacement = "", x = df.dat[row,col])
      df.dat[row,col]<-ifelse(is.na(df.dat[row,col]),  "NA", df.dat[row,col])
    }
  }
  df.dat<-rbind.data.frame(names(df.dat), df.dat)
  
  str0<-(toJSON(as.matrix(df.dat)))
  # str0<-gsub(pattern = "null", replacement = 'NA', str0)
  str0<-gsub(pattern = '"null"', replacement = 'null', str0)
  str0<-gsub(pattern = '""', replacement = '"', str0)
  
  str0<-gsub(pattern = '\\],\\[', replacement = '\\],
             \\[', x = str0)
  str0<-gsub(pattern = '\\]\\"', replacement = '\\]', x = str0)
  str0<-gsub(pattern = '\\"\\[', replacement = '\\[', x = str0)
  # str0<-gsub(pattern = "'", replacement = "/'", x = str0)
  # str0<-gsub(pattern = "/[/'", replacement = "/[`", x = str0)
  # str0<-gsub(pattern = "'/]", replacement = "/]`", x = str0)
  # str0<-gsub(pattern = '\\[\"', replacement = '\\["', x = str0)
  
  # str0<-gsub(pattern = '\\\"', replacement = '"', x = str0, fixed = T)
  str0<-gsub(pattern = '\\\"', replacement = '', x = str0, fixed = T)
  str0<-gsub(pattern = '\\.\\]', replacement = '\\.\\"\\]', x = str0)
  
  
  #make year numeric
  if (sum(names(df.dat)=="Year")==1) {
    for (i in 1:length(minyr:maxyr)) {
      str0<-gsub(pattern = paste0("'", as.character(minyr:maxyr)[i], "'"),
                 replacement = as.character(minyr:maxyr)[i], x = str0)
    }
  }
  
  
  return(str0)
}




###***Text Editing####

text_liststocks<-function(dat, metric, orgby = "pct", absvalue=F, pctvalue=T, 
                          preunit = "", postunit = "",
                          decreasingTF, yr, lv, section="nominal", capitalizefirst=T) {
  
  dat0<-dat
  dat<-data.frame(dat)
  names(dat) <- gsub(pattern = "X", replacement = "", x = names(dat))
  
  if (absvalue == TRUE) {
    dat<-dat[order(dat[,which(colnames(dat) %in% paste0(metric, ""))],decreasing=decreasingTF),]
  } else {
    dat<-dat[order(dat[which(colnames(dat) %in% paste0(metric, ".pct"))],decreasing=decreasingTF),]
  }
  
  absval<-dat[lv,which(colnames(dat) %in% paste0(metric, ""))]
  pctval<-100*dat[lv,which(colnames(dat) %in% paste0(metric, ".pct"))]
  org<-tolower2(str0 = dat$keyspecies[lv], 
                capitalizefirst = capitalizefirst)
  
  str0<-c()#org
  
  #Anything to note?
  
  #No
  if (ifelse(orgby == "pct", is.na(pctval), is.na(absval)) | 
      ifelse(orgby == "pct", 
             ifelse(decreasingTF==T, pctval<0, pctval>0), 
             ifelse(decreasingTF==T, absval<100000, absval>100000) )) { #a wrong value (i.e. NA or negative for increasing visa versa)
    if (lv == 1) { #first value doesn't have anything useful
      str0<-paste0(str0, 
                   paste0("There were no ", 
                          ifelse(orgby == "pct", "percent ", "monitary "), 
                          ifelse(decreasingTF==T, "increases", "decreases"),
                          ifelse(section %in% "nominal", " (in nominal dollar values)", ""),
                          "."))
    } else { #other values qualified and there are no more
      str0<-""
    }
    
    #Yes!
  } else {
    
    str0<-paste0(str0, org, " (")
    
    #PERCENT VALUE
    if (pctvalue == T) {
      #Nominal Values
      str0.pct<-xunitspct(pctval)
      
      #REAL TERMS
      if (metric %in% "YR10" & section %in% "real terms") { #if a correct value and realterms = T (realterms only applicable for money aka revenue or price)
        datcol.real<-dat[which(colnames(dat) %in% paste0(metric, ".infl.pct"))]
        val.real<-100*(datcol.real[lv,])
        
        str0.pct<-paste0(str0.pct,  
                         ", ", 
                         xunitspct(val.real),
                         " in real terms")
        
      }
      
    }
    
    #ABSOLUTE VALUE
    # str0<-ifelse(absvalue==F, paste0(str0, ")"), str0)
    if (absvalue==T) {
      # str0.abs<-ifelse((metric==paste0("YR10") & section %in% "absolute"), 
      #              paste0(str0, ","), str0)
      str0.abs<-paste0(ifelse(sign(absval) == 1, "", "-"), preunit, 
                       paste0(xunits(abs(absval)), collapse = ""), 
                       ifelse(postunit=="", "", paste0(" ", postunit)),
                       " in absolute change terms")
    }
    
    
    if (absvalue == T & pctvalue == T) {
      str0<-ifelse(orgby == "pct", 
                   paste0(str0, str0.pct, " and ", str0.abs,")"),   
                   paste0(str0, str0.abs, " and ", str0.pct,")") ) 
      
    } else if (absvalue == T & pctvalue == F) {
      str0<-(paste0(str0, str0.abs,")"))  
    } else if (absvalue == F & pctvalue == T) {
      str0<-(paste0(str0, str0.pct,")"))  
    }
  }
  
  return(str0)
}

text_increasingdecreasing<-function(minyr, maxyr, 
                                    dat, metric, orgby = "pct", absvalue=F, pctvalue=T, 
                                    preunit = "", postunit = "",
                                    decreasingTF, section, capitalizefirst=F, 
                                    foot="", bullets) {
  capitalizefirst<-F
  
  str0<-paste0("From ", minyr," to ", maxyr ,", ",
               funct_list(c(text_liststocks(dat=dat, metric = "YR10", decreasingTF = T, yr=minyr, lv=1, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YR10", decreasingTF = T, yr=minyr, lv=2, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YR10", decreasingTF = T, yr=minyr, lv=3, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit))),
               " had the largest increases, while ", 
               funct_list(c(text_liststocks(dat=dat, metric = "YR10", decreasingTF = F, yr=minyr, lv=1, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YR10", decreasingTF = F, yr=minyr, lv=2, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YR10", decreasingTF = F, yr=minyr, lv=3, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit)))," had the largest decreases.",foot," ")
  section<-"nominal"
  str0<-paste0(str0, #"\n\n ",
               "From ", maxyr-1 ," to ", maxyr ,", ", 
               funct_list(c(text_liststocks(dat=dat, metric = "YOY", decreasingTF = T, yr=maxyr-1, lv=1, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YOY", decreasingTF = T, yr=maxyr-1, lv=2, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YOY", decreasingTF = T, yr=maxyr-1, lv=3, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit)))," had the largest increases, while ", 
               funct_list(c(text_liststocks(dat=dat, metric = "YOY", decreasingTF = F, yr=maxyr-1, lv=1, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YOY", decreasingTF = F, yr=maxyr-1, lv=2, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit), 
                            text_liststocks(dat=dat, metric = "YOY", decreasingTF = F, yr=maxyr-1, lv=3, 
                                            capitalizefirst=capitalizefirst, section=section, 
                                            orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                            preunit = preunit, postunit = postunit))),
               " had the largest decreases.")
  
  return(str0)
  
}


text_increasingdecreasingbullets<-function(minyr, maxyr, dat, orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                                           preunit = preunit, postunit = postunit,
                                           decreasingTF, section=section, capitalizefirst=T, keyphrase) {
  
  a<-paste0("**", keyphrase,": Largest Increases** \n\n ",
            "*From ", minyr,":*", " \n\n ",
            text_liststocks(dat, metric = "YR10", orgby = orgby, absvalue=absvalueTF, pctvalue=pctvalueTF, 
                            preunit = preunit, postunit = postunit,
                            decreasingTF = T, yr, lv = 1, section=section, capitalizefirst=T), " \n\n ", 
            text_liststocks(dat=dat, metric = "YR10", decreasingTF = T, yr=minyr, orgby = orgby,
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,
                            lv=2, capitalizefirst=capitalizefirst, section=section), " \n\n ", 
            text_liststocks(dat=dat, metric = "YR10", decreasingTF = T, yr=minyr, orgby = orgby,
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,
                            lv=3, capitalizefirst=capitalizefirst, section=section), " \n\n ", 
            
            "*From ", maxyr-1,":* \n\n ",
            text_liststocks(dat=dat, metric = "YOY", decreasingTF = T, yr=maxyr-1, orgby = orgby,
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,
                            lv=1, capitalizefirst=capitalizefirst, section="nominal"), " \n\n ",
            text_liststocks(dat=dat, metric = "YOY", decreasingTF = T, yr=maxyr-1, orgby = orgby,
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,
                            lv=2, capitalizefirst=capitalizefirst, section="nominal")," \n\n ",
            text_liststocks(dat=dat, metric = "YOY", decreasingTF = T, yr=maxyr-1, orgby = orgby,
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,
                            lv=3, capitalizefirst=capitalizefirst, section="nominal"), " \n\n ",
            
            "**", keyphrase,": Largest Decreases**",  " \n\n ",
            "*From ",minyr,":*", " \n\n ",
            text_liststocks(dat=dat, metric = "YR10", decreasingTF = F, yr=minyr, 
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,orgby = orgby,
                            lv=1, capitalizefirst=capitalizefirst, section=section), " \n\n ",
            text_liststocks(dat=dat, metric = "YR10", decreasingTF = F, yr=minyr, 
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,orgby = orgby,
                            lv=2, capitalizefirst=capitalizefirst, section=section), " \n\n ",
            text_liststocks(dat=dat, metric = "YR10", decreasingTF = F, yr=minyr, 
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,orgby = orgby,
                            lv=3, capitalizefirst=capitalizefirst, section=section), " \n\n ",
            
            "*From ", maxyr-1,":*", " \n\n ",
            text_liststocks(dat=dat, metric = "YOY", decreasingTF = F, yr=maxyr-1, 
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,orgby = orgby,
                            lv=1, capitalizefirst=capitalizefirst, section="nominal"), " \n\n ",
            text_liststocks(dat=dat, metric = "YOY", decreasingTF = F, yr=maxyr-1, 
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,orgby = orgby,
                            lv=2, capitalizefirst=capitalizefirst, section="nominal"), " \n\n ",
            text_liststocks(dat=dat, metric = "YOY", decreasingTF = F, yr=maxyr-1, 
                            preunit = preunit, postunit = postunit, absvalue = absvalueTF,orgby = orgby,
                            lv=3, capitalizefirst=capitalizefirst, section="nominal"))
  a<- gsub(pattern = " \n\n  \n\n ", replacement = " \n\n ", x = a)
  a<- gsub(pattern = " \n\n  \n\n ", replacement = " \n\n ", x = a)
  return(a)
}

simpleCap <- function(x) {
  str1<-c()
  for (i in 1:length(x)) {
    xx<-tolower(x[i])
    s <- strsplit(xx, " ")[[1]]
    ss<-paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    str1<-c(str1, ss)
  }
  return(str1)
}


tolower2<-function(str0, capitalizefirst=F) {
  str2<-c()
  
  if (str0[1] %in% "") { 
    str<-""
  } else {
    for (i in 1:length(str0)) {
      str1<-gsub(pattern = "\\(", replacement = "\\( ", x = tolower(str0[i]))
      str1<-gsub(pattern = "\\)", replacement = " \\)", x = str1)
      str1<-strsplit(x = str1, split = " ")[[1]]
      str1<-gsub(pattern = "fw", replacement = "freshwater", x = str1, ignore.case = T)
      
      keywords <- c(
        #State
        "Alabama", "Alaska", "California", "Connecticut", 
        "Delaware", "East Florida", "West Florida", "Florida", "Georgia", 
        "Louisiana", "Maine", "Maryland", "Massachusetts", 
        "Mississippi", "New Hampshire", "New Jersey", "New York", 
        "North Carolina", "Oregon", "Rhode Island", "South Carolina", 
        "Texas",  "Virginia", "Washington", 
        #Region
        "North Pacific", "Pacific", "Western Pacific (Hawai`i)", "Western Pacific",
        "New England",
        "Mid-Atlantic","Gulf of Mexico",
        "South Atlantic", 
        #For specific Species
        "Spanish", "Gulf", "Bringham's", "Von Siebold's", "Pfluger's", "African", "Eurpoean",
        # Other
        "Atlantic", "American", 
        "Atka", "Chinook", "Great Lakes") 
      
      # keywords<-c(keywords, paste0("(", keywords), paste0(keywords, ")"))
      
      
      for (ii in 1:length(keywords)) {
        keywords1<-strsplit(x = keywords[ii], split = " ")[[1]]
        if (length(keywords1) %in% 1 & 
            sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T))>0) {
          str1[grep(x = str1, pattern = keywords[ii], ignore.case = T)]<-keywords[ii]
        } else if (length(keywords1) %in% 2 & 
                   sum(grepl(x = str0, pattern = keywords1[1], ignore.case = T)>0) & 
                   sum(grepl(x = str0, pattern = keywords1[2], ignore.case = T)>0)) {
          str1[grep(x = str1, pattern = keywords1[1], ignore.case = T)]<-keywords1[1] 
          str1[grep(x = str1, pattern = keywords1[2], ignore.case = T)]<-keywords1[2] 
        } else if (length(keywords1) %in% 3 & 
                   grepl(x = str0, pattern = keywords1[1], ignore.case = T) & 
                   grepl(x = str0, pattern = keywords1[2], ignore.case = T) &
                   grepl(x = str0, pattern = keywords1[3], ignore.case = T)) {
          str1[sum(grep(x = str1, pattern = keywords1[1], ignore.case = T)>0)]<-keywords1[1] 
          str1[sum(grep(x = str1, pattern = keywords1[2], ignore.case = T)>0)]<-keywords1[2] 
          str1[sum(grep(x = str1, pattern = keywords1[3], ignore.case = T)>0)]<-keywords1[3] 
        }     
      }
      
      # if (str1[1] == "von" & str1[2] == "siebolds") {
      #   str1<-str1[2:length(str1)]
      #   str1<-c("VonSiebold's", str1[3])
      # }
      
      # if (sum(grepl(pattern = "*A'u*", x = str1, ignore.case = T))>=1) {
      #   str1[grepl(pattern = "*A'u*", x = str1, ignore.case = T)]<-"*A\U02BBu*"
      # }
      # 
      # if (sum(grepl(pattern = "*O'io*", x = str1, ignore.case = T))>=1) {
      #   str1[grepl(pattern = "*O'io*", x = str1, ignore.case = T)]<-"*O\U02BBio*"
      # }
      # 
      # if (sum(grepl(pattern = "*'Ahi*", x = str1, ignore.case = T))>=1) {
      #   str1[grepl(pattern = "*'Ahi*", x = str1, ignore.case = T)]<-"*\U02BBAhi*"
      # }
      
      
      str1<-paste(str1, collapse = " ")
      str1<-gsub(pattern = "\\( ", replacement = "\\(", x = str1)
      str1<-gsub(pattern = " \\)", replacement = "\\)", x = str1)
      if (capitalizefirst==T) {
        str1<-paste(toupper(substr(str1, 1, 1)), substr(str1, 2, nchar(str1)), sep="")
        
      }
      
      str1<-gsub(pattern = "&", replacement = "and", x = str1)
      
      str2<-c(str2, str1)
    }
    str2<-trimws(str2)
  }
  return(str2)
}

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

funct_list<-function(x) {
  x<-x[which(x!="")]
  # x<-x[which(!is.null(x))]
  x<-x[which(!is.na(x))]
  # x<-x[order(x)]
  if (length(x)==2) { 
    str1<-paste(x, collapse = " and ")
  } else if (length(x)>2) {
    str1<-paste(x[1:(length(x)-1)], collapse = ", ")
    str1<-paste0(str1, ", and ", x[length(x)])
  } else {
    str1<-x
  }
  return(str1)
}

#Fork of https://github.com/ateucher/useful_code/blob/master/R/numbers2words.r
numbers2words <- function(x){
  ## Function by John Fox found here: 
  ## http://tolstoy.newcastle.edu.au/R/help/05/04/2715.html
  ## Tweaks by AJH to add commas and "and"
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
    helper(x)    
  }
  
}

###***Math/Formatting####

#   Function: pchange()  -  print percent change calculation and direction
pchange<-function(start, end, ending="", percent_first=TRUE){
  #calculate percent change:
  
  if (is.na(start) | is.na(end)) {
    
    final<-paste0(NA, "%")
    
  } else {
    
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
  return(final)
}


modnum<-function(x, divideby = 1000, commaseperator = T) { 
  xxx<-matrix(data = NA, nrow = nrow(x), ncol = ncol(x))
  
  for (c in 1:ncol(x)){
    for (r in 1:nrow(x)){
      xx<-ifelse(is.na(x[r,c]), NA, as.numeric(gsub(x = x[r,c], pattern = ",", replacement = "")))
      # print(paste0(r,", ",c, ", ", xx))
      if (!is.na(xx)) {
        if(xx>0 & xx<999) {
          xx<-"< 1"
        } else if (xx>=1000 & xx<1499) {
          xx<-"1"    
        } else {
          xx<-format(xx/divideby, digits = 0, trim = F, 
                     big.mark = ifelse(commaseperator == T, ",", ""), scientific = F)
        }
      }
      xxx[r,c]<-xx
    }}
  return(xxx)
}


xunits<-function(temp00, combine=T) {
  
  temp00<-sum(as.numeric(temp00))
  if (is.na(temp00)) {
    out<-NA
  } else {
    
    sigfig<-format(temp00, digits = 3, scientific = TRUE)
    sigfig0<-as.numeric(substr(x = sigfig, start = (nchar(sigfig)-1), stop = nchar(sigfig)))
    
    if (sigfig0<=5) {
      # if (sigfig0<4) {
      unit<-""
      x<-format(x = temp00, big.mark = ",", digits = 0, scientific = F)
      # } else if (sigfig0>=4 & sigfig0<6) {
      #   unit<-" thousand"
      # x<-round(temp00/1e3, digits = 1)
      # } else if (sigfig0==5) {
      #   unit<-" thousand"
      #   x<-round(temp00/1e3, digits = 0)
    } else if (sigfig0>=6 & sigfig0<9) {
      unit<-" million"
      x<-round(temp00/1e6, digits = 1)
    } else if (sigfig0>=9 & sigfig0<12) {
      unit<-" billion"
      x<-round(temp00/1e9, digits = 1)
    } else if (sigfig0>=12) {
      unit<-" trillion"
      x<-round(temp00/1e12, digits = 1)
    }
    
    out<-ifelse(combine==T, paste0(x, unit), list(x, unit))
  }
  
  return(out)
}

xunitspct<-function(temp00, sign = T) {
  
  if (is.na(temp00)) {
    temp<-NA
  } else if (temp00 > -1 & temp00 <= 0 | #negative values between 0 and -1
             temp00 < 1 & temp00 >= 0) { #positive values between 1 and 0
    temp<-as.numeric(format(temp00, digits = 0, nsmall = 1, big.mark = ",", trim =T, scientific = F))
  } else {
    temp<-as.numeric(round(temp00, digits = 0))
  }
  
  if (sign == F | is.na(temp00)) {
    out<-temp    
  } else {
    out<-paste0(temp, "%")
  }
  
  return(out)
  
}

format_cells <- function(df.dat, rows ,cols, value = c("italics", "bold", "strikethrough")){
  #https://stackoverflow.com/questions/28166168/how-to-change-fontface-bold-italics-for-a-cell-in-a-kable-table-in-rmarkdown
  # select the correct markup
  map <- setNames(c("*", "**", "~~"), c("italics", "bold", "strikethrough"))
  markup <- map[value]  
  
  for (r in rows){
    for(c in cols){
      
      # Make sure values are not factors
      df.dat[[c]] <- as.character( df.dat[[c]])
      
      # Update formatting
      df.dat[r, c] <- paste0(markup, df.dat[r, c], markup)
    }
  }
  
  return(df.dat)
}


###***Data Management####

df2list<-function(dat, listlevels, collect) {
  alist<-list()
  for (i in 1:length(unique(dat[, names(dat) %in% listlevels]))) {
    id<-as.character(unique(dat[,names(dat) %in% listlevels])[i])
    alist[[i]]<-dat[as.character(dat[,names(dat) %in% listlevels]) %in% id,
                    names(dat) %in% collect]
    names(alist)[[i]]<-id
  }
  return(alist)
}

numbers0<-function(x) {
  xx<-rep_len(x = NA, length.out = length(x))
  for (i in 1:length(x)){
    xx[i]<-paste0(paste(rep_len(x = 0, 
                                length.out = nchar(max(x))-nchar(x[i])), 
                        collapse = ""), 
                  as.character(x[i]))
  }
  return(xx)
}

funct_counter<-function(counter0) {
  counter00<-ifelse(as.numeric(counter0) %in% 0, 1, as.numeric(counter0)+1)
  counter<-numbers0(c(counter00, as.numeric(paste0("1", 
                                                   paste(rep_len(x = 0, length.out = (nchar(counter0)-1)), 
                                                         collapse = "")))))[1]
  return(counter)
}


ageoffile<-function(path) {
  # system("touch temp")
  info <- file.info(path)
  x<-format(info$mtime,"%B %d, %Y")
  return(x)
}



###***File Saving####


temp.save<-function(df.dat, minyr, maxyr, filename00, ending, csvname, Tfootnotes=NA, reg, st) {
  
  if (sum(names(df.dat) %in% paste0("X", maxyr))==1) {  #If the names of the columns are "X2017", make them "2017"
    for (i in 1:length(minyr:maxyr)) {
      idx<-which(names(df.dat) %in% paste0("X", (minyr:maxyr)[i]))
      names(df.dat)[idx]<-as.character((minyr:maxyr)[i])
    }    
  }
  
  #Save CSV
  filename0<-gsub(pattern = "_XXX", replacement = paste0("_", ending), x = filename00)
  df0<-df.dat
  
  if (is.data.frame(df0)) { #as opposed to if the data frame says "no values"
    # df0$Footnotes<-list2string.ft(df0$Footnotes)
    aa<-rep_len(x = "", length.out = ncol(df0))
    aaa<-names(df0)
    names(aaa)<-names(aa)<-names(df0)
    for(ii in 1:ncol(df0)) {
      df0[,ii]<-as.character(df0[,ii])
    }
    df0<-rbind.data.frame(aa,
                          aaa, 
                          df0)
    # df0$Footnotes<-gsub(pattern = ", ", replacement = "_ ", x = df0$Footnotes)
  }
  
  names(df0)<-NULL
  df0<-as.matrix(df0)
  
  for (i in 1:ncol(df0)) {
    df0[,i]<-paste(df0[,i])
  }
  
  write.table(csvname,  
              file=filename0, 
              sep = ",",
              row.names=FALSE, col.names = F, append = F)
  
  write.table(ifelse((st %in% c("Region", "United States") | st %in% reg), 
                     paste0(reg, " Region"), 
                     paste(trimws(reg), " Region, ", st)),  
              file=filename0, 
              sep = ",",
              row.names=FALSE, col.names = F, append = T)
  
  write.table(df0,  
              file=filename0, 
              sep = ",",
              row.names=FALSE, col.names = F, append = T)
  
  if (!is.na(Tfootnotes) | Tfootnotes %in% ""){
    
    write.table("",  
                file=filename0, 
                sep = ",",
                row.names=FALSE, col.names = F, append = T)
    
    a<-strsplit(x = Tfootnotes, split = " 123456789 ")[[1]]
    a<-unique(a)
    write.table(a,  
                file=filename0, 
                sep = ",",
                row.names=FALSE, col.names = F, append = T)
  }
  
  #footnote-ify df.dat footnote column for rmarkdown
  if (is.data.frame(df.dat)) {
    df.dat$Footnotes<-list2string.ft(x = df.dat$Footnotes)
  }
  
  return(df.dat)
  
}  


funct_save<-function(Tfootnotes = NA, 
                     place, area, folder,
                     maxyr, minyr, 
                     csvname1000, csvname, 
                     st, reg, xreg, xsect, xstate, dir.outputtables, statereg, 
                     temp.code = NA,
                     # temp.ref = NA,
                     temp.print = NA,
                     webtool = NA) {
  
  #SAVE STUFF
  # filename00<-paste0(dir.outputtables, folder, '/',area,'_', 
  #                    gsub(pattern = " ", replacement = "", simpleCap(place)), 
  #                    '_', folder,'_XXX.csv')
  
  filename00<-paste0(dir.outputtables, folder, '/',
                     FilenameAuto(xreg, xsect, xstate, 
                                  xdesc =  paste0(folder, "_",area,"_XXX.csv")))
  # area,'_', 
  # gsub(pattern = " ", replacement = "", simpleCap(place)), 
  # '_', folder,'_XXX.csv')
  # 
  if (!(length(webtool) %in% 1)){
    #Webtool
    webtool<-data.frame(webtool)
    webtool$Footnotes<-as.character(list2string.webtool(x = webtool$Footnotes))  #Convert Footnote from list
    
    #add State and Region collumns
    if (sum(names(webtool) %in% c("State", "Region"))>1) {
      webtool<-webtool
    } else if (sum(names(webtool) %in% "State")>0) {
      webtool<-cbind.data.frame(Region=reg, 
                                webtool)
    } else {
      webtool<-cbind.data.frame(Region=reg, 
                                State = st, 
                                webtool)
    }
    
    #If the names of the columns are "X2017", make them "2017"
    if (sum(names(webtool) %in% paste0("X", maxyr))==1) {
      for (i in 1:length(minyr:maxyr)) {
        idx<-which(names(webtool) %in% paste0("X", (minyr:maxyr)[i]))
        names(webtool)[idx]<-as.character((minyr:maxyr)[i])
      }    
    }  
    
  }
  
  
  Tfootnotes<-unique(Tfootnotes)
  
  #Raw/Code
  if (!(length(temp.code) %in% 1)) { # if (!(is.na(temp.code)) & ncol(temp.code) %in% 1){
    temp.code<-temp.save(df.dat = temp.code, minyr, maxyr, filename00, ending = "raw", csvname = csvname, Tfootnotes = Tfootnotes, reg, st) 
  }
  #Print
  if (!(length(temp.print) %in% 1)) { # if (!(is.na(temp.print))){
    temp.print<-temp.save(df.dat = temp.print, minyr, maxyr, filename00, ending = "print", csvname = csvname1000, Tfootnotes, reg, st) 
  }
  
  # #Reference
  # if (!(length(temp.ref) %in% 1)) {# if (!(is.na(temp.ref))){
  #   temp.ref<-temp.save(df.dat = temp.ref, minyr, maxyr, filename00, ending = "ref", csvname = csvname, Tfootnotes, reg, st) 
  # }
  
  return(list("temp.code" = (temp.code), 
              # "temp.ref" = (temp.ref), 
              "temp.print" = (temp.print), 
              "webtool" = (webtool)))
  
}

FilenameAuto<-function(xreg, xsect, xstate, xdesc){
  
  
  State <- c("United States", "Alabama", "Alaska", "California", "Connecticut", 
             "Delaware", "East Florida", "West Florida", "Georgia", "Hawai`i", 
             "Louisiana", "Maine", "Maryland", "Massachusetts", 
             "Mississippi", "New Hampshire", "New Jersey", "New York", 
             "North Carolina", "Oregon", "Rhode Island", "South Carolina", 
             "Texas",  "Virginia", "Washington") # states in order
  
  State1<-State
  State1[grep(pattern = "Florida", x = State)]<-"Florida"
  State1[grep(pattern = "Hawai`i", x = State)]<-"Hawaii"
  
  statereg <- data.frame(State = State, 
                         State1 = State1, 
                         fips = c(0, 1 ,2 ,6 ,9 ,10, 12, 12, 13, 15, 22,23,24,25,28,33,34,36,37,41,44,45,48,51,53), 
                         Region = c("United States", "Gulf of Mexico", "North Pacific", "Pacific", "New England", 
                                    "Mid-Atlantic", "South Atlantic", "Gulf of Mexico", "South Atlantic", "Western Pacific (Hawai`i)", 
                                    "Gulf of Mexico", "New England", "Mid-Atlantic", "New England", 
                                    "Gulf of Mexico", "New England", "Mid-Atlantic", "Mid-Atlantic", 
                                    "South Atlantic", "Pacific", "New England", "South Atlantic", 
                                    "Gulf of Mexico",  "Mid-Atlantic", "Pacific"), 
                         abbvst = c("US", "AL", "AK", "CA", "CT", "DE", "EFL", "WFL", "GA", "HI", 
                                    "LA", "ME", "MD", "MA", "MI", "NH", "NJ", "NY",  
                                    "NC", "OR", "RI", "SC", "TX",  "VA", "WA"), 
                         abbvreg = c("US", "GOM", "NP", "Pac", "NE", 
                                     "MA", "SA", "GOM", "SA", "WP", 
                                     "GOM", "NE", "MA", "NE", 
                                     "GOM", "NE", "MA", "MA", 
                                     "SA", "Pac", "NE", "SA", 
                                     "GOM",  "MA", "Pac"), 
                         xstate = c("0", "1", "1", "1", "1", "1", "1", "2", "2", "1", "3", "2", "2", "3", "4", 
                                    "4", "3", "4", "3", "2", "5", "4", "5",  "5", "3"), 
                         xreg = c("0", "7", "1", "2", "4", "5", "6", "7", "6", "3", "7", "4", "5", "4", "7", 
                                  "4", "5", "5", "6", "2", "4", "6", "7",  "5", "2"))
  
  
  ###
  
  
  ref<-data.frame(code = c(1, 2, 3, 4), 
                  element = c("xsect", "xsect", "xsect", "xsect"), 
                  meaning = c("ManagCont", "Comm", "Rec", "MarEcon"))
  
  xdesc<-paste0(unique(as.character(statereg$abbvreg[statereg$xreg %in% xreg])),#"Reg",
                "_", ref$meaning[ref$code %in% xsect], #section
                ifelse(xstate == 0,
                       "",
                       paste0("_", unique(as.character(statereg$abbvst[statereg$xstate %in% xstate &
                                                                         statereg$xreg %in% xreg])))#"St"
                ),
                "_",
                xdesc)
  
  xdesc<-gsub(pattern = "\\(", replacement = "", x = 
                gsub(pattern = ")", replacement = "", x = 
                       gsub(pattern = "`", replacement = "", x = 
                              gsub(xdesc, pattern = " ", replacement = ""))))
  
  filename0<-paste0(xreg, "_", xsect, "_", xstate, "_", xdesc)
  
  return(filename0) 
}

###***Metadata####


CreateLoadedDataMetadata<-function(dir.out, data, title) {
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 style = "heading 1") %>%
    
    body_add_par("Code Author", style = "heading 2") %>%
    body_add_par("Writiten by Emily Markowitz, emilyhmarkowitz@gmail.com/emily.markowitz@noaa.gov", style = "Normal") %>%  
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    # body_add_par("System Info:", style = "heading 2") %>%
    # body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    # body_add_par("R Version", style = "heading 2") %>%
    # body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
    body_add_par("Input Data used in this Run",
                 style = "heading 2")
  
  for (i in 1:length(loaded.data)){
    
    temp<-loaded.data[[i]]
    
    my_doc <- my_doc %>%
      body_add_par(names(loaded.data)[i], style = "heading 3") %>%
      body_add_table(head(temp)) %>%
      body_add_par(summary(temp), style = "Normal")
  }
  
  # a<-sessionInfo()
  # my_doc <- my_doc %>% 
  #   body_add_par("R Packages Loaded", style = "heading 2")
  # for (i in 1:length(a$basePkgs)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(a$basePkgs[i], style = "Normal") 
  # }
  # for (i in 1:length(a$otherPkgs)){
  #   temp<-a$otherPkgs[[i]]
  #   my_doc <- my_doc %>% 
  #     body_add_par(temp$Package,
  #                  style = "heading 3") %>%
  #     body_add_par(temp$Version, style = "Normal") %>%
  #     body_add_par(temp$Title, style = "Normal") %>%
  #     body_add_par(temp$Description, style = "Normal") %>%
  #     body_add_par(temp$SystemRequirements, style = "Normal") %>%
  #     body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
  #     body_add_par(temp$URL, style = "Normal")
  # }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}


CreateMetadata<-function(dir.out, title){
  my_doc <- read_docx() 
  styles_info(my_doc)
  
  my_doc <- my_doc %>% 
    body_add_par(title,
                 # body_add_par(paste0("Population Narrative of ", commorg, " (", fp_text(sciname, italic = T, color = "black", font.size=10), ")"," in ", region),
                 style = "heading 1") %>%
    
    body_add_par("Code Author", style = "heading 2") %>%
    body_add_par("Writiten by Emily Markowitz, emilyhmarkowitz@gmail.com/emily.markowitz@noaa.gov", style = "Normal") %>%  
    body_add_par("Date Code Ran:", style = "heading 2") %>%
    body_add_par(Sys.time(), style = "Normal") %>%
    body_add_par("System Info:", style = "heading 2") %>%
    body_add_par(paste0(Sys.info()[[1]], " ", R.version$platform), style = "Normal") %>%
    body_add_par("R Version", style = "heading 2") %>%
    body_add_par(paste0(R.version$version.string, ": ", R.version$nickname), style = "Normal") #%>%
  #   body_add_par("Populations Run in this Iteration", 
  #                style = "heading 2") 
  # for (i in 1:length(org_pop)){
  #   my_doc <- my_doc %>% 
  #     body_add_par(org_pop[i], style = "Normal") 
  # }
  
  a<-sessionInfo()
  my_doc <- my_doc %>% 
    body_add_par("R Packages Loaded", style = "heading 2")
  for (i in 1:length(a$basePkgs)){
    my_doc <- my_doc %>% 
      body_add_par(a$basePkgs[i], style = "Normal") 
  }
  for (i in 1:length(a$otherPkgs)){
    temp<-a$otherPkgs[[i]]
    my_doc <- my_doc %>% 
      body_add_par(temp$Package,
                   style = "heading 3") %>%
      body_add_par(temp$Version, style = "Normal") %>%
      body_add_par(temp$Title, style = "Normal") %>%
      body_add_par(temp$Description, style = "Normal") %>%
      body_add_par(temp$SystemRequirements, style = "Normal") %>%
      body_add_par(paste0(temp$`Authors@R`), style = "Normal") %>%
      body_add_par(temp$URL, style = "Normal")
  }
  
  print(my_doc, target = paste0(dir.out, "/Metadata_", Sys.Date(), ".docx"))
}



######END######

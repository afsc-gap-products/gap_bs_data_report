#' ---
#' title: Data Report: Select relevant species
#' purpose: List and select all of the relevant species for each survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---

# library(devtools)
# devtools::install_github("emilyhmarkowitz/RMarkReports")
library(RMarkReports)


# General stuff

yrofsurvey<-(maxyr-2018)+37

stndth<-ifelse(grepl(pattern = 1, x = substr(x = (maxyr-2018)+37, start = nchar((maxyr-2018)+37), 
                                             stop = nchar((maxyr-2018)+37) )), 
               "st", 
               ifelse(grepl(pattern = 2, x = substr(x = (maxyr-2018)+37, start = nchar((maxyr-2018)+37), 
                                                    stop = nchar((maxyr-2018)+37))), "nd", "th")
)

spplist<-SpeciesList[survey]



# 




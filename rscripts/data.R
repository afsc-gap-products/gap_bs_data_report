#' ---
#' title: Data Report: Load Data
#' purpose: Load data from survey for data report
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---

 vesdat<-data.frame("VESSEL_NAME" = c("FV Vesteraalen", "FV Alaska Knight"), 
                    "VESSEL" = c(162, 94), 
                    "VESSEL_M" = c(38, 43.5) )


if (survey %in% "EBS") {
  
  SURVEY<-"eastern Bering Sea"
  sectname<-"EBS-BTS-Report"
  
  dat<-read.csv(file = "./data/ebs2017_2018.csv")
  dat<-merge.data.frame(x = dat, y = vesdat, by = "VESSEL")
  
}



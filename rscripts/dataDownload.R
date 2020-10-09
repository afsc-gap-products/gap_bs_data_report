#' ---
#' title: Data Report: Download Data
#' purpose: Download relevant data for each survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---



if (survey %in% "EBS") {
  # Download EBS
  download.file(url = "https://www.afsc.noaa.gov/RACE/groundfish/survey_data/downloads/ebs2017_2018.zip", 
                destfile=paste0("./data/ebs2017_2018.zip") )
  
  
  if (filetype==".zip") {
    zip::unzip(zipfile = paste0("./data/ebs2017_2018.zip"), 
          overwrite = T,
          exdir = paste0("./data/"))
  }
}
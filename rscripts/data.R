#' ---
#' title: Data Report: Load Data
#' purpose: Load data from survey for data report
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---


Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")


vesdat<-data.frame("VESSEL_NAME" = c("FV Alaska Knight", "FV Vesteraalen"), 
                    "VESSEL" = c(162, 94), 
                    "VESSEL_M" = c(43.5, 38) )
 
shps<-c(21:25)


if (survey %in% "EBS") {
  
  SURVEY<-"eastern Bering Sea"
  sectname<-"EBS-BTS-Report"
  map.area<-"bs.south"
  
  dat<-read.csv(file = "./data/ebs2017_2018.csv")
  dat<-merge.data.frame(x = dat, y = vesdat, by = "VESSEL")
  
  reg_dat <- get_base_layers(select.region = map.area, set.crs = "auto")
  
  # First read in the shapefile, using the path to the shapefile and the shapefile name minus the
  # extension as arguments
  surveygrid_shp00 <- readOGR(here("/shapefiles/southern_survey_grid_trimmed/southern_survey_grid_trimmed"), 
                       "southern_survey_grid_trimmed")
  
  surveygrid_shp0 <- sp::spTransform(x = surveygrid_shp00, #[,c("FID_1", "AREA", "PERIMETER", "STATION_ID")], 
                                    CRSobj = crs(reg_dat$akland))
  
  surveygrid_shp <- fortify(surveygrid_shp0) # Next the shapefile has to be converted to a dataframe for use in ggplot2


  
}
 

  
  dat$VESSEL_shp <- mapvalues(dat$VESSEL, 
                                 from=c(unique(dat$VESSEL)), 
                                 to=c(shps[1:length(unique(dat$VESSEL))]))
  

minyr <- min(dat$YEAR)
dat<-dat[dat$YEAR %in% minyr:maxyr, ]
dat$COMMON_NAME<-dat$COMMON
dat$STATIONID<-dat$STATION
dat$CPUE_KGHA<-0

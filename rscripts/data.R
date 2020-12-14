#' ---
#' title: Data Report: Load Data
#' purpose: Load data from survey for data report
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---


####SPECIES###

invert<-c("Porifera",
          "Cnidaria",
          "Platyhelminthes",
          "Nematoda",
          "Annelida",
          "Chromista",
          "Echinodermata",
          "Arthropoda",
          "Mollusca")
vert<-c("Urochordata",
        "Agnatha",
        "Chondrichthyes",
        "Sarcopterygii",
        "Tetrapoda",
        "Actinopterygii")
fish<-c("Agnatha",
        "Chondrichthyes",
        "Sarcopterygii",
        "Actinopterygii")
other<-c("Plantae",
         "Fungi",
         "Protozoa",
         "Bacteria",
         "Archaea")




SpCodeName.General<-list("Walleye Pollock" = 934083, # Species	Gadus chalcogrammus Pallas, 1814 – Walleye ), 
                         'Pacific cod' = 164711, #Species	Gadus macrocephalus Tilesius, 1810 – morue du Pacifique, bacalao del Pacifico, Pacific cod
                         "Yellowfin Sole" = 172907, # Species	Limanda aspera (Pallas, 1814) – yellowfin sole, limande à nageoires jaunes, Yellowfin Sole 
                         "Northern Rock Sole" = 616392, # Species	Lepidopsetta polyxystra Orr & Matarese, 2000 – northern rock sole, limande du nord, Northern Rock Sole
                         "Southern Rock Sole" = 172917, # Species	Lepidopsetta bilineata (Ayres, 1855) – rock sole, fausse limande du Pacifique, Rock Sole
                         "Flathead Sole" = 172875, # Species	Hippoglossoides elassodon Jordan & Gilbert, 1880 – flathead sole, Flathead Sole, plie à tête plate
                         "Bering Flounder" = 172876, # Species	Hippoglossoides robustus Gill & Townsend, 1897 – Bering flounder, Bering Flounder, plie de Béring
                         "Alaska Plaice" = 172901, # Species	Pleuronectes quadrituberculatus Pallas, 1814 – Alaska plaice, Alaska Plaice
                         "Greenland Turbot" = 172930, #  Species	Reinhardtius hippoglossoides (Walbaum, 1792) – Greenland halibut, platija negra, Greenland turbot, Newfoundland turbot, turbot, greeenland halibut, flétan du Groenland, Greenland Halibut
                         "Arrowtooth Flounder" = 172862, # Species	Atheresthes stomias (Jordan & Gilbert, 1880) – arrowtooth flounder, Arrowtooth Flounder, plie à grande bouche 
                         "Kamchatka Flounder" = 172861, #  Species	Atheresthes evermanni Jordan & Starks, 1904 – Kamchatka flounder, Kamchatka Flounder
                         "Pacific Halibut" = 172932) #Species: Hippoglossus stenolepis Schmidt, 1904 – valid)


SpeciesList<-list("EBS" = list("Walleye Pollock" = SpCodeName.General$`Walleye Pollock`, 
                               'Pacific cod' = SpCodeName.General$`Pacific cod`,
                               "Yellowfin Sole" = SpCodeName.General$`Yellowfin Sole`, 
                               "Northern and Southern Rock Sole (grouped)" = c(SpCodeName.General$`Northern Rock Sole`, 
                                                                               SpCodeName.General$`Southern Rock Sole`), 
                               "Flathead Sole" = SpCodeName.General$`Flathead Sole`, 
                               "Bering Flounder" = SpCodeName.General$`Bering Flounder`, 
                               "Alaska Plaice" = SpCodeName.General$`Alaska Plaice`, 
                               "Greenland Turbot" = SpCodeName.General$`Greenland Turbot`, 
                               "Arrowtooth Flounder" = SpCodeName.General$`Arrowtooth Flounder`, 
                               "Kamchatka Flounder" = SpCodeName.General$`Kamchatka Flounder`, 
                               "Pacific Halibut" = SpCodeName.General$`Pacific Halibut`)
)


####REPORT SPECIFIC###

Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")


vesdat<-data.frame("VESSEL_NAME" = c("FV Alaska Knight", "FV Vesteraalen"), 
                    "VESSEL" = c(162, 94), 
                    "VESSEL_M" = c(43.5, 38) )
 
shps<-c(21:25)


if (SRVY %in% "EBS") {
  
  SURVEY<-"eastern Bering Sea"
  sectname<-"EBS-BTS-Report"
  map.area<-"bs.south"
  
  dat<-read.csv(file = "./data/ebs2017_2018.csv")
  dat<-merge.data.frame(x = dat, y = vesdat, by = "VESSEL")
  
  reg_dat <- get_base_layers(select.region = map.area, set.crs = "auto")
  
  # First read in the shapefile, using the path to the shapefile and the shapefile name minus the
  # extension as arguments
  surveygrid_shp00 <- readOGR(here("./shapefiles/southern_survey_grid_trimmed/southern_survey_grid_trimmed"), 
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

# Time
dates<-strsplit(x = dat$DATETIME, split = " ")
dat$dates<-sapply(dates, "[", 1 )

dat$DAY<-as.numeric(substr(x = as.character(dat$dates), 
                                    start = 4, stop = 5))
dat$MONTH<-as.numeric(substr(x = as.character(dat$dates), 
                                   start = 1, stop = 2))


months.words<-c("January", "February",	"March", "April", 
                "May", "June", "July", "August", 
                "September", "October", "November", "December")



load(file = "./data/specieslistinTSN.rdata")
reftable<-spp.cat2$reftable
reftable<-dplyr::rename(reftable, 
                        "SID" = "SID_orig")
dat<-left_join(dat, reftable, "SID")
spp.tsn.list<-spp.cat2$tsn.list
dat.maxyr<-dat[dat$YEAR %in% maxyr,]




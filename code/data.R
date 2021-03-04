#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. J. Dawson, R. Haehn and E. H. Markowitz'
#' purpose: Wrangle data
#' start date: 2021-03-03
#' date modified: 2021-03-03        # CHANGE
#' Notes:                             # CHANGE
#' ---


######## Knowns ########

months.words<-c("January", "February",	"March", "April", 
                "May", "June", "July", "August", 
                "September", "October", "November", "December")

#### Report Specific#######

shps<-c(21:25)


if (SRVY %in% "EBS") {
  
  SURVEY<-"eastern Bering Sea"
  sectname<-"EBS-BTS-Report"
  map.area<-"bs.south"
  
  # ageofdownload<-ageoffile(path = "./data/ebs2017_2018.csv")
  # dat<-read.csv(file = "./data/ebs2017_2018.csv")
  # dat<-merge.data.frame(x = dat, y = vesdat, by = "VESSEL")
  
  reg_dat <- get_base_layers(select.region = map.area, set.crs = "auto")
  
  # First read in the shapefile, using the path to the shapefile and the shapefile name minus the
  # extension as arguments
  surveygrid_shp00 <- readOGR(dsn = here::here("shapefiles","STRATA", "EBS_NBS_2019.shp"))
  
  proj4string(surveygrid_shp00) <- crs(reg_dat$akland)
  
  # surveygrid_shp0 <- sp::spTransform(x = surveygrid_shp00, 
  #                                    CRSobj = crs(reg_dat$akland), )
  
  surveygrid_shp <- fortify(surveygrid_shp00) # Next the shapefile has to be converted to a dataframe for use in ggplot2
  
  # SpeciesList<-list("Walleye Pollock" = SpCodeName.General$`Walleye Pollock`, 
  #                   'Pacific cod' = SpCodeName.General$`Pacific cod`,
  #                   "Yellowfin Sole" = SpCodeName.General$`Yellowfin Sole`, 
  #                   "Northern and Southern Rock Sole (grouped)" = c(SpCodeName.General$`Northern Rock Sole`, 
  #                                                                   SpCodeName.General$`Southern Rock Sole`), 
  #                   "Flathead Sole" = SpCodeName.General$`Flathead Sole`, 
  #                   "Bering Flounder" = SpCodeName.General$`Bering Flounder`, 
  #                   "Alaska Plaice" = SpCodeName.General$`Alaska Plaice`, 
  #                   "Greenland Turbot" = SpCodeName.General$`Greenland Turbot`, 
  #                   "Arrowtooth Flounder" = SpCodeName.General$`Arrowtooth Flounder`, 
  #                   "Kamchatka Flounder" = SpCodeName.General$`Kamchatka Flounder`, 
  #                   "Pacific Halibut" = SpCodeName.General$`Pacific Halibut`)
  
  # report_species<-data.frame(species = c("Walleye Pollock", 
  #                                        'Pacific cod', 
  #                                        "Yellowfin Sole", 
  #                                        "Northern and Southern Rock Sole (grouped)", 
  #                                        "Flathead Sole", 
  #                                        "Bering Flounder", 
  #                                        "Alaska Plaice", 
  #                                        "Greenland Turbot", 
  #                                        "Arrowtooth Flounder", 
  #                                        "Kamchatka Flounder", 
  #                                        "Pacific Halibut"), 
  #                            sci = c("Walleye Pollock", 
  #                                    'Pacific cod', 
  #                                    "Yellowfin Sole", 
  #                                    "Northern and Southern Rock Sole (grouped)", 
  #                                    "Flathead Sole", 
  #                                    "Bering Flounder", 
  #                                    "Alaska Plaice", 
  #                                    "Greenland Turbot", 
  #                                    "Arrowtooth Flounder", 
  #                                    "Kamchatka Flounder", 
  #                                    "Pacific Halibut"))
  
  report_species<-list("Walleye Pollock" = c(21740, 21741, 21742, 21744),
                    'Pacific cod' = c(21720, 21721, 21722),
                    "Yellowfin Sole" = c(10210, 10209),
                    "Northern and Southern Rock Sole (grouped)" = c(10262, 10261, 10263),
                    "Flathead Sole" = c(10130),
                    "Bering Flounder" = c(10140, 10141),
                    "Alaska Plaice" = c(10285), # TOLEDO - include 10221 (Platichthys stellatus X Pleuronectes quadrituberculatus hybrid hybrid starry flounder X Alaska plaice)
                    "Greenland Turbot" = c(10115, 10116),
                    "Arrowtooth Flounder" = c(10110),
                    "Kamchatka Flounder" = c(10112),
                    "Pacific Halibut" = c(10120, 10121))
  
}


if (SRVY %in% "NEBS") {
  
  SURVEY<-"northern and eastern Bering Sea"
  sectname<-"EBS-BTS-Report"
  map.area<-"bs.south"
  
  ageofdownload<-ageoffile(path = "./data/ebs2017_2018.csv")
  # dat<-read.csv(file = "./data/ebs2017_2018.csv")
  # dat<-merge.data.frame(x = dat, y = vesdat, by = "VESSEL")
  
  reg_dat <- get_base_layers(select.region = map.area, set.crs = "auto")
  
  # First read in the shapefile, using the path to the shapefile and the shapefile name minus the
  # extension as arguments
  surveygrid_shp00 <- readOGR(here::here("shapefiles", "southern_survey_grid_trimmed", "southern_survey_grid_trimmed"), 
                              "southern_survey_grid_trimmed")
  
  # surveygrid_shp0 <- sp::spTransform(x = surveygrid_shp00, #[,c("FID_1", "AREA", "PERIMETER", "STATION_ID")], 
  #                                    CRSobj = crs(reg_dat$akland))
  proj4string(surveygrid_shp00) <- crs(reg_dat$akland)
  
  surveygrid_shp <- fortify(surveygrid_shp00) # Next the shapefile has to be converted to a dataframe for use in ggplot2
  
  report_species<-list("Walleye Pollock" = c(21740, 21741, 21742, 21744),
                       'Pacific cod' = c(21720, 21721, 21722),
                       "Yellowfin Sole" = c(10210, 10209),
                       "Northern and Southern Rock Sole (grouped)" = c(10262, 10261, 10263),
                       "Flathead Sole" = c(10130),
                       "Bering Flounder" = c(10140, 10141),
                       "Alaska Plaice" = c(10285), # TOLEDO - include 10221 (Platichthys stellatus X Pleuronectes quadrituberculatus hybrid hybrid starry flounder X Alaska plaice)
                       "Greenland Turbot" = c(10115, 10116),
                       "Arrowtooth Flounder" = c(10110),
                       "Kamchatka Flounder" = c(10112),
                       "Pacific Halibut" = c(10120, 10121))
  
}

################## Load Oracle Data #################
a<-list.files(path = here::here("data", "oracle"))
for (i in 1:length(a)){
  b <- read_csv(file = paste0(here::here("data", "oracle", a[i])))
  b <- janitor::clean_names(b)
  if (names(b)[1] %in% "x1"){
    b$x1<-NULL
  }
  assign(x = gsub(pattern = "\\.csv", replacement = "", x = a[i]), value = b)
}


######*** domain (survey area IDs)#########
domain<-data.frame(stratum = unique(stratum$stratum))
domain$region <- NA
domain$region[domain$stratum %in% c(81, 70, 71)]<-"NBS"
domain$region[domain$stratum %in% c(90, 82, 62, 43, 61, 41, 20, 42, 32, 50, 31, 10)]<-"EBS"
domain$domain <- NA
domain$domain[domain$stratum %in% c(10, 20)]<-"EBS Inner Shelf"
domain$domain[domain$stratum %in% c(31, 32, 41, 42, 43, 82)]<-"EBS Middle Shelf"
domain$domain[domain$stratum %in% c(50, 61, 62, 90)]<-"EBS Outer Shelf"

######*** stratum_area (survey area)#########
stratum_area <- stratum %>% 
  # dplyr::right_join(x = ., y = domain, by = "stratum") %>%
  filter(region == SRVY0 &
           year <= maxyr & 
           stratum < 100 &
           # !(stratum %in%  c(70, 71, 81)) &
           stratum %in% domain$stratum[domain$region %in% SRVY]) %>%
  filter(year == max(year))

#G:\HaehnR\rScripts\working on for techmemo\tables_TechMemo\code\Fig_1_stratra_area_hauls.R
# ## year = 2019 is most up to date- not updated every year
# strata_area <- sqlQuery(channel, "SELECT STRATUM, AREA
# 
# FROM RACEBASE.STRATUM
# 
# WHERE region = 'BS' AND
# year = 2019 AND
# stratum < 100 AND stratum != 70 AND
# stratum != 71 AND stratum != 81
# ORDER BY STRATUM;
#                          ")


######*** haul_cruises#########
haul_cruises<-left_join(x = haul, y = cruises, by = "cruisejoin")
haul_cruises<-haul_cruises %>%
  dplyr::filter(performance >= 0 & 
                  haul_type == 3 & 
                  !(is.null(stationid)) &
                  stratum %in% c(10,20,31,32,41,42,43,50,61,62,82,90)) 

######*** catch_haul_cruises, dat, catch_haul_cruises_maxyr, dat_maxyr#########
dat <- catch_haul_cruises<-dplyr::left_join(x = catch, y = haul_cruises, by = c("hauljoin", "cruisejoin")) 
dat_maxyr <- catch_haul_cruises_maxyr <- catch_haul_cruises[catch_haul_cruises$year %in% maxyr,]
dat_maxyr_1 <- catch_haul_cruises_maxyr_1 <- catch_haul_cruises[catch_haul_cruises$year %in% (maxyr-1),]


######*** haul_info #########

haul_info <- hauls[,c( "cruise_id", "haul", "station", "stratum", "performance",  "haul_type", "gear")]

# haul_info <- sqlQuery(channel, "SELECT
# CRUISE_ID, HAUL, STATION, STRATUM, PERFORMANCE, HAUL_TYPE, GEAR
# FROM
# RACE_DATA.HAULS")
# 
# 
## dataframe to assign domain (inner, middle, outer)


# latex(tablular( (Factor(strata_domain$domain, "Stratum"))))

######*** vessel_info #########

vessel_info<-vessels[vessels$vessel_id %in% unique(catch_haul_cruises_maxyr$vessel_id), ]

# vesdat<-data.frame("VESSEL_NAME" = c("FV Alaska Knight", "FV Vesteraalen"), 
#                    "VESSEL" = c(162, 94), 
#                    "VESSEL_M" = c(43.5, 38) )

vessel_info$VESSEL_SHP <- mapvalues(vessel_info$vessel_id,
                            from=c(unique(vessel_info$vessel_id)),
                            to=c(shps[1:length(unique(vessel_info$vessel_id))]))

######*** spp_info #########
# species_classification$species_code<-as.numeric(as.character(species_classification$species_code))
spp_info <- dplyr::left_join(x = catch_haul_cruises_maxyr[, "species_code"], 
                             y = species_classification)

########*** length_type #########
length_type <- data.frame(matrix(data = c("1", "Fork length measurement,from tip of snout to fork of tail.",
                                          "2", "Mideye to fork of tail.",
                                          "3", "Tip of snout to hypural plate (standard).",
                                          "4", "Mideye to hypural plate.",
                                          "5", "Total length (extremity to extremity).",
                                          "6", "Snout to second dorsal (e.G. Ratfish).",
                                          "7", "Length of carapace from back of right eye socket to end of carapace.",
                                          "8", "Width of carapace.",
                                          "9", "Head length (snout tip to posterior opercular margin).",
                                          "11", "Snout to anal fin origin (e.G. Rattails).",
                                          "12", "Mantle length (e.G. Squid).",
                                          "13", "Posterior of orbital to end of telson (e.g. Shrimp).",
                                          "14", "Wingtip to wingtip (e.g. skates and rays)",
                                          "15", "Outer tip of rostrum to end of telson (e.g. shrimp)",
                                          "16", "Modal, created in merge juveniles script",
                                          "17", "Length frequency estimited using size composition proportions from adjacent hauls with similar catch composition"), 
                                 ncol = 2, byrow = TRUE))

names(length_type) <- c("code", "description")
length_type$sentancefrag <- c("fork length",
                      "mideye to fork length",
                      "tip of snout to hypural plate length",
                      "mideye to hypural plate length",
                      "total length",
                      "snout to second dorsal length",
                      "carapace from back of right eye socket to end of carapace length",
                      "carapace width",
                      "head length",
                      "snout to anal fin origin length",
                      "mantle length",
                      "posterior of orbital to end of telson length",
                      "wingtip to wingtip length",
                      "outer tip of rostrum to end of telson length",
                      "modal length (created in merge juveniles script)",
                      "length frequency estimited using size composition proportions from adjacent hauls with similar catch composition")


#### Species ######

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





# SpCodeName.General<-list("Walleye Pollock" = 934083, # Species	Gadus chalcogrammus Pallas, 1814 – Walleye ), 
#                          'Pacific cod' = 164711, #Species	Gadus macrocephalus Tilesius, 1810 – morue du Pacifique, bacalao del Pacifico, Pacific cod
#                          "Yellowfin Sole" = 172907, # Species	Limanda aspera (Pallas, 1814) – yellowfin sole, limande à nageoires jaunes, Yellowfin Sole 
#                          "Northern Rock Sole" = 616392, # Species	Lepidopsetta polyxystra Orr & Matarese, 2000 – northern rock sole, limande du nord, Northern Rock Sole
#                          "Southern Rock Sole" = 172917, # Species	Lepidopsetta bilineata (Ayres, 1855) – rock sole, fausse limande du Pacifique, Rock Sole
#                          "Flathead Sole" = 172875, # Species	Hippoglossoides elassodon Jordan & Gilbert, 1880 – flathead sole, Flathead Sole, plie à tête plate
#                          "Bering Flounder" = 172876, # Species	Hippoglossoides robustus Gill & Townsend, 1897 – Bering flounder, Bering Flounder, plie de Béring
#                          "Alaska Plaice" = 172901, # Species	Pleuronectes quadrituberculatus Pallas, 1814 – Alaska plaice, Alaska Plaice
#                          "Greenland Turbot" = 172930, #  Species	Reinhardtius hippoglossoides (Walbaum, 1792) – Greenland halibut, platija negra, Greenland turbot, Newfoundland turbot, turbot, greeenland halibut, flétan du Groenland, Greenland Halibut
#                          "Arrowtooth Flounder" = 172862, # Species	Atheresthes stomias (Jordan & Gilbert, 1880) – arrowtooth flounder, Arrowtooth Flounder, plie à grande bouche 
#                          "Kamchatka Flounder" = 172861, #  Species	Atheresthes evermanni Jordan & Starks, 1904 – Kamchatka flounder, Kamchatka Flounder
#                          "Pacific Halibut" = 172932) #Species: Hippoglossus stenolepis Schmidt, 1904 – valid)





####### Footnotes #########

Footnotes.list<-list("ExOfStandardFt" = "Wow, this project is so cool!")

# 
# 
# minyr <- min(dat$YEAR)
# dat<-dat[dat$YEAR %in% minyr:maxyr, ]
# dat$COMMON_NAME<-dat$COMMON
# dat$STATIONID<-dat$STATION
# 
# # Time
# dates<-strsplit(x = dat$DATETIME, split = " ")
# dat$dates<-sapply(dates, "[", 1 )
# 
# dat$DAY<-as.numeric(substr(x = as.character(dat$dates), 
#                            start = 4, stop = 5))
# dat$MONTH<-as.numeric(substr(x = as.character(dat$dates), 
#                              start = 1, stop = 2))
# 
# 
# months.words<-c("January", "February",	"March", "April", 
#                 "May", "June", "July", "August", 
#                 "September", "October", "November", "December")
# 
# 
# 
# load(file = "./data/specieslistinTSN.rdata")
# reftable<-spp.cat2$reftable
# reftable<-dplyr::rename(reftable, 
#                         "SID" = "SID_orig")
# dat<-left_join(dat, reftable, "SID")
# 
# 
# dat$PERFORMANCE<-0 # TOLEDO
# dat$HAULTYPE<-3 # TOLEDO
# 
# dat<-dat[dat$PERFORMANCE == 0 & dat$HAULTYPE == 3, ]
# spp.tsn.list<-spp.cat2$tsn.list
# dat.maxyr<-dat[dat$YEAR %in% maxyr,]


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

########*** Any BS ##########
if (SRVY %in% c("EBS", "NEBS", "NBS")) {
  
    sectname<-"EBS-BTS-Report"
    
    # STRAT <- c(10,20,31,32,41,42,43,50,61,62,82,90)
    
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
    
  ########*** *** EBS ##############
  if (SRVY %in% "EBS") {
    
    SURVEY<-"eastern Bering Sea"
    map.area<-"bs.south"
    SRVY1<-"EBS"
    
    reg_dat <- get_base_layers(select.region = map.area, set.crs = "auto")
    
    extrap.box = c(xmn = -179.5, xmx = -157, 
                   ymn = 54, ymx = 63)
    
    STRAT <- c(90, 82, 62, 43, 61, 41, 20, 42, 32, 50, 31, 10)

  ######*** *** NEBS###########
  } else if (SRVY %in% "NEBS") {
    
    SURVEY<-"northern and eastern Bering Sea"
    map.area<-"bs.all"
    SRVY1 <- c("EBS", "NBS")
    
    reg_dat <- get_base_layers(select.region = map.area, set.crs = "auto")
    
    extrap.box = c(xmn = -179.5, xmx = -157, 
                   ymn = 54, ymx = 68)
    
    STRAT <- c(90, 82, 62, 43, 61, 41, 20, 42, 32, 50, 31, 10, #EBS
              81, 70, 71) # NBS
    
  
  ######*** *** NBS###########
  } else if (SRVY %in% "NBS") {
    
    SURVEY<-"northern Bering Sea"
    map.area <- "bs.north" # "bs.north"
    SRVY1 <- "NBS"
    
    reg_dat_s <- akgfmaps::get_base_layers(select.region = "bs.south", set.crs = "auto")
    reg_dat_a <- akgfmaps::get_base_layers(select.region = "bs.all", set.crs = "auto")
    
    library(raster)
    reg_dat <- reg_dat_a
    reg_dat$survey.area <- intersect(reg_dat_s$survey.area, 
                                       reg_dat_a$survey.area)$geometry[2]
    reg_dat$place.labels$region <- "bs.north"
    reg_dat$plot.boundary[2,] <- reg_dat_s$plot.boundary[2,]
    reg_dat$lat.breaks <- c(56, 58, 60, 62, 64, 66)
    
    extrap.box <- c(xmn = -179.5, xmx = -157, 
                    ymn = 63, ymx = 68)
    
    STRAT <- c(81, 70, 71) # NBS
    
  }

    
    # First read in the shapefile, using the path to the shapefile and the shapefile name minus the
    # extension as arguments
    surveygrid_shp00 <- readOGR(dsn = here::here("shapefiles","STRATA", "EBS_NBS_2019.shp"))
    
    proj4string(surveygrid_shp00) <- crs(reg_dat$akland)
    
    # surveygrid_shp0 <- sp::spTransform(x = surveygrid_shp00, 
    #                                    CRSobj = crs(reg_dat$akland), )
    
    surveygrid_shp <- fortify(surveygrid_shp00) # Next the shapefile has to be converted to a dataframe for use in ggplot2    
    
    
    placenames <- read.csv(file = system.file("data", 
                                              file = "placenames.csv", package = "akgfmaps", 
                                              mustWork = TRUE), stringsAsFactors = FALSE) %>% 
      transform_data_frame_crs(out.crs = sf::st_crs(reg_dat$survey.strata)) 
    
    placenames_n <- placenames %>% 
      dplyr::filter(region == "bs.all") %>% 
      dplyr::mutate(region = "bs.north") 

    # placenames_n[placenames_n$lab %in% "Alaska", c("x", "y")] <- 
        
    placenames <- rbind.data.frame(placenames, placenames_n)
    
    placenames <- placenames %>%
      dplyr::filter(region == map.area)
    
    # placenames <- placenames[!(placenames$lab %in% c("Pribilof Isl.", "St. Matthew")), ]
}

################## Load Design Based Estimates #################

##### *** Weight ######
df.ls<-list()

for (ii in 1:length(SRVY1)) {
  
  a<-list.files(path = here::here("data", "surveydesign", SRVY1[ii], "CPUE"), full.names = TRUE)
  if (length(grep(pattern = "_plusnw", x = a, ignore.case = T)) > 0) {
    a <- a[grep(pattern = "_plusnw", x = a)]
  }
  
  for (i in 1:length(a)){
    b <- read_csv(file = a[i])
    b <- janitor::clean_names(b)
    if (names(b)[1] %in% "x1"){
      b$x1<-NULL
    }
    b$file <- a[i]
    b$survey <- SRVY1[ii]
    df.ls[[i]]<-b
    names(df.ls)[i]<-a[i]
  }
}

dat_cpue<-SameColNames(df.ls)

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


# ######*** domain (survey area IDs)#########
# domain<-data.frame(stratum = unique(stratum$stratum))
# domain$region <- NA
# domain$region[domain$stratum %in% c(81, 70, 71)]<-"NBS"
# domain$region[domain$stratum %in% c(90, 82, 62, 43, 61, 41, 20, 42, 32, 50, 31, 10)]<-"EBS"
# domain$domain <- NA
# domain$domain[domain$stratum %in% c(10, 20)]<-"EBS Inner Shelf"
# domain$domain[domain$stratum %in% c(31, 32, 41, 42, 43, 82)]<-"EBS Middle Shelf"
# domain$domain[domain$stratum %in% c(50, 61, 62, 90)]<-"EBS Outer Shelf"

######*** stratum_area (survey area)#########
stratum_area <- stratum %>% 
  # dplyr::right_join(x = ., y = domain, by = "stratum") %>%
  filter(region == SRVY0 &
           year <= maxyr & 
           stratum < 100 &
           # !(stratum %in%  c(70, 71, 81)) &
           stratum %in% STRAT) %>% # TOLEDO
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
  dplyr::filter(abundance_haul == "Y" & 
                  performance >= 0 & 
                 haul_type == 3 &
                  survey_name == "Eastern Bering Sea Crab/Groundfish Bottom Trawl Survey" &
                  # region.x == SRVY0 &
                  !(is.null(stationid)) &
                  stratum %in% STRAT
                  ) 
haul_cruises$SRVY <- NA
# haul_cruises$SRVY[haul_cruises$stratum %in% c(90, 82, 62, 43, 61, 41, 20, 42, 32, 50, 31, 10)] <- "EBS" 
haul_cruises$SRVY[haul_cruises$survey_definition_id == 112] <- "EBS" 
# haul_cruises$SRVY[haul_cruises$stratum %in% c(81, 70, 71)] <- "NBS"
haul_cruises$SRVY[haul_cruises$survey_definition_id == 146] <- "NBS" 

######*** catch_haul_cruises, dat, catch_haul_cruises_maxyr, dat_maxyr#########
catch_haul_cruises<-dplyr::left_join(x = catch, 
                                            y = haul_cruises, 
                                            by = c("hauljoin", "cruisejoin")) 

dat <- catch_haul_cruises

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

########*** length##############
dat_length <- length %>% 
  dplyr::filter(#species_code %in% spp_code, 
                hauljoin %in% unique(haul_cruises$hauljoin)) 

dat_length <- dplyr::left_join(x = dat_length, 
                               y = haul_cruises %>% 
                                 dplyr::select(stratum, hauljoin, SRVY, year))

dat_length_maxyr <- dat_length %>% 
  dplyr::filter(year == maxyr)

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
                                          "17", "Length frequency estimated using size composition proportions from adjacent hauls with similar catch composition"), 
                                 ncol = 2, byrow = TRUE))

names(length_type) <- c("code", "description")
length_type$sentancefrag <- c("fork lengths",
                      "lengths from mideye to fork of the tail",
                      "lengths from the tip of snout to hypural plate",
                      "lengths from mideye to hypural plate",
                      "total lengths",
                      "snout to second dorsal lengths",
                      "lengths of carapace from back of right eye socket to the end of the carapace",
                      "carapace widths",
                      "head lengths",
                      "snout to anal fin origin lengths",
                      "mantle lengths",
                      "posterior of orbital to end of telson lengths",
                      "wingtip to wingtip lengths",
                      "outer tip of rostrum to end of telson lengths",
                      "modal lengths",
                      "frequency of lengths estimated using size composition proportions from adjacent hauls with similar catch composition")


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


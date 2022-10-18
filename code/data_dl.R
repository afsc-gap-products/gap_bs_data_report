#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Download data
#' start date: 2021-03-03
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---

# This has a specific username and password because I DONT want people to have access to this!
# source("C:/Users/emily.markowitz/Work/Projects/ConnectToOracle.R")
# source("C:/Users/emily.markowitz/Documents/Projects/ConnectToOracle.R")
source("Z:/Projects/ConnectToOracle.R")
 # source("C:/Users/nicole.charriere/Work/R_Projects/Connect_to_Oracle.R")

# I set up a ConnectToOracle.R that looks like this: 
#   
#   PKG <- c("RODBC")
# for (p in PKG) {
#   if(!require(p,character.only = TRUE)) {  
#     install.packages(p)
#     require(p,character.only = TRUE)}
# }
# 
# channel<-odbcConnect(dsn = "AFSC",
#                      uid = "USERNAME", # change
#                      pwd = "PASSWORD", #change
#                      believeNRows = FALSE)
# 
# odbcGetInfo(channel)

##################DOWNLOAD CPUE and BIOMASS EST##################################

locations<-c(
  ## BIOMASS
 "HAEHNR.biomass_ebs_plusnw",# "HAEHNR.biomass_ebs_plusnw_safe", # no longer used
 "HAEHNR.biomass_ebs_plusnw_grouped",
 "HAEHNR.biomass_nbs_safe", 

 ## CPUE
 # "HAEHNR.cpue_nbs",
 # "HAEHNR.cpue_ebs_plusnw",
 # "HAEHNR.cpue_ebs_plusnw_grouped",
 "EBSSHELF.EBSSHELF_CPUE",
 "NBSSHELF.NBS_CPUE",

  ## Size Comps - the extrapolated size distributions of each fish
 "HAEHNR.sizecomp_nbs_stratum",
 "HAEHNR.sizecomp_ebs_plusnw_stratum",
 "HAEHNR.sizecomp_ebs_plusnw_stratum_grouped",
  
  ## CRAB # lengthed
 "crab.ebscrab",
 "crab.ebscrab_nbs",
  
 ## Public data
  # 'RACEBASE_FOSS.racebase_public_foss',
  
  # #General Tables of data
  "RACEBASE.CATCH",
  # "RACE_DATA.HAULS", # For vessel net mens. codes
  "RACEBASE.HAUL",
  "RACE_DATA.V_CRUISES",
  "RACE_DATA.V_EXTRACT_FINAL_LENGTHS", # the number of fish physically by hand lengthed (not extrapolated into sizecomp); do not use "RACEBASE.LENGTH",
  "RACEBASE.SPECIMEN",
  "RACEBASE.STRATUM", # Do not use .STATION, it is out of date
  "RACEBASE.SPECIES",
  "RACEBASE.SPECIES_CLASSIFICATION",
  "RACE_DATA.LENGTH_TYPES",
  "RACE_DATA.VESSELS"
  )


#sinks the data into connection as text file
sink("./data/metadata.txt")

print(Sys.Date())

for (i in 1:length(locations)){
  print(locations[i])
  if (locations[i] == "RACEBASE.HAUL") { # that way I can also extract TIME

    a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
    
    a <- RODBC::sqlQuery(channel, paste0("SELECT ",
                                       paste0(names(a)[names(a) != "START_TIME"], sep = ",", collapse = " "),
                                       " TO_CHAR(START_TIME,'MM/DD/YYYY HH24:MI:SS') START_TIME  FROM ", 
                                       locations[i]))
  } else {
    a <- RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
  }
  write.csv(x = a, 
            paste0("./data/oracle/",
                   tolower(gsub(pattern = '\\"', 
                                replacement = "", 
                                x = strsplit(x = locations[i], 
                                             split = ".", 
                                             fixed = TRUE)[[1]][2], 
                                perl = TRUE)),
                   ".csv"))
  remove(a)
}

sink()

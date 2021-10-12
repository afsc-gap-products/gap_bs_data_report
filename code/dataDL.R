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
source("C:/Users/emily.markowitz/Documents/Projects/ConnectToOracle.R")

##################DOWNLOAD CPUE and BIOMASS EST##################################

locations<-c(
  # BIOMASS
  "HAEHNR.biomass_ebs_plusnw_safe", 
  "HAEHNR.biomass_ebs_plusnw", 
  "HAEHNR.biomass_ebs_plusnw_grouped", 
  #CPUE
  "HAEHNR.cpue_nbs", 
  "HAEHNR.cpue_ebs_plusnw", 
  "HAEHNR.cpue_ebs_plusnw_grouped",
  # Size Comps
  "HAEHNR.sizecomp_nbs_stratum", 
  "HAEHNR.sizecomp_ebs_plusnw_stratum", 
  "HAEHNR.sizecomp_ebs_plusnw_stratum_grouped",
  #General Tables of data
  "RACEBASE.CATCH", 
  "RACEBASE.HAULS", 
  "RACEBASE.HAUL", 
  "RACE_DATA.V_CRUISES",
  "RACEBASE.LENGTH", 
  "RACEBASE.SPECIMEN", 
  "RACEBASE.STRATUM", 
  "RACEBASE.STATIONS", 
  "RACEBASE.SPECIES", 
  "RACEBASE.SPECIES_CLASSIFICATION", 
  "RACE_DATA.RACE_SPECIES_CODES", 
  "RACE_DATA.VESSELS", 
  "RACE_DATA.TAXONOMIC_RANKS", 
  "RACE_DATA.SPECIES_TAXONOMIC", 
  # ADFG
  "RACEBASE.LENGTH_ADFG", 
  "RACEBASE.SPECIMEN_ADFG")


#sinks the data into connection as text file
sink("./data/metadata.txt")

print(Sys.Date())

for (i in 1:length(locations)){
  print(locations[i])
  a<-RODBC::sqlQuery(channel, paste0("SELECT * FROM ", locations[i]))
  write.csv(x=a, 
            paste0("./data/",
                   tolower(strsplit(x = locations[i], 
                                    split = ".", 
                                    fixed = TRUE)[[1]][2]),
                   ".csv"))
  remove(a)
}

sink()



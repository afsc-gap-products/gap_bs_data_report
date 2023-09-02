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

# DOWNLOAD CPUE and BIOMASS EST ------------------------------------------------

locations <- c(
 #  ## BIOMASS
 # "HAEHNR.biomass_ebs_plusnw",# "HAEHNR.biomass_ebs_plusnw_safe", # no longer used
 # "HAEHNR.biomass_ebs_plusnw_grouped",
 # "HAEHNR.biomass_nbs_safe",
 # 
 # ## CPUE
 # # "HAEHNR.cpue_nbs",
 # # "HAEHNR.cpue_ebs_plusnw",
 # # "HAEHNR.cpue_ebs_plusnw_grouped",
 # "EBSSHELF.EBSSHELF_CPUE",
 # "NBSSHELF.NBS_CPUE",
 # 
 #  ## Size Comps - the extrapolated size distributions of each fish
 # "HAEHNR.sizecomp_nbs_stratum",
 # "HAEHNR.sizecomp_ebs_plusnw_stratum",
 # "HAEHNR.sizecomp_ebs_plusnw_stratum_grouped",
 # 
 #  ## CRAB # lengthed
 # "crab.ebscrab",
 # "crab.ebscrab_nbs",
 #  
 # ## Public data
 #  # 'RACEBASE_FOSS.racebase_public_foss',
 #  
 #  # #General Tables of data
 #  "RACEBASE.CATCH",
 #  # "RACE_DATA.HAULS", # For vessel net mens. codes
 #  "RACEBASE.HAUL",
 #  "RACE_DATA.V_CRUISES",
 #  "RACE_DATA.V_EXTRACT_FINAL_LENGTHS", # the number of fish physically by hand lengthed (not extrapolated into sizecomp); do not use "RACEBASE.LENGTH",
 #  "RACEBASE.SPECIMEN",
 #  "RACEBASE.STRATUM", # Do not use .STATION, it is out of date
 #  "RACEBASE.SPECIES",
 #  "RACEBASE.SPECIES_CLASSIFICATION",
 #  "RACE_DATA.LENGTH_TYPES",
 #  "RACE_DATA.VESSELS", 
  
 "GAP_PRODUCTS.OLD_STATION",
 "GAP_PRODUCTS.AKFIN_AGECOMP",
 "GAP_PRODUCTS.AKFIN_AREA",
 "GAP_PRODUCTS.AKFIN_BIOMASS",
 "GAP_PRODUCTS.AKFIN_CATCH",
 "GAP_PRODUCTS.AKFIN_CPUE",
 "GAP_PRODUCTS.AKFIN_CRUISES",
 "GAP_PRODUCTS.AKFIN_HAUL",
 "GAP_PRODUCTS.AKFIN_LENGTH",
 "GAP_PRODUCTS.AKFIN_METADATA_COLUMN",
 "GAP_PRODUCTS.AKFIN_SIZECOMP",
 "GAP_PRODUCTS.AKFIN_SPECIMEN",
 "GAP_PRODUCTS.AKFIN_STRATUM_GROUPS",
 "GAP_PRODUCTS.AKFIN_SURVEY_DESIGN",
 "GAP_PRODUCTS.AKFIN_TAXONOMICS_WORMS",
 # "RACE_DATA.LENGTH_TYPES",
 # "NBSSHELF.NBS_CPUE", # CPUE
 "crab.gap_ebs_nbs_abundance_biomass", # Biomass
 "crab.gap_ebs_nbs_crab_cpue", # CPUE
 "crab.ebscrab", # length data
 "crab.ebscrab_nbs", # length data,
 "RACE_DATA.LENGTH_TYPES"
  )


#sinks the data into connection as text file
# sink("./data/metadata.txt")

print(Sys.Date())

error_loading <- c()
for (i in 1:length(locations)){
  print(locations[i])
  
  a <- RODBC::sqlQuery(channel = channel, 
                               query = paste0("SELECT *
    FROM ", locations[i], "
    FETCH FIRST 1 ROWS ONLY;"))
  
  end0 <- c()
  if ("SURVEY_DEFINITION_ID" %in% names(a)) {
    end0 <- c(end0, "SURVEY_DEFINITION_ID IN (143, 98)")
  }
  if ("YEAR" %in% names(a) & locations[i] != "GAP_PRODUCTS.AKFIN_BIOMASS") {
    end0 <- c(end0, paste0("YEAR IN (",maxyr,", ", compareyr, ")"))
  }
  end0 <- ifelse(is.null(end0), "", paste0(" WHERE ", paste0(end0, collapse = " AND ")))
  
  start0 <- ifelse(!("START_TIME" %in% names(a)), 
                    "*", 
                    paste0(paste0(names(a)[names(a) != "START_TIME"], sep = ",", collapse = " "),
    " TO_CHAR(START_TIME,'MM/DD/YYYY HH24:MI:SS') START_TIME "))

  a <- RODBC::sqlQuery(channel, paste0("SELECT ", start0, " FROM ", locations[i], end0, "; "))
  
  if (is.null(nrow(a))) { # if (sum(grepl(pattern = "SQLExecDirect ", x = a))>1) {
    error_loading <- c(error_loading, locations[i])
  } else {
  write.csv(x = a, 
            here::here("data","oracle",
                       paste0(tolower(gsub(pattern = '.', 
                                    replacement = "_", 
                                    x = locations[i], 
                                    fixed = TRUE)),
                   ".csv")))
    }
  remove(a)
}
error_loading
# sink()
error_loading
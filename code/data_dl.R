#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Download data
#' start date: 2021-03-03
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---

# This has a specific username and password because I DONT want people to have access to this!
source("Z:/Projects/ConnectToOracle.R")
channel0 <- channel
channel <- channel_products

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
 "GAP_PRODUCTS.OLD_STATION",
 "GAP_PRODUCTS.AKFIN_AGECOMP",
 "GAP_PRODUCTS.AKFIN_AREA",
 "GAP_PRODUCTS.AKFIN_BIOMASS",
 "GAP_PRODUCTS.AKFIN_CATCH",
 "GAP_PRODUCTS.AKFIN_CPUE",
 "GAP_PRODUCTS.AKFIN_CRUISE",
 "GAP_PRODUCTS.AKFIN_HAUL",
 "GAP_PRODUCTS.AKFIN_METADATA_COLUMN",
 "GAP_PRODUCTS.AKFIN_SIZECOMP",
 "GAP_PRODUCTS.AKFIN_SPECIMEN",
 "GAP_PRODUCTS.AKFIN_STRATUM_GROUPS",
 "GAP_PRODUCTS.AKFIN_SURVEY_DESIGN",
 "GAP_PRODUCTS.AKFIN_TAXONOMICS_WORMS",
 "GAP_PRODUCTS.TEST_SPECIES_CLASSIFICATION"
 # "crab.gap_ebs_nbs_abundance_biomass", # Biomass
 # "crab.gap_ebs_nbs_crab_cpue", # CPUE
 # "crab.ebscrab", # length data
 # "crab.ebscrab_nbs", # length data
  )

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
  if ("YEAR" %in% names(a) & !(locations[i] %in% 
                               c("GAP_PRODUCTS.AKFIN_SIZECOMP", "GAP_PRODUCTS.AKFIN_BIOMASS", 
                                 "GAP_PRODUCTS.AKFIN_CRUISE", "GAP_PRODUCTS.AKFIN_CPUE",
                                 "GAP_PRODUCTS.AKFIN_HAUL")) ) {
    end0 <- c(end0, paste0("YEAR IN (",paste0(maxyr:compareyr0, collapse = ","), ")"))
  }
  
  if ("YEAR" %in% names(a) & locations[i] %in% c("GAP_PRODUCTS.AKFIN_CPUE")) {
    end0 <- c(end0, paste0("YEAR IN (",text_list(x = maxyr:(maxyr-9), sep = ", ", sep_last2 = ", ", sep_last = ", "), ")"))
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
            here::here("data",
                       paste0(tolower(gsub(pattern = '.', 
                                    replacement = "_", 
                                    x = locations[i], 
                                    fixed = TRUE)),
                   ".csv")))
    }
  remove(a)
}
error_loading



# pull data from RACE_DATA

locations <- c("RACE_DATA.V_EXTRACT_FINAL_LENGTHS", 
               "RACE_DATA.LENGTHS", # "GAP_PRODUCTS.AKFIN_LENGTH", # has extrapolated lengths, not how many were actually counted
                "RACE_DATA.HAULS", # need for manipulating RACE_DATA.LENGTHS
                "RACE_DATA.CRUISES", 
                "RACE_DATA.SURVEYS", 
                "RACE_DATA.SURVEY_DEFINITIONS", 
                "RACE_DATA.VESSELS", 
                "RACE_DATA.LENGTH_TYPES", 
                "RACE_DATA.EDIT_HAULS")

for (i in 1:length(locations)) {
a <- RODBC::sqlQuery(channel = channel0, # NOT RACEBASE.HAUL
                     query = paste0("SELECT *
FROM ",locations[i],"; ")) 
write.csv(x = a, 
          here::here("data",
                     paste0(tolower(gsub(pattern = '.', 
                                         replacement = "_", 
                                         x = locations[i], 
                                         fixed = TRUE)),
                            ".csv")))
}


# a <- RODBC::sqlQuery(channel = channel0, # NOT RACEBASE.HAUL
#                      query = paste0("SELECT  
# -- SD.REGION REGION,
# --        A.VESSEL_ID VESSEL,
#         A.CRUISE CRUISE,
# --        B.HAUL HAUL,
#         C.SPECIES_CODE SPECIES_CODE,
# --        C.SEX SEX,
#         SUM(C.FREQUENCY) FREQUENCY--,
# --        C.EDIT_LENGTH LENGTH,
# --        C.LENGTH_TYPE LENGTH_TYPE,
# --        C.LENGTH_SUBSAMPLE_TYPE SAMPLE_TYPE
#  FROM RACE_DATA.CRUISES A
#  JOIN RACE_DATA.SURVEYS S
#    ON (S.SURVEY_ID = A.SURVEY_ID)
#  JOIN RACE_DATA.SURVEY_DEFINITIONS SD
#    ON (SD.SURVEY_DEFINITION_ID = S.SURVEY_DEFINITION_ID)
#  JOIN RACE_DATA.HAULS B
#    ON (B.CRUISE_ID = A.CRUISE_ID)
#  JOIN RACE_DATA.LENGTHS C
#    ON (C.HAUL_ID = B.HAUL_ID)
# WHERE PERFORMANCE >= 0 
# AND HAUL_TYPE = 3
# AND CRUISE IN (202201, 202202, 202301, 202302)
# -- AND SPECIES_CODE IN (21740, 21720, 21741, 21721)
# AND REGION = 'BS'
#  GROUP BY SD.REGION,
# --          A.VESSEL_ID,
#           A.CRUISE,
# --          B.HAUL,
#           C.SPECIES_CODE--,
# --          C.SEX,
# --          C.EDIT_LENGTH,
# --          C.LENGTH_TYPE,
# --          C.LENGTH_SUBSAMPLE_TYPE;")) 
# write.csv(x = a, 
#           here::here("data","race_data_lengths_mod.csv"))

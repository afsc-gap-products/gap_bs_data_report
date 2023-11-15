
if (file.exists("Z:/Projects/ConnectToOracle.R")) {
  # This has a specific username and password because I DONT want people to have access to this!
  source("Z:/Projects/ConnectToOracle.R")
} else {
  # For those without a ConnectToOracle file
  channel <- odbcConnect(dsn = "AFSC", 
                         uid = rstudioapi::showPrompt(title = "Username", 
                                                      message = "Oracle Username", default = ""), 
                         pwd = rstudioapi::askForPassword("Enter Password"),
                         believeNRows = FALSE)
}

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
  "GAP_PRODUCTS.AKFIN_TAXONOMIC_CLASSIFICATION"
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

locations <- c(
  "crab.gap_ebs_nbs_abundance_biomass", # Biomass
  "crab.gap_ebs_nbs_crab_cpue", # CPUE
  "crab.ebscrab", # length data
  "crab.ebscrab_nbs", # length data
  "crab.nbs_size1mm_all_species", 
  "RACE_DATA.LENGTHS",   # "RACE_DATA.V_EXTRACT_FINAL_LENGTHS" also works, but is ephemeral # "GAP_PRODUCTS.AKFIN_LENGTH", # has extrapolated lengths, not how many were actually counted
  "RACE_DATA.EDIT_HAULS", 
  "race_data.edit_events",
  "RACE_DATA.HAULS", # need for manipulating RACE_DATA.LENGTHS
  "RACE_DATA.CRUISES", 
  "RACE_DATA.SURVEYS", 
  "RACE_DATA.SURVEY_DEFINITIONS", 
  "RACE_DATA.VESSELS", 
  "RACE_DATA.LENGTH_TYPES")

for (i in 1:length(locations)) {
  print(locations[i])
  a <- RODBC::sqlQuery(channel = channel, # NOT RACEBASE.HAUL
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

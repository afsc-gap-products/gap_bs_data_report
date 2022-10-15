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
  # # BIOMASS
# "HAEHNR.biomass_ebs_plusnw_safe", # no longer used
 "HAEHNR.biomass_ebs_plusnw",
 "HAEHNR.biomass_ebs_plusnw_grouped",
 "EBSCRAB.EBSCRAB_ABUNDANCE_BIOMASS",
 "CRAB.NBS_BK_BIO_NEWTS_V2022",
 "CRAB.NBS_CO_MATFEM_BIO_NEWTS_V2022",
 "CRAB.NBS_CB_MATFEM_BIO_NEWTS_V2022",
 "CRAB.NBS_RK_MALE_BIO_NEWTS_V2022",
 "CRAB.NBS_RK_FEMALE_BIO_NEWTS_V2022",
 
 # NBS data provided in .csvs over email
 
  # # CPUE
 # "HAEHNR.cpue_nbs",
 # "HAEHNR.cpue_ebs_plusnw",
 # "HAEHNR.cpue_ebs_plusnw_grouped",
 "EBSSHELF.EBSSHELF_CPUE",
 "NBSSHELF.NBS_CPUE",
 # "EBSCRAB.EBSCRAB_CPUE",  # pulled uniquely below
 "CRAB.NBS_BK_CPUENUM_SIZEGROUP",
 "CRAB.NBS_BK_CPUEWGT_SIZEGROUP",
 "CRAB.NBS_CB_CPUENUM_SIZEGROUP",
 "CRAB.NBS_CB_CPUEWGT_SIZEGROUP",
 "CRAB.NBS_CO_CPUENUM_SIZEGROUP",
 "CRAB.NBS_CO_CPUEWGT_SIZEGROUP",
 "CRAB.NBS_RK_CPUENUM_SIZEGROUP_FEMALES",
 "CRAB.NBS_RK_CPUENUM_SIZEGROUP_MALES",
 "CRAB.NBS_RK_CPUEWGT_SIZEGROUP_FEMALES",
 "CRAB.NBS_RK_CPUEWGT_SIZEGROUP_MALES",
 # NBS data provided in .csvs over email
 
  # # Size Comps - the extrapolated size distributions of each fish
 "HAEHNR.sizecomp_nbs_stratum",
 "HAEHNR.sizecomp_ebs_plusnw_stratum",
 "HAEHNR.sizecomp_ebs_plusnw_stratum_grouped",
  
  # # CRAB
 "crab.ebscrab",
 "crab.ebscrab_nbs",
# "crab.co_size1_cpuenum",
# "crab.co_size1_cpuewgt",
# "crab.cb_size1_cpuenum",
# "crab.cb_size1_cpuewgt",
# "crab.rk_size1_cpuenum_leg1",
# "crab.rk_size1_cpuewgt_leg1",
# # "crab.rk_size1_cpuenum_leg2",
# # "crab.rk_size1_cpuewgt_leg2",
# "crab.rk_size1_cpuenum_leg3",
# "crab.rk_size1_cpuewgt_leg3",
# "crab.bk_size1_cpuenum",
# "crab.bk_size1_cpuewgt",
# "crab.co_size1_cpuenum_nbs",
# "crab.co_size1_cpuewgt_nbs",
# # "crab.co_weight_mt_size1_union_nbs",
# # "crab.co_num_size1_union_nbs",
# "crab.bk_size1_cpuenum_nbs",
# "crab.bk_size1_cpuewgt_nbs",
# # "crab.bk_weight_mt_size1_union_nbs",
# # "crab.bk_num_size1_union_nbs",
# "crab.rk_size1_cpuenum_nbs",
# "crab.rk_size1_cpuewgt_nbs",
  
  # '"RACEBASE_FOSS"."racebase_public_foss"',
  
  # #General Tables of data
  "RACEBASE.CATCH",
  # "RACE_DATA.HAULS", # For vessel net mens. codes
  "RACEBASE.HAUL",
  "RACE_DATA.V_CRUISES",
  "RACE_DATA.V_EXTRACT_FINAL_LENGTHS", # the number of fish physically by hand lengthed (not extrapolated into sizecomp)
  # "RACEBASE.LENGTH",
  "RACEBASE.SPECIMEN",
  "RACEBASE.STRATUM",
  # "RACEBASE.STATIONS",
  "RACEBASE.SPECIES",
  "RACEBASE.SPECIES_CLASSIFICATION",
  "RACE_DATA.LENGTH_TYPES",
  # "RACE_DATA.RACE_SPECIES_CODES",
  "RACE_DATA.VESSELS"#,
  # "RACE_DATA.TAXONOMIC_RANKS",
  # "RACE_DATA.SPECIES_TAXONOMIC"#,
  # # ADFG
  # "RACEBASE.LENGTH_ADFG",
  # "RACEBASE.SPECIMEN_ADFG"
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

# Pull crazy crab data

a <- RODBC::sqlQuery(channel, paste0("select survey_year,species_code,gis_station, mid_latitude, mid_longitude, hauljoin, vessel,
sum (CASE
        WHEN ((size_group = 'MALE_TOTAL') or (size_group = 'FEMALE_TOTAL'))  
        THEN (crab_cpuewgt_mt)
       ELSE 0
       END) cpue_kgha,
round ( sum (CASE
        WHEN ((size_group = 'MALE_TOTAL') or (size_group = 'FEMALE_TOTAL'))  
        THEN (crab_cpuenum)
       ELSE 0
       END)) cpue_noha
from ebscrab.ebscrab_cpue
where survey_year > 2009 and district_code = 'ALL'
group by survey_year,species_code,gis_station, mid_latitude, mid_longitude, hauljoin, vessel
order by survey_year,gis_station; "))

write.csv(x = a, 
          paste0("./data/oracle/ebscrab_cpue_modified.csv"))

# a <- RODBC::sqlQuery(channel, "SELECT DISTINCT SIZE_GROUP FROM EBSCRAB.EBSCRAB_CPUE;")



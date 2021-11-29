#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
#' purpose: Run Scripts and R Markdown Files
#' start date: 2021-09-01
#' date modified: 2021-09-01                                          # CHANGE
#' Notes:                                                               # CHANGE
#' ---

# START ------------------------------------------------------------------------

# *** REPORT KNOWNS ------------------------------------------------------------
report_title <- "Data Report" # Fake until I get a better idea of how to automate something down the line
workfaster <- TRUE # an attempt to satisfy limited patience
refcontent <- TRUE # produce extra summary text and tables for each spp to help with writing
googledrive_dl <- TRUE # redownload google drive tables and docs?
indesign_flowin <- FALSE
pres_img <- FALSE
usePNGPDF <- "png"

# maxyr <- 2016 # or the year of the report, for example
# compareyr <- 2015
# # compareyr_nbs <- 2010 #will this always be the same?
# SRVY<-"EBS"
# ref_compareyr_ebs <- "@RN976" # temp
# ref_compareyr_nbs <- "@RN909" # temp

# maxyr <- 2017 # or the year of the report, for example
# compareyr <- 2010
# # compareyr_nbs <- 2010 #will this always be the same?
# SRVY<-"NEBS"
ref_compareyr_ebs <- "@RN976"
ref_compareyr_nbs <- "@RN909"
dir_googledrive <- "1vtwfDwRprFml_5wN_WkeVViynwGhC8fe" # https://drive.google.com/drive/folders/1vtwfDwRprFml_5wN_WkeVViynwGhC8fe?usp=sharing

# maxyr <- 2018 # NOTE RAPID RESPONCE
# compareyr <- 2016
# SRVY<-"NEBS"
# ref_compareyr_ebs <- "@RN976" # CHANGE
# ref_compareyr_nbs <- NA # CHANGE
# dir_googledrive <- "1W8VfqBF9j48vk0GpFLyg5cZGzuHlelAy" # https://drive.google.com/drive/folders/1W8VfqBF9j48vk0GpFLyg5cZGzuHlelAy?usp=sharing

# maxyr <- 2019 
# compareyr <- 2017
# SRVY<-"NEBS" 
# crabretow <- TRUE
# dir_googledrive <- "1HpuuIIp6piS3CieRJR81-8hVJ3QaKOU" # https://drive.google.com/drive/folders/1HpuuIIp6piS3CieRJR81-8hVJ3QaKOU-?usp=sharing

maxyr <- 2021
compareyr <- 2019
SRVY<-"NEBS"
# crabretow <- TRUE
# study1530 <- TRUE
dir_googledrive <- "1i3NRmaAPpIYfMI35fpJCa-8AjefJ7J7X" # https://drive.google.com/drive/folders/1i3NRmaAPpIYfMI35fpJCa-8AjefJ7J7X?usp=sharing

# *** SIGN INTO GOOGLE DRIVE----------------------------------------------------

googledrive::drive_deauth()
googledrive::drive_auth()
1

# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

source('./code/directories.R')

source('./code/functions.R')

# source('./code/dataDL.R')

source('./code/data.R')


# *** REPORT TITLE -------------------------------------------------------------
report_title <- paste0('Data Report: ',maxyr,' ', NMFSReports::TitleCase(SURVEY),
                       ' continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna')
report_authors <- 'L. Britt, E. H. Markowitz, E. J. Dawson, N. Charriere, S. Rohan, B. Prohaska, R. Haehn, and D. Stevenson'
# report_yr <- maxyr 


# RUN EACH REPORT SECTION ------------------------------------------------------

# TOLEDO
# create file that checks for errors in RMDs
# https://github.com/NOAA-EDAB/esp_data_aggregation/blob/main/R-scripts/test_rmds.R
# https://github.com/NOAA-EDAB/esp_data_aggregation/blob/main/R-scripts/render%20dev%20report%20with%20errors.R

# *** *** Figures and Tables ------------------------
# - run figures and tables before each chapter so everything works smoothly
  
  report_spp1 <- add_report_spp(spp_info = spp_info, 
                                spp_info_codes = "species_code", 
                                report_spp = report_spp, 
                                report_spp_col = "order", 
                                report_spp_codes = "species_code", 
                                lang = FALSE)

if (FALSE) {
  # cnt_chapt<-auto_counter(cnt_chapt)
  cnt_chapt_content<-"001"
  filename0<-paste0(cnt_chapt, "_")
  rmarkdown::render(paste0(dir_code, "/figtab.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))

  
  for (jj in 1:length(unique(report_spp1$file_name))) {
    
    print(paste0(jj, " of ", length(unique(report_spp1$file_name))))
    start_time <- Sys.time()
    # cnt_chapt<-auto_counter(cnt_chapt)
    # cnt_chapt_content<-"001"
    filename00<-paste0(cnt_chapt, "_spp_")
    rmarkdown::render(paste0(dir_code, "/figtab_spp.Rmd"),
                      output_dir = dir_out_ref,
                      output_file = paste0(filename00, cnt_chapt_content, "_", 
                                           unique(report_spp1$file_name)[jj],".docx"))
    end_time <- Sys.time()
    print(paste0(end_time - start_time))
  }
  
  save(list_figures,
       file=paste0(dir_out_figures, "/report_figures.rdata"))
  
  save(list_tables,
       file=paste0(dir_out_tables, "/report_tables.rdata"))
  
}

load(file = paste0(dir_out_figures, "/report_figures.rdata"))
load(file = paste0(dir_out_tables, "/report_tables.rdata"))

# *** *** 01 - Abstract ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_abstract_")
rmarkdown::render(paste0(dir_code, "/01_abstract.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 02 - Introduction ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_introduction_")
rmarkdown::render(paste0(dir_code, "/02_introduction.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 03 - History ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_history_")
rmarkdown::render(paste0(dir_code, "/03_history.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 04 - Methods ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_methods_")
rmarkdown::render(paste0(dir_code, "/04_methods.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 05 - Results ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_results_")
rmarkdown::render(paste0(dir_code, "/05_results.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 06 - Results_spp ------------------------
report_spp1 <- add_report_spp(spp_info = spp_info, 
                              spp_info_codes = "species_code", 
                              report_spp = report_spp, 
                              report_spp_col = "order", 
                              report_spp_codes = "species_code", 
                              lang = TRUE)

for (jj in 1:length(unique(report_spp1$file_name))) {
  
  print(paste0(jj, " of ", length(unique(report_spp1$file_name))))
  
  cnt_chapt_content<-auto_counter(cnt_chapt_content)
  filename00<-paste0(cnt_chapt, "_spp_")
  rmarkdown::render(paste0(dir_code, "/06_results_spp.Rmd"),
                    output_dir = dir_out_chapters,
                    output_file = paste0(filename00, cnt_chapt_content, "_", 
                                         unique(report_spp1$file_name)[jj],".docx"))
}

# *** *** 07 - Results_crabretow ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_results_crabretow_")
rmarkdown::render(paste0(dir_code, "/07_results_crabretow.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 08 - Results_15-30study ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_results_15-30study_")
rmarkdown::render(paste0(dir_code, "/08_results_15-30study.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 09 - Results_discussion ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_results_discussion_")
rmarkdown::render(paste0(dir_code, "/09_results_discussion.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 10 - Endmatter ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_endmatter_")
rmarkdown::render(paste0(dir_code, "/10_endmatter.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


# *** *** 11 - Presentation ------------------------


# *** *** *** - Figures and Tables ------------------------
# - run figures and tables before each chapter so everything works smoothly
if (FALSE) {
  
    report_spp1 <- add_report_spp(spp_info = spp_info, 
                                spp_info_codes = "species_code", 
                                report_spp = report_spp, 
                                report_spp_col = "order", 
                                report_spp_codes = "species_code", 
                                lang = TRUE)
  
  # cnt_chapt<-auto_counter(cnt_chapt)
  cnt_chapt_content<-"001"
  filename0<-paste0(cnt_chapt, "_")
  rmarkdown::render(paste0(dir_code, "/figtab_pres.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  

  
  
  for (jj in 1:length(unique(report_spp1$file_name))) {
    
    print(paste0(jj, " of ", length(unique(report_spp1$file_name))))
    start_time <- Sys.time()
    # cnt_chapt<-auto_counter(cnt_chapt)
    # cnt_chapt_content<-"001"
    filename00<-paste0(cnt_chapt, "_spp_")
    rmarkdown::render(paste0(dir_code, "/figtab_spp_pres.Rmd"),
                      output_dir = dir_out_ref,
                      output_file = paste0(filename00, cnt_chapt_content, "_", 
                                           unique(report_spp1$file_name)[jj],".docx"))
    end_time <- Sys.time()
    print(paste0(end_time - start_time))
  }
  
  save(list_figures,
       file=paste0(dir_out_figures, "/report_figures_pres.rdata"))
  
  save(list_tables,
       file=paste0(dir_out_tables, "/report_tables_pres.rdata"))
  
}

load(file = paste0(dir_out_tables, "/report_figures_pres.rdata"))
load(file = paste0(dir_out_tables, "/report_tables_pres.rdata"))

# subtitle
cruises_maxyr0  <- haul_cruises_vess_maxyr %>% 
  dplyr::filter(SRVY %in% c("NBS", "EBS")) %>% 
  dplyr::select("year", "survey_name", "vessel", "vessel_name", 
                "vessel_ital", "SRVY", "SRVY_long", 
                "start_date_cruise", "end_date_cruise", 
                "start_date_haul", "end_date_haul") %>% 
  unique() %>% 
  group_by(year, survey_name, vessel, vessel_name, 
           vessel_ital, SRVY, SRVY_long) %>% 
  dplyr::summarise(start_date_cruise = min(start_date_cruise), 
                   end_date_cruise = max(end_date_cruise), 
                   start_date_haul = min(start_date_haul), 
                   end_date_haul = max(end_date_haul)) %>% 
  dplyr::arrange(start_date_cruise)

str <- paste0(
  format(min(cruises_maxyr0$start_date_haul), format = "%B %d"), 
  " to ", 
  format(max(cruises_maxyr0$end_date_haul), format = "%B %d, %Y") ) 

cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_presentation_")
rmarkdown::render(paste0(dir_code, "/11_presentation.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".pptx"))



# SAVE OTHER OUTPUTS -----------------------------------------------------------

# save(list_figures,
#      file=paste0(dir_out_figures, "/report_figures.rdata"))
# 
# save(list_tables,
#      file=paste0(dir_out_tables, "/report_tables.rdata"))

save(list_equations,
     file=paste0(dir_out_tables, "/report_equations.rdata"))

# MAKE MASTER DOCX -------------------------------------------------------------

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one

# SAVE METADATA ----------------------------------------------------------------

con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append=TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log


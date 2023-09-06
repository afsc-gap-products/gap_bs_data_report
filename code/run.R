#' ---------------------------------------------
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: E. H. Markowitz, E. J. Dawson
#' purpose: Run Scripts and R Markdown Files
#' start date: 2021-09-01
#' date modified: 2022-09-01                                          # CHANGE
#' Notes:  
#' create file that checks for errors in RMDs:
#'    https://github.com/NOAA-EDAB/esp_data_aggregation/blob/main/R-scripts/test_rmds.R
#'    https://github.com/NOAA-EDAB/esp_data_aggregation/blob/main/R-scripts/render%20dev%20report%20with%20errors.R
#'    # rmarkdown::render(input = "./notforgit/test.Rmd",
#'                   output_dir = dir_out_chapters,
#'                   output_file = "test.docx")
#' ---------------------------------------------

# *** Report knowns ------------------------------------------------------------
report_title <- "data" 

refcontent <- FALSE # produce extra summary text and tables for each spp to help with writing
access_to_internet  <- TRUE # redownload google drive tables and docs?
pres_img <- FALSE
font0 <- "Arial Narrow"
font_size0 <- 12

# maxyr <- 2017 # or the year of the report, for example
# compareyr <- 2010
# strat_yr <- 2010
# SRVY<-"NEBS"
# ref_compareyr <- "@RN976"
# # ref_compareyr <- "@RN909"
# # dir_googledrive <- "1vtwfDwRprFml_5wN_WkeVViynwGhC8fe" # https://drive.google.com/drive/folders/1vtwfDwRprFml_5wN_WkeVViynwGhC8fe?usp=sharing

# maxyr <- 2018 # NOTE RAPID RESPONCE
# strat_yr <- 2019
# compareyr <- 2016
# SRVY<-"EBS"
# ref_compareyr <- "@RN976" # CHANGE
# dir_googledrive <- "1W8VfqBF9j48vk0GpFLyg5cZGzuHlelAy" # https://drive.google.com/drive/folders/1W8VfqBF9j48vk0GpFLyg5cZGzuHlelAy?usp=sharing

# maxyr <- 2019
# compareyr <- 2017
# strat_yr <- 2019
# SRVY<-"NEBS"
# ref_compareyr <- "@Lauth2019" # CHANGE
# dir_googledrive <- "1HpuuIIp6piS3CieRJR81-8hVJ3QaKOU-" # https://drive.google.com/drive/folders/1HpuuIIp6piS3CieRJR81-8hVJ3QaKOU-?usp=sharing
# ref_compareyr_ebs <- "@RN976" # check
# ref_compareyr_nbs <- "@RN909" # check

# maxyr <- 2021
# compareyr <- 2019
# strat_yr <- 2019
# SRVY <- "NEBS" # "NEBS"
# ref_compareyr <- "@2019NEBS2022" # CHANGE
# dir_googledrive <- "1i3NRmaAPpIYfMI35fpJCa-8AjefJ7J7X" # https://drive.google.com/drive/folders/1i3NRmaAPpIYfMI35fpJCa-8AjefJ7J7X?usp=sharing
# ref_compareyr_ebs <- "@RN976" # community report
# ref_compareyr_nbs <- "@RN909" # community report
# id_googledrive_comm <- googledrive::as_id("1bqXIlM9Er8MeITCkRQy8D52Yd46CLz3f")

maxyr <- 2022
compareyr <- 2021
strat_yr <- 2022
SRVY<-"NEBS"
ref_compareyr <- ref_compareyr_ebs <- "@2021NEBS2022" # CHANGE
# dir_googledrive <- "1x50OKqAyLcqNLYhjQNX84dHLXHcTBnaD" # https://drive.google.com/drive/folders/1x50OKqAyLcqNLYhjQNX84dHLXHcTBnaD
ref_compareyr_nbs <- "@RN909" # community report
# dir_googledrive_comm <- "1uZy1uDB_poml2KKX3R_Qv8qrWG1WLewE" # "https://drive.google.com/drive/folders/1uZy1uDB_poml2KKX3R_Qv8qrWG1WLewE")

# maxyr <- 2023
# compareyr <- 2022
# strat_yr <- 2022
# SRVY<-"NEBS"
# ref_compareyr <- ref_compareyr_ebs <- "@2022NEBS2023" # CHANGE
dir_googledrive <- "https://drive.google.com/drive/folders/19ttU1_VAlos_3KKjiRqfcMF-k1cpaENN" 
dir_googledrive_comm <- "https://drive.google.com/drive/folders/1gJYWYWzU8Iwi7gQmoSpCFVfxsoV20P2v"

googledrive::drive_deauth()
googledrive::drive_auth()
2

# *** Source support scripts ---------------------------------------------------

source(here::here("code","directories.R"))

source(here::here("code","functions.R"))

# source('./code/data_dl.R') # Run when there is new data!

source(here::here("code","data.R"))

# SAVE METADATA ----------------------------------------------------------------
rmarkdown::render(paste0(here::here("code","README.Rmd")),
                  output_dir = here::here(),
                  output_file = paste0("README.md"))

# Figures and Tables -----------------------------------------------------------

# General figures
rmarkdown::render(paste0(dir_code, "/figtab.Rmd"),
                  output_dir = dir_out_ref,
                  output_file = paste0(cnt_chapt_content, ".docx"))

# Species figures
comb <- report_spp1 %>% dplyr::filter(!is.na(order)) %>% dplyr::select(file_name) %>% unlist() %>% unique()
for (jj in 1:length(comb)) {
  print(paste0(jj, " of ", length(comb), ": ", comb[jj]))
  rmarkdown::render(paste0(dir_code, "/figtab_spp.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(cnt_chapt_content, "_", comb[jj],".docx"))
}

# Appendix 
rmarkdown::render(paste0(dir_code, "/figtab_appendix.Rmd"),
                  output_dir = dir_out_ref,
                  output_file = paste0(cnt_chapt_content, ".docx"))

# Run data report --------------------------------------------------------------
report_title <- "data" 

rmarkdown::render(input = paste0(dir_code, "00_data_report.Rmd"), 
                  output_format = "officedown::rdocx_document", 
                  output_dir = dir_out_chapters, 
                  output_file = paste0("00_data_report_", maxyr, ifelse(refcontent, "_ref", ""), ".docx"))

# for (abcd in LETTERS[1:(ifelse(SRVY == "NEBS", 4, 2))]) {
#   rmarkdown::render(input = paste0(dir_code, "00_data_report_appendix.Rmd"),
#                   output_format = "officedown::rdocx_document",
#                   output_dir = dir_out_chapters,
#                   output_file = paste0("00_data_report_", maxyr, "_app", abcd, ".docx"))
# }

# COMMUNITY HIGHLIGHTS ---------------------------------------------------------

report_title <- "community"
source(here::here("code","directories.R"))
dir_googledrive <- dir_googledrive_comm
source(here::here("code","data.R"))

## Species figures -------------------------------------------------------------

comb <- report_spp1 %>% dplyr::filter(!is.na(order)) %>% dplyr::select(file_name) %>% unlist() %>% unique()
for (jj in 1:length(comb)) {
  print(paste0(jj, " of ", length(comb), ": ", comb[jj]))
  rmarkdown::render(paste0(dir_code, "/figtab_spp.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(cnt_chapt_content, "_", comb[jj],".docx"))
}

## Run community highlights report -----------------------------------------------

rmarkdown::render(input = paste0(dir_code, "00_community_report.Rmd"), 
                  output_format = "officedown::rdocx_document", 
                  output_dir = dir_out_chapters, 
                  output_file = paste0("00_community_report_", maxyr, ifelse(refcontent, "_ref", ""), ".docx"))

# PRESENTATION ------------------------------------------------------

# report_spp1 <- add_report_spp(spp_info = spp_info, 
#                               spp_info_codes = "species_code", 
#                               report_spp = report_spp, 
#                               report_spp_col = "order", 
#                               report_spp_codes = "species_code0", 
#                               lang = TRUE)

yrs <- sort(nbsyr, decreasing = FALSE)

cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_")
rmarkdown::render(paste0(dir_code, "/figtab_pres.Rmd"),
                  output_dir = dir_out_ref,
                  output_file = paste0(filename0, cnt_chapt_content, ".docx"))


for (jj in 1:length(unique(report_spp1$file_name))) {
  
  print(paste0(jj, " of ", length(unique(report_spp1$file_name)), ": ", unique(report_spp1$file_name)[jj]))
  SRVY1 <- c("NBS", "EBS")
  pcol <- viridis::mako(n = 2, begin = .2, end = .6, direction = -1) # TOLEDO - need to find out where this is outside of a function
  
  filename00<-paste0(cnt_chapt, "_spp_")
  rmarkdown::render(paste0(dir_code, "/figtab_spp_pres.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(filename00, cnt_chapt_content, "_", 
                                         unique(report_spp1$file_name)[jj],".docx"))
}

save(list_figures,
     file=paste0(dir_out_figures, "/report_figures_pres.rdata"))

save(list_tables,
     file=paste0(dir_out_tables, "/report_tables_pres.rdata"))

cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_presentation_")
rmarkdown::render(paste0(dir_code, "/11_presentation.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".pptx"))

# SAVE METADATA ----------------------------------------------------------------
con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append=TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log

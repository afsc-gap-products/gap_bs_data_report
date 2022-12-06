 

# Save figures and tables locally to working draft folder
file.copy(from = dir_out_figtab, 
          to = dir_out_todaysrun, 
          overwrite = TRUE, 
          recursive = TRUE)

# Run data report --------------------------------------------------------------

rmarkdown::render(input = paste0(dir_code, "00_full_report_data.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0("00_full_report_data_", maxyr, ifelse(refcontent, "_ref", ""), ".docx"))

# quarto::quarto_render(input = paste0(dir_code, "00_full_report_data.qmd"),
#                   output_file = paste0(dir_out_chapters, "00_full_report_data_", maxyr, ifelse(refcontent, "_ref", ""), ".docx"))

# COMMUNITY HIGHLIGHTS ---------------------------------------------------------

# *** Report knowns ------------------------------------------------------------

# report_title <- paste0(maxyr, ' Northern Bering Sea Groundfish and Crab Trawl Survey Highlights')
report_title <- "community" 
refcontent <- FALSE # produce extra summary text and tables for each spp to help with writing
googledrive_dl <- TRUE # redownload google drive tables and docs?
indesign_flowin <- FALSE
pres_img <- FALSE
usePNGPDF <- "png"
font0 <- "Arial Narrow"

SRVY <- "NEBS"

# # 2019
# report_yr <- maxyr <- 2019
# compareyr <- 2010
# ref_compareyr_ebs <- "@RN976" # check
# ref_compareyr_nbs <- "@RN909" # check

# # 2021
# report_yr <- maxyr <- 2021
# compareyr <- c(2019)# , 2017, 2010)
# ref_compareyr_ebs <- "@RN976" # check
# ref_compareyr_nbs <- "@RN909" # check
# id_googledrive <- googledrive::as_id("1bqXIlM9Er8MeITCkRQy8D52Yd46CLz3f")

# 2022
report_yr <- maxyr <- 2022
compareyr <- 2021
ref_compareyr_ebs <- ref_compareyr <- "@2021NEBS2022" # check
ref_compareyr_nbs <- "@RN909" # check
dir_googledrive <- "1uZy1uDB_poml2KKX3R_Qv8qrWG1WLewE" # "https://drive.google.com/drive/folders/1uZy1uDB_poml2KKX3R_Qv8qrWG1WLewE")
strat_yr <- 2022

googledrive::drive_deauth()
googledrive::drive_auth()
1

# *** Source support scripts ---------------------------------------------------

source('./code/directories.R')
dir_out_figtab <- paste0(dir_output, "figtab/")

source('./code/functions.R')

# source('./code/data_dl.R') # Run when there is new data!

source('./code/data.R')

# *** Figures and Tables ------------------------
report_spp1 <- add_report_spp(spp_info = spp_info, 
                              spp_info_codes = "species_code", 
                              report_spp = report_spp, 
                              report_spp_col = "order", 
                              report_spp_codes = "species_code", 
                              lang = FALSE)

# General figures
cnt_chapt_content<-"001"
rmarkdown::render(paste0(dir_code, "/figtab_c.Rmd"),
                  output_dir = dir_out_ref,
                  output_file = paste0(cnt_chapt_content, ".docx"))

# Species figures
for (jj in 1:length( unique(report_spp1$file_name)[!is.na(unique(report_spp1$file_name))] )) {
  print(paste0(jj, " of ", length(unique(report_spp1$file_name)), ": ", unique(report_spp1$file_name)[jj]))
  rmarkdown::render(paste0(dir_code, "/figtab_spp.Rmd"),
                    output_dir = dir_out_ref,
                    output_file = paste0(cnt_chapt_content, "_", 
                                         unique(report_spp1$file_name)[jj],".docx"))
}

# Save figures and tables locally to working draft folder
file.copy(from = dir_out_figtab, 
          to = dir_out_todaysrun, 
          overwrite = TRUE, 
          recursive = TRUE)

# Run community highlights report -----------------------------------------------

rmarkdown::render(input = paste0(dir_code, "00_full_report_community.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0("00_full_report_community_", maxyr, ifelse(refcontent, "_ref", ""), ".docx"))

# PRESENTATION ------------------------------------------------------

report_spp1 <- add_report_spp(spp_info = spp_info, 
                              spp_info_codes = "species_code", 
                              report_spp = report_spp, 
                              report_spp_col = "order", 
                              report_spp_codes = "species_code0", 
                              lang = TRUE)

yrs <- sort(nbsyr, decreasing = FALSE)

# *** Figures and Tables ------------------------

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

# }

cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_presentation_")
rmarkdown::render(paste0(dir_code, "/11_presentation.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".pptx"))

# SAVE METADATA ----------------------------------------------------------------
rmarkdown::render(paste0("./README.Rmd"),
                  output_dir = "./",
                  output_file = paste0("README.md"))


con <- file(paste0(dir_out_todaysrun, "metadata.log"))
sink(con, append=TRUE)
sessionInfo()
sink() # Restore output to console
# cat(readLines("notes.log"), sep="\n") # Look at the log


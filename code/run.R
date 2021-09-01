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
report_title <- 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
report_authors <- 'L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn'
report_yr <- substr(x = Sys.Date(), start = 1, stop = 4)            # SUGGESTION

# maxyr <- 2017 # or the year of the report, for example
# compareyr <- 2010
# SRVY<-"NEBS" 

# maxyr <- 2018 # or the year of the report, for example
# compareyr <- 2016
# SRVY<-"EBS" 

maxyr <- 2019 # or the year of the report, for example
compareyr <- 2017
SRVY<-"NEBS" 

# maxyr <- 2021 # or the year of the report, for example
# compareyr <- 2019
# SRVY<-"NEBS" 

SRVY0 <- "BS" # in Oracle

# *** OUTPUT TYPE --------------------------------------------------------------
#Is this for InDesign?
indesign_flowin <- FALSE

# *** SOURCE SUPPORT SCRIPTS ---------------------------------------------------

source('./code/directories.R')

source('./code/functions.R')

# source('./code/dataDL.R')

source('./code/data.R')

# source('./code/surveyspp.R')


# *** RENV: SAVE PACKAGES USED TO CREATE THIS REPORT ---------------------------
# renv::init()
# renv::snapshot()

# *** SIGN INTO GOOGLE DRIVE----------------------------------------------------

googledrive::drive_deauth()
googledrive::drive_auth()
1

# MAKE REPORT ------------------------------------------------------------------

# *** HOUSEKEEPING -------------------------------------------------------------

# Keep chapter content in a proper order
cnt_chapt <- "000"
# Automatically name objects with consecutive numbers
cnt_figures <- 0 #  e.g., Figure 1
cnt_tables <- 0 # e.g., Table 1
cnt_equations <- 0 # e.g., Equation 1
# Save object content
list_equations <- list()
list_tables <- list()
list_figures <- list()

# *** RUN EACH REPORT SECTION --------------------------------------------------


# *** *** 00 - Example ------------------------
# cnt_chapt<-auto_counter(cnt_chapt)
# cnt_chapt_content<-"001"
# filename0<-paste0(cnt_chapt, "_example_")
# rmarkdown::render(paste0(dir_code, "/00_example.Rmd"),
#                   output_dir = dir_out_chapters,
#                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))


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
cnt.chapt<-auto_counter(cnt.chapt)
cnt.chapt.content<-"001"
filename0<-paste0(cnt.chapt, "_Results_")
rmarkdown::render(paste0(dir.scripts, "/5_results.rmd"),
                  output_dir = dir.chapters,
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))


spplist<-(SpeciesList[SRVY][[1]])

for (spp0 in 1:length(spplist)) {
  
  spp.common<-names(spplist)[spp0]
  spp.tsn<-spplist[spp0][[1]]
  spp.sci0<-classification(spp.tsn, "itis")[[1]]
  spp.sci<-spp.sci0$name[spp.sci0$rank %in% "species"]
  
  filename0<-paste0(cnt.chapt, "_Results_")
  rmarkdown::render(paste0(dir.scripts, "/06_results_spp.Rmd"),
                    output_dir = dir.chapters,
                    output_file = paste0(filename0, cnt.chapt.content, "_Text_",
                                         gsub(x = spp.common, pattern = " ", replacement = ""),".docx"))
  
}

# cnt_chapt<-auto_counter(cnt_chapt)
# cnt_chapt_content<-"001"
# filename0<-paste0(cnt_chapt, "_results_spp_")
# rmarkdown::render(paste0(dir_code, "/06_results_spp.Rmd"),
#                   output_dir = dir_out_chapters,
#                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))


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
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_presentation_")
rmarkdown::render(paste0(dir_code, "/11_presentation.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ".pptx"))



# SAVE OTHER OUTPUTS -----------------------------------------------------------

save(list_figures,
     file=paste0(dir_out_figures, "/report_figures.rdata"))

save(list_tables,
     file=paste0(dir_out_tables, "/report_tables.rdata"))

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


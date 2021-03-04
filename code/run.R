#' ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'L. Britt, E. J. Dawson, R. Haehn and E. H. Markowitz'
#' purpose: Run Scripts and R Markdown Files
#' start date: 2021-03-03
#' date modified: 2021-03-03                                          # CHANGE
#' Notes:                                                               # CHANGE
#' ---

######START#######



########Sign into google drive##########
drive_deauth()
drive_auth()
1

######***KNOWNS#########
maxyr <- 2018 # or the year of the report, for example
SRVY<-"EBS" # For the 0frontmatter.Rmd file
SRVY0 <- "BS" # in Oracle


report_title <- paste0("Data Report: ", maxyr, " Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna")
report_authors <- 'L. Britt, E. J. Dawson, R. Haehn and E. H. Markowitz'
report_yr <- substr(x = Sys.Date(), start = 1, stop = 4)                # CHANGE
report_office_location <- 
"Alaska Fisheries Science Center

7600 Sand Point Way N.E.

Seattle, WA 98115-6349"
report_office <- "AFSC" # For example: AFSC, NEFSC                          # CHANGE
report_num <- "###"                                                     # CHANGE
report_NOAA_leaders <- "U.S. Department of Commerce

Wynn Coggins, Acting Secretary


National Oceanic and Atmospheric Administration

Benjamin Friedman, Acting NOAA Administrator


National Marine Fisheries Service

Paul Doremus, Acting Assistant Administrator for Fisheries"

#######***WHAT KIND OF OUTPUT#######
#Is this for InDesign?
indesign_flowin <- FALSE

#######SOURCE SUPPORT SCRIPTS#############
library(here) # Other functions load in the 0_functions.R

source(here::here('code', 'directories.R' ))

source(here::here('code', 'functions.R' ))

# source(here::here('code', 'dataDL.R' ))

source(here::here('code', 'data.R' ))

# source(here::here('code', 'surveyspp.R' ))


#######SAVE PACKAGES USED TO CREATE THIS REPORT#############
# renv::init()
# renv::snapshot()


######MAKE REPORT########
cnt_chapt <- "000" # Keep everything in a proper order
plot_list <- c() # This will help us by saving R-ready plots so we can easily go back and edit them if necessary.
table_list <- c() # This will help us by saving R-ready tables  so we can easily go back and edit them if necessary.
cnt_figures <- 0 # This will autoname your figures with consecutive numbers (e.g., Figure 1.)
cnt_tables <- 0 # This will autoname your tables with consecutive numbers (e.g., Table 1.)
cnt_equ <- 0

####### RUN EACH SECTION#############



  ############# 00 - Example ####################
  # cnt_chapt<-auto_counter(cnt_chapt)
  # cnt_chapt_content<-"001"
  # filename0<-paste0(cnt_chapt, "_example_")
  # rmarkdown::render(paste0(dir_code, "/00_example.Rmd"),
  #                   output_dir = dir_out_chapters,
  #                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))
# 
# 
#   
  # ############# 01 - Frontmatter ####################
  # cnt_chapt<-auto_counter(cnt_chapt)
  # cnt_chapt_content<-"001"
  # filename0<-paste0(cnt_chapt, "_frontmatter_")
  # rmarkdown::render(paste0(dir_code, "/01_frontmatter.Rmd"),
  #                   output_dir = dir_out_chapters,
  #                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))
# 
# 
#   
  # ############# 02 - Abstract ####################
  # cnt_chapt<-auto_counter(cnt_chapt)
  # cnt_chapt_content<-"001"
  # filename0<-paste0(cnt_chapt, "_abstract_")
  # rmarkdown::render(paste0(dir_code, "/02_abstract.Rmd"),
  #                   output_dir = dir_out_chapters,
  #                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  # 
  # 
  # 
  # ############# 03 - Introduction ####################
  # cnt_chapt<-auto_counter(cnt_chapt)
  # cnt_chapt_content<-"001"
  # filename0<-paste0(cnt_chapt, "_introduction_")
  # rmarkdown::render(paste0(dir_code, "/03_introduction.Rmd"),
  #                   output_dir = dir_out_chapters,
  #                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  # 
  # 
  # 
  # ############# 04 - History ####################
  # cnt_chapt<-auto_counter(cnt_chapt)
  # cnt_chapt_content<-"001"
  # filename0<-paste0(cnt_chapt, "_history_")
  # rmarkdown::render(paste0(dir_code, "/04_history.Rmd"),
  #                   output_dir = dir_out_chapters,
  #                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  # 
  # 
  # 
  # ############# 05 - Methods ####################
  # cnt_chapt<-auto_counter(cnt_chapt)
  # cnt_chapt_content<-"001"
  # filename0<-paste0(cnt_chapt, "_methods_")
  # rmarkdown::render(paste0(dir_code, "/05_methods.Rmd"),
  #                   output_dir = dir_out_chapters,
  #                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))
  # 
  # 
  # 
  # ############# 06 - Results ####################
  # cnt_chapt<-auto_counter(cnt_chapt)
  # cnt_chapt_content<-"001"
  # filename0<-paste0(cnt_chapt, "_results_")
  # rmarkdown::render(paste0(dir_code, "/06_results.Rmd"),
  #                   output_dir = dir_out_chapters,
  #                   output_file = paste0(filename0, cnt_chapt_content, ".docx"))

  # ######***RESULTS############
  # cnt.chapt<-auto_counter(cnt.chapt) 
  # cnt.chapt.content<-"001" 
  # filename0<-paste0(cnt.chapt, "_Results_")
  # rmarkdown::render(paste0(dir.scripts, "/5_results.rmd"), 
  #                   output_dir = dir.chapters, 
  #                   output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))
  # 
  # 
  # spplist<-(SpeciesList[SRVY][[1]])
  # 
  # for (spp0 in 1:length(spplist)) {
  #   
  #   spp.common<-names(spplist)[spp0]
  #   spp.tsn<-spplist[spp0][[1]]
  #   spp.sci0<-classification(spp.tsn, "itis")[[1]]
  #   spp.sci<-spp.sci0$name[spp.sci0$rank %in% "species"]
  #   
  #   filename0<-paste0(cnt.chapt, "_Results_")
  #   rmarkdown::render(paste0(dir.scripts, "/5_results_2_spp.rmd"), 
  #                     output_dir = dir.chapters, 
  #                     output_file = paste0(filename0, cnt.chapt.content, "_Text_",
  #                                          gsub(x = spp.common, pattern = " ", replacement = ""),".docx"))
  #   
  # }
  
  
  ############# 07 - Results_spp ####################
  for (spp0 in 1:length(report_species)) {
    
    
      spp_common<-names(report_species)[spp0]
      spp_code<-report_species[spp0][[1]]
      spp_sci<-unique(species_classification$report_name_scientific[species_classification$species_code %in% spp_code])
    
      cnt_chapt<-auto_counter(cnt_chapt)
      cnt_chapt_content<-"001"
      filename0<-paste0(cnt_chapt, "_results_spp_")
      rmarkdown::render(paste0(dir_code, "/07_results_spp.Rmd"),
                        output_dir = dir_out_chapters,
                        output_file = paste0(filename0, cnt_chapt_content, 
                                             gsub(x = spp_common, pattern = " ", replacement = ""), 
                                             ".docx"))
  }


  
  ############# 08 - Results_discussion ####################
  cnt_chapt<-auto_counter(cnt_chapt)
  cnt_chapt_content<-"001"
  filename0<-paste0(cnt_chapt, "_results_discussion_")
  rmarkdown::render(paste0(dir_code, "/08_results_discussion.Rmd"),
                    output_dir = dir_out_chapters,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))


  
  ############# 09 - Endmatter ####################
  cnt_chapt<-auto_counter(cnt_chapt)
  cnt_chapt_content<-"001"
  filename0<-paste0(cnt_chapt, "_endmatter_")
  rmarkdown::render(paste0(dir_code, "09_endmatter.Rmd"),
                    output_dir = dir_out_chapters,
                    output_file = paste0(filename0, cnt_chapt_content, ".docx"))



    ############# 10 - Presentation ####################
    cnt_chapt<-auto_counter(cnt_chapt)
    cnt_chapt_content<-"001"
    filename0<-paste0(cnt_chapt, "_presentation_")
    rmarkdown::render(paste0(dir_code, "/10_presentation.Rmd"),
                      output_dir = dir_out_chapters,
                      output_file = paste0(filename0, cnt_chapt_content, ".pptx"))


##### SAVE OTHER OUTPUTS#############

save(plot_list,
     file=paste0(dir_out_figures, "/report_plots.rdata"))

save(table_list,
     file=paste0(dir_out_tables, "/report_tables.rdata"))

########***MAKE MASTER DOCX################

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one


###############***METADATA##################
# So we can
#    1. Go back and recreate this exactly with the libraries you used to create this script and
#    2. Cite the apropriate versions of the packages you used in your report
# More info here: https://rstudio.github.io/packrat/walkthrough.html

CreateMetadata(dir_out = paste0(dir_out_todaysrun, "/documentation"),
               title = paste0(report_title, " Metadata ", Sys.Date()))

# setwd(paste0(dir_out_todaysrun))

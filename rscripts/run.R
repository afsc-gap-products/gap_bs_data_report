#' ---
#' title: Data Report: Run Scripts
#' purpose: run all of the scripts and R markdown scripts for data report of any survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---

rm(list=ls())


######WHAT TYPE OF OUTPUTS?#########
# Which Survey? Options: EBS, GOA, AI

######START#######

# Always start with a clean state by removing everything in your environment!
rm(list=ls())


######***KNOWNS#########
maxyr <- 2021 # or the year of the report, for example

# For the 0frontmatter.Rmd file
survey<-"EBS" 
authors0<-"R. R. Lauth, L. Britt, E. J. Dawson, R. Haehn, J. Conner, and E. H. Markowitz"
title0<-paste0("Data Report: ", maxyr, " Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna")
office0<-"F/SPO, OHC, OPR or OSF"
reportnum0<-"###"
OfficeLocation0<-"Alaska Fisheries Science Center

7600 Sand Point Way N.E.

Seattle, WA 98115-6349"

NOAALeaders0<-"U.S. Department of Commerce

Wilbur L. Ross, Jr., Secretary  


National Oceanic and Atmospheric Administration

Neil A. Jacobs, Ph.D., Acting NOAA Administrator

 
National Marine Fisheries Service

Chris Oliver, Assistant Administrator for Fisheries "

#######***WHAT KIND OF OUTPUT#######
#Is this for InDesign? 
designflowin <- F

#######***REFERENCE WORD DOCUMENT###########
# Choices: 
# "refdoc_NOAATechMemo.docx"
#   This uses the classic NOAA Tech Memo report as a guideline, in all of it's Times New Roman glory. 
# "refdoc_FisheriesEconomicsOfTheUS.docx"
#   This uses the same styles as those found in FEUS, which are considerably prettier. 

refdoc<-"refdoc_NOAATechMemo.docx" #Choose Here

#######CITATION STYLE###########
cls<-"apa.csl"

#######***LOAD PROJECT LIBRARIES AND FUNCTIONS#############
#Functions specific to this section
source("./rscripts/functions.R")

#######***LOAD PROJECT Data#############
#Data specific to this section
# source(paste0(dir.scripts, "/dataDownload.r"))
source("./rscripts/data.R")

######MAKE REPORT########
cnt.chapt<-"000" # Keep everything in a proper order
plot.list<-c() # This will help us by saving R-ready plots so we can easily go back and edit them if necessary. 
cnt.figures<-0 # This will autoname your figures with consecutive numbers (e.g., Figure 1.)
cnt.tables<-0 # This will autoname your tables with consecutive numbers (e.g., Table 1.)
cnt.equ<-0


######MAKE REPORT########

######***FRONT MATTER############
cnt.chapt<-auto_counter(cnt.chapt) # The order of the chapter in the report
cnt.chapt.content<-"001" # The order of the content in the report (e.g., figures, images, tables)
filename0<-paste0(cnt.chapt, "_FrontMatter_") #Seperated because we'll need it inside the RMarkdown
rmarkdown::render(paste0(dir.scripts, "/0_frontmatter.Rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))


######***ABSTRACT############
cnt.chapt<-auto_counter(cnt.chapt) 
cnt.chapt.content<-"001" 
filename0<-paste0(cnt.chapt, "_Abstract_")
rmarkdown::render(paste0(dir.scripts, "/1_abstract.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))

######***INTRODUCTION############
cnt.chapt<-auto_counter(cnt.chapt) 
cnt.chapt.content<-"001" 
filename0<-paste0(cnt.chapt, "_Introduction_")
rmarkdown::render(paste0(dir.scripts, "/2_introduction.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))

######***HISTORY############
cnt.chapt<-auto_counter(cnt.chapt) 
cnt.chapt.content<-"001" 
filename0<-paste0(cnt.chapt, "_History_")
rmarkdown::render(paste0(dir.scripts, "/3_history.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))

######***METHODS############
cnt.chapt<-auto_counter(cnt.chapt) 
cnt.chapt.content<-"001" 
filename0<-paste0(cnt.chapt, "_Methods_")
rmarkdown::render(paste0(dir.scripts, "/4_methods.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))


######***RESULTS############
cnt.chapt<-auto_counter(cnt.chapt) 
cnt.chapt.content<-"001" 
filename0<-paste0(cnt.chapt, "_Results_")
rmarkdown::render(paste0(dir.scripts, "/5_results.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))

# spplist<-c()
# for (i in 1:length(spplist)) {
#   counter0<-counter0+1
#   filename0<-paste0(counter0, "_ResultsDiscussion_1Text", spplist[i])
#   rmarkdown::render(paste0(dir.scripts, "/5resultsDiscussion2-spp.rmd"), 
#                     output_dir = dir.chapters, 
#                     output_file = paste0(filename0, "_",counter0,".docx"))
# }
# 
# counter0<-counter0+1
# filename0<-paste0(counter0, "_ResultsDiscussion_1Text")
# rmarkdown::render(paste0(dir.scripts, "/5resultsDiscussion3.rmd"), 
#                   output_dir = dir.chapters, 
#                   output_file = paste0(filename0, "_",counter0,".docx"))

# ######***DISCUSSION############
# cnt.chapt<-auto_counter(cnt.chapt) 
# cnt.chapt.content<-"001" 
# filename0<-paste0(cnt.chapt, "_Discussion_")
# rmarkdown::render(paste0(dir.scripts, "/6_discussion.rmd"), 
#                   output_dir = dir.chapters, 
#                   output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))

######***ACKNOWLEGEMENTS############
cnt.chapt<-auto_counter(cnt.chapt) 
cnt.chapt.content<-"001" 
filename0<-paste0(cnt.chapt, "_Acknowledgments_")
rmarkdown::render(paste0(dir.scripts, "/7_acknowledgments.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))

######***WORKS CITED############
cnt.chapt<-auto_counter(cnt.chapt) 
cnt.chapt.content<-"001" 
filename0<-paste0(cnt.chapt, "_WorksCited_")
rmarkdown::render(paste0(dir.scripts, "/8_worksCited.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, cnt.chapt.content, "_Text.docx"))


save(plot.list, file=paste0(dir.output.todaysrun, "/plots/reportPlots"))


########***MAKE MASTER DOCX################

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one


###############***METADATA##################
# So we can 
#    1. Go back and recreate this exactly with the libraries you used to create this script and 
#    2. Cite the apropriate versions of the packages you used in your report
# More info here: https://rstudio.github.io/packrat/walkthrough.html

CreateMetadata(dir.out = paste0(dir.out, "/metadata"), 
               title = paste0("Data Report for ", survey, " Metadata ", Sys.Date()))

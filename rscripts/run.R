#' ---
#' title: Data Report: Run Scripts
#' purpose: run all of the scripts and R markdown scripts for data report of any survey
#' author: emily.markowitz AT noaa.gov
#' start date: 2020-10
#' ---

######WHAT TYPE OF OUTPUTS?#########
# Which Survey? Options: EBS, GOA, AI
survey<-"EBS" 
authors0<-"R. R. Lauth, E. J. Dawson, R. Haehn, J. Conner, and E. Markowitz"

#Is this for InDesign?
designflowin <- F

#Temp, borrowing old code
FEUS0<-F

#######KNOWNS#######
maxyr <- 2018
minyr <- maxyr-2


#Functions common to all sections, to help instill standardization
# Common.Funct<-paste0(dirname(dirname(getwd())), "/FEUS/FEUS",maxyr,"Common/rscripts/Common_Functions.r")
# source(Common.Funct)
# library(devtools)
# source_url("https://raw.githubusercontent.com/emilyhmarkowitz/FEUS2018Common/master/rscripts/Common_Functions.R")
# library(here)
# dir.scripts<-paste0(here::here(), "/rscripts/")
# dir.chapters<-paste0(here::here(), "/output/FEUS",maxyr,sectname,Sys.Date(),"/chapters/")
script.functCommon<-paste0(dir.scripts, "/functionsCommon.R")
source(script.functCommon)

#######OTHER DIRECTORIES############
# dir.create(paste0(dir.outputtables, "/T1_Impact")) #table 1
# dir.create(paste0(dir.outputtables, "/T2_Revenue")) #table 2
# dir.create(paste0(dir.outputtables, "/T3_Landings")) #table 3
# dir.create(paste0(dir.outputtables, "/T4_Price")) #table 4
# dir.create(paste0(dir.outputtables, "/RevenueLandingsPrice")) #table 4

#############SAVE FILE LOCATIONS###############

script.run<-paste0(dir.scripts, "/run.r")
script.functCommon<-paste0(dir.scripts, "/functionsCommon.R")
script.funct<-paste0(dir.scripts, "/functions.R")
script.data<-paste0(dir.scripts, "/data.r")
script.surveyspp<-paste0(dir.scripts, "/surveyspp.r")
script.dataDL<-paste0(dir.scripts, "/dataDownload.r")

# Common.Policy<-paste0(dir.rscripts.common, "/Common_Policy.rmd")
# Common.CallOut.Keyspp<-paste0(dir.rscripts.common, "/Common_CallOut_KeySpp.rmd")
# Common.CallOut.IncDec<-paste0(dir.rscripts.common, "/Common_CallOut_IncDec.rmd")
# Common.CallOut.IncDec.FS<-paste0(dir.rscripts.common, "/Common_CallOut_IncDec_FS.rmd")
# Common.Title<-paste0(dir.rscripts.common, "/Common_TableFigureTitle.rmd")

#Download new data
# source(script.dataDL)

#Data specific to this section
source(script.surveyspp)
source(script.data)

#Functions specific to this section
source(script.funct)

######MAKE REPORT########


######FRONT MATTER############
counter0<-0
filename0<-paste0(counter0, "_FrontMatter_1Text")
counter<-counter0
rmarkdown::render(paste0(dir.scripts, "/0frontmatter.Rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, "_",counter,".docx"))


######ABSTRACT############
counter0<-counter0+1
filename0<-paste0(counter0, "_Abstract_1Text")
rmarkdown::render(paste0(dir.scripts, "/1abstract.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, "_",counter0,".docx"))

######INTRODUCTION############
counter0<-counter0+1
filename0<-paste0(counter0, "_Introduction_1Text")
rmarkdown::render(paste0(dir.scripts, "/3introduction.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, "_",counter0,".docx"))

######METHODS############
counter0<-counter0+1
filename0<-paste0(counter0, "_Methods_1Text")
rmarkdown::render(paste0(dir.scripts, "/4methods.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, "_",counter0,".docx"))


######RESULTS############
counter0<-counter0+1
filename0<-paste0(counter0, "_ResultsDiscussion_1Text")
rmarkdown::render(paste0(dir.scripts, "/5resultsDiscussion1.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, "_",counter0,".docx"))

spplist<-c()
for (i in 1:length(spplist)) {
  counter0<-counter0+1
  filename0<-paste0(counter0, "_ResultsDiscussion_1Text", spplist[i])
  rmarkdown::render(paste0(dir.scripts, "/5resultsDiscussion2-spp.rmd"), 
                    output_dir = dir.chapters, 
                    output_file = paste0(filename0, "_",counter0,".docx"))
}

counter0<-counter0+1
filename0<-paste0(counter0, "_ResultsDiscussion_1Text")
rmarkdown::render(paste0(dir.scripts, "/5resultsDiscussion3.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, "_",counter0,".docx"))

######ACKNOWLEGEMENTS############
counter0<-counter0+1
filename0<-paste0(counter0, "_Acknowledgments_1Text")
rmarkdown::render(paste0(dir.scripts, "/6acknowledgments.rmd"), 
                  output_dir = dir.chapters, 
                  output_file = paste0(filename0, "_",counter0,".docx"))


########MAKE MASTER DOCX################

#USE GUIDENCE FROM THIS LINK
#https://support.microsoft.com/en-us/help/2665750/how-to-merge-multiple-word-documents-into-one


###############METADATA##################
CreateMetadata(dir.out = paste0(dir.out, "/metadata"), 
               title = paste0("Data Report for ", survey, " Metadata ", Sys.Date()))

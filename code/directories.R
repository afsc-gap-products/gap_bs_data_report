#'  ---
#' title: 'Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna'
#' author: 'E. H. Markowitz'
#' purpose: Create directories and short hands for those directories
#' start date: 2021-09-01
#' date modified: 2021-09-01        # CHANGE
#' Notes:                             # CHANGE
#' ---

# SAVE FILE LOCATIONS ----------------------------------------------------------
# Just in case you change the base name for any reason, it will change for anytime you load the files inside the folder, too! (e.g., if you have something against "scripts" being the name of the folder, just let the script know in one place aka right here).

# Directories to all of your current folders
dir_in<-paste0(here::here(), "/")
dirs<-list.dirs(path = getwd(), full.names = FALSE, recursive = FALSE)
dirs<-dirs[!grepl(pattern = "\\..", x = dirs)]
for (i in 1:length(dirs)) {
  assign(x = paste0("dir_", dirs[i]),
         value = paste0(dir_in, (dirs[i]), "/"))
}

# Where we save everything
dir_out_todaysrun <- paste0(dir_output, Sys.Date(),"/")
dir.create(dir_out_todaysrun)
dir_out_todaysrun <- paste0(dir_out_todaysrun, maxyr, "_", report_title, "/")
dir.create(dir_out_todaysrun)

dir_out_figtab <- paste0(dir_output, "figtab_",maxyr,"/")

dirs <- c("chapters", "rawdata")#, "documentation", "code", "figtab", "cite", "ref")
for (i in 1:length(dirs)) {
  if (dir.exists(paste0(dir_out_todaysrun, dirs[i])) == FALSE) {
    dir.create(paste0(dir_out_todaysrun, "/", dirs[i]))
  }
  assign(x = paste0("dir_out_", dirs[i]), value = paste0(dir_out_todaysrun, "/",dirs[i],"/"))
}

# # CITATION STYLE ---------------------------------------------------------------
options("citation_format" = "pandoc")

.rs.restartR()

library(devtools)
devtools::install_github("EmilyMarkowitz-NOAA/NMFSReports", force = TRUE)
3


library(NMFSReports)

sections = c("frontmatter", "abstract", "introduction", 
             "history", "methods", 
             "results", "results_spp", 
             "results_discussion", "results_discussion_spp", 
             "endmatter")
authors = "L. Britt, E. J. Dawson, R. Haehn and E. H. Markowitz"
title = paste0("Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna")
styles_reference_pptx = "refppt_nmfs"
styles_reference_docx = "refdoc_noaa_tech_memo"
bibliography.bib = "bib_example"
csl = "bulletin-of-marine-science"
# write(x = read.delim(file = "https://raw.githubusercontent.com/citation-style-language/styles/master/nature.csl", 
#                  as.is = TRUE), file = "csl.csl")
# library(here)
# csl = here("csl.csl")

buildReport(
  sections = sections,
  authors = authors,
  title = title,
  styles_reference_pptx = styles_reference_pptx,
  styles_reference_docx = styles_reference_docx,
  bibliography.bib = bibliography.bib,
  csl = csl
)

source("./code/run.R")

# user csl
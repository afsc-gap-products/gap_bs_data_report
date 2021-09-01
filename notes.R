
library(devtools)
devtools::install_github("EmilyMarkowitz-NOAA/NMFSReports")
3
library(NMFSReports)


buildReport(
  sections = c("abstract", "introduction", "history", "methods", "results", "results_spp", "results_crabretow", "results_15-30study", "results_discussion",
               "endmatter", "presentation"),
  report_authors  = "L. Britt, E. H. Markowitz, E. J. Dawson, and R. Haehn",
  report_title  = "Data Report: MAXYR Eastern Bering Sea continental shelf Bottom Trawl Survey of Groundfish and Invertebrate Fauna",
  styles_reference_pptx = "refppt_nmfs",
  styles_reference_docx = "refdoc_noaa_tech_memo",
  bibliography.bib = "bib_example",
  csl = "bulletin-of-marine-science"
)

buildReport<-function(
  sections = c("abstract",
               "introduction",
               "methods",
               "results",
               "discussion",
               "endmatter",
               "presentation"),
  authors = "",
  title = "",
  styles_reference_pptx = "refppt_nmfs",
  styles_reference_docx = "refdoc_noaa_tech_memo",
  bibliography.bib = "bib_example",
  csl = "bulletin-of-marine-science") {
  
  ##################  Create Architecture
  dirs <- c("code", "data", "documentation", "img", "cite", "output")
  
  for (i in 1:length(dirs)) {
    if (dir.exists(dirs[i]) == FALSE) {
      dir.create(dirs[i])
    }
  }
  
  # Now... Load those folders with stuff you care about
  
  ################## RMD scripts
  a <- list.files(path = system.file("rmd", package="NMFSReports"), pattern = "0_")
  b <- c("example", sections)
  if (!(is.null(styles_reference_pptx))) {
    b <- c(b, "presentation")
  }
  
  b<-unique(b)
  
  counter<-NMFSReports::numbers0(x = c(0, length(b)))[1]
  temp<-gsub(pattern = "\\.Rmd", replacement = "",
             x = gsub(pattern = "0_", replacement = "",
                      x = a,
                      ignore.case = T))
  
  for (i in 1:length(b)){
    
    copyfrom<-ifelse((sum(temp %in% b[i]) == 1),
                     a[which(temp %in% b[i])],
                     "0_blank.Rmd")
    
    copyto <- paste0("./code/", counter,"_",b[i], ".Rmd")
    
    counter<-NMFSReports::auto_counter(counter)
    
    file.copy(from = system.file("rmd", copyfrom, package="NMFSReports"),
              to = copyto,
              overwrite = T)
    
    if (copyfrom %in% "0_blank.Rmd") {
      
      rfile <- base::readLines(paste0(copyto))
      
      rfile <- gsub(pattern = "SECTION_TITLE",
                    replacement = NMFSReports::TitleCase(b[i]),
                    x = rfile)
      
      utils::write.table(x = rfile,
                         file = copyto,
                         row.names = FALSE,
                         col.names = FALSE,
                         quote = FALSE)
    }
  }
  
  ################## R scripts
  a <- list.files(path = system.file("rmd", package="NMFSReports"), pattern = "1_")
  support_scripts = c("directories",
                      "functions",
                      "dataDL",
                      "data")
  b <- support_scripts
  for (i in 1:length(b)){
    
    temp<-gsub(pattern = "\\.R", replacement = "",
               x = gsub(pattern = "1_", replacement = "",
                        x = a,
                        ignore.case = T))
    
    copyfrom <- ifelse((sum(temp %in% b[i]) == 1),
                       a[which(temp %in% b[i])],
                       "1_blank.R")
    
    copyto <- paste0("./code/", b[i], ".R")
    
    file.copy(from = system.file("rmd", copyfrom, package="NMFSReports"),
              to = copyto,
              overwrite = T)
    
    rfile <- base::readLines(copyto)
    
    rfile <- gsub(pattern = "# INSERT_REPORT_TITLE",
                  replacement = ifelse(title %in% "", "''",
                                       paste0("'", title, "'")),
                  x = rfile)
    
    rfile <- gsub(pattern = "# INSERT_AUTHOR",
                  replacement = ifelse(authors %in% "", "''",
                                       paste0("'", authors, "'")),
                  x = rfile)
    
    rfile<-gsub(pattern = "# YYYY-MM-DD",
                replacement = Sys.Date(),
                x = rfile)
    
    utils::write.table(x = rfile,
                       file = copyto,
                       row.names = FALSE,
                       col.names = FALSE,
                       quote = FALSE)
  }
  
  ################## other content
  b<-c("header.yaml",
       styles_reference_docx,
       styles_reference_pptx,
       csl,
       # "TableFigureHeader.Rmd",
       bibliography.bib) # bib_example
  
  for (i in 1:length(b)){
    
    b[i]<-dplyr::case_when(grepl(pattern = "refdoc", x = b[i]) ~
                             paste0(b[i], ".docx"),
                           grepl(pattern = "refppt", x = b[i]) ~
                             paste0(b[i], ".pptx"),
                           grepl(pattern = "bib_", x = b[i]) ~
                             paste0(b[i], ".bib"),
                           TRUE ~ b[i])
    
    if (system.file("rmd", b[i], package="NMFSReports") != "") { # is a file from the package
      copyfrom <- system.file("rmd", b[i], package="NMFSReports")
    } else if (system.file("cite", paste0(b[i], ".csl"), package="NMFSReports") != "") {
      copyfrom <- system.file("cite", paste0(b[i], ".csl"), package="NMFSReports")
    } else if (dir.exists(dirname(b[i]))) { # is a local path
      copyfrom <- b[i]
    }
    
    copyto <- dplyr::case_when(b[i] == paste0(styles_reference_docx, ".docx") ~
                                 "./code/styles_reference.docx",
                               b[i] == paste0(styles_reference_pptx, ".pptx") ~
                                 "./code/styles_reference.pptx",
                               b[i] == paste0(bibliography.bib, ".bib") ~
                                 "./cite/bibliography.bib",
                               b[i] == csl ~
                                 "./cite/citestyle.csl",
                               TRUE ~ paste0("./code/", b[i]))
    file.copy(from = copyfrom,
              to = copyto,
              overwrite = T)
  }
  
  ################## images
  # Load those folders with stuff you care about
  a<-list.files(path = system.file("img", package="NMFSReports"))
  for (i in 1:length(a)){
    file.copy(from = system.file("img", a[i], package="NMFSReports"),
              to = paste0("./img/", a[i]),
              overwrite = T)
  }
  
  ################## Write run.R
  run0 <- base::readLines(system.file("rmd","run.R",
                                      package="NMFSReports"))
  
  # directories
  
  # support_scripts
  a<-paste0("source('./code/",
            paste0(support_scripts, ".R')

"), collapse = "")
  
  run0<-gsub(pattern = "# INSERT_SUPPORT_SCRIPTS",
             replacement = a,
             x = run0)
  
  # INSERT_SECTIONS
  b <- list.files(path = "./code/", pattern = ".Rmd") # find the files that are already there
  bb <- strsplit(x = b, split = "_")
  sections_no <- unlist(lapply(bb, `[[`, 1))
  bb <- strsplit(x = b, split = "[0-9]+_")
  b<-unlist(lapply(bb, function(x) x[-1]))
  b <- gsub(pattern = ".Rmd", replacement = "",
            x = unlist(lapply(bb, `[[`, 2)))
  b_type <- rep_len(x = '".docx"', length.out = length(b))
  if (sum(b %in% "presentation")>0) {
    b_type[which(b %in% "presentation")]<-'".pptx"'
  }
  
  
  a<-paste(paste0('
# *** *** ', sections_no,' - ', stringr::str_to_title(b),' ------------------------
cnt_chapt<-auto_counter(cnt_chapt)
cnt_chapt_content<-"001"
filename0<-paste0(cnt_chapt, "_', b,'_")
rmarkdown::render(paste0(dir_code, "/',sections_no,'_',b,'.Rmd"),
                  output_dir = dir_out_chapters,
                  output_file = paste0(filename0, cnt_chapt_content, ',b_type,'))

'), collapse = "")
  
  run0<-gsub(pattern = "# INSERT_SECTIONS",
             replacement = a,
             x = run0)
  
  
  # # INSERT_POWERPOINT
  # # only if there is a reference type specified
  # if (!(is.null(styles_reference_pptx))) {
  #   sections_no_pres <- auto_counter(sections_no[length(sections_no)])
  #   a<-dplyr::if_else(is.na(styles_reference_pptx),
  #                     "",
  #                     paste(paste0('
  #   ############# ', sections_no_pres,' - Presentation ####################
  #   cnt_chapt<-auto_counter(cnt_chapt)
  #   cnt_chapt_content<-"001"
  #   filename0<-paste0(cnt_chapt, "_presentation_")
  #   rmarkdown::render(paste0(dir_code, "/',sections_no_pres,'_presentation.Rmd"),
  #                     output_dir = dir_out_chapters,
  #                     output_file = paste0(filename0, cnt_chapt_content, ".pptx"))
  #
  #
  #   '), collapse = ""))
  #
  #   run0<-gsub(pattern = "# INSERT_POWERPOINT",
  #              replacement = a,
  #              x = run0)
  # } else {
  #   run0<-gsub(pattern = "# INSERT_POWERPOINT",
  #              replacement = "",
  #              x = run0)
  # }
  
  # OTHER CONTENT
  run0<-gsub(pattern = "# INSERT_REPORT_TITLE",
             replacement = ifelse(title %in% "", "''",
                                  paste0("'", title, "'")),
             x = run0)
  
  run0<-gsub(pattern = "# INSERT_AUTHOR",
             replacement = ifelse(authors %in% "", "''",
                                  paste0("'", authors, "'")),
             x = run0)
  
  run0<-gsub(pattern = "# YYYY-MM-DD",
             replacement = Sys.Date(),
             x = run0)
  
  
  # write new run file
  utils::write.table(x = run0,
                     file = "./code/run.R",
                     row.names = FALSE,
                     col.names = FALSE,
                     quote = FALSE)
  
  # done!
  
}
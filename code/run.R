
# Report knowns ----------------------------------------------------------------

access_to_internet  <- FALSE # TRUE = redownload google drive tables and docs
quarto <- FALSE
options(scipen=999)

maxyr <- 2025
compareyr <- c("EBS" = 2024, "NBS" = 2023)
# compareyr0 <- 2023
strat_yr <- 2022
srvy <- "NEBS" # "EBS"
# ref_compareyr <- "@2024EBS" # CHANGE
ref_compareyr <- "@2023NEBS" # CHANGE
ref_compareyr_ebs <- "@2024EBS" # CHANGE
dir_googledrive <- "https://drive.google.com/drive/folders/15FM6WQ7Uqb1AbsQLsWQ3O-P5WnPYlPJx"
dir_googledrive_comm <- "https://drive.google.com/drive/folders/1T2Vv4soro2z-jGDlxXlQUfWiyEB4ehy6"
dl_change_start <- "24-APR-02 11.00.00 PM"
dl_change_end <- toupper(format(x = Sys.time(), format = "%d-%b-%y %I.%M.%S %p")) # "22-OCT-24 11.59.00 PM"

# Data Report ------------------------------------------------------------------

## Source Scripts --------------------------------------------------------------

report_title <- "data" 
# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
# devtools::install_github("afsc-gap-products/coldpool")
# devtools::install_github("afsc-gap-products/gapindex")
source(here::here("code","functions.R"))
# source(here::here("code","data_dl.R"))  # Run when there is new data!
source(here::here("code","data.R"))
2 # no touchy! - Needed for Google Drive authentication

## Figures and Tables ----------------------------------------------------------

# General figures
rmarkdown::render(paste0(dir_code, "/figtab.Rmd"),
                  output_dir = dir_out_rawdata,
                  output_file = paste0("figtab.docx"))

# Species figures
comb <- report_spp0 |> 
  dplyr::filter(!is.na(community_order) |	!is.na(datar_order)) |> 
  dplyr::select(file_name, species_code) 

report_spp3 <- data.frame()
for (i in 1:nrow(comb)){
  temp2 <- eval(expr = parse(text = comb$species_code[i]))
  report_spp3 <- dplyr::bind_rows(report_spp3, 
                            dplyr::bind_cols(comb[i,], 
                                             species_code1 = eval(expr = parse(text = comb$species_code[i]))))
}
comb <- unique(sort(comb$file_name))
# comb <- comb[!grepl(pattern = "-crab", x = comb)] # temporary
comb <- comb[!grepl(pattern = "butterfly", x = comb)] # temporary
comb <- comb[!grepl(pattern = "octopuses", x = comb)] # temporary
for (jj in 1:length(comb)) {
  print(paste0(jj, " of ", length(comb), ": ", comb[jj]))
  a <- report_spp3[which(report_spp3$file_name == comb[jj]), ]
  spp_code <- a$species_code1
  aa <- catch_haul_cruises |> 
    dplyr::filter(species_code %in% spp_code & year == maxyr)
  if (nrow(aa)>0) {
  rmarkdown::render(paste0(dir_code, "/figtab_spp.Rmd"),
                    output_dir = dir_out_rawdata,
                    output_file = paste0("figtab_spp_", comb[jj],".docx"))
  }
}

## Write report ----------------------------------------------------------------

# Main Body
rmarkdown::render(input = paste0(dir_code, "00_data_report.Rmd"), 
                  output_format = "officedown::rdocx_document", 
                  output_dir = dir_out_todaysrun, 
                  output_file = paste0("00_data_report_", maxyr, ".docx"))

# Community Highlights ---------------------------------------------------------

report_title <- "community"
dir_googledrive <- dir_googledrive_comm
source(here::here("code","functions.R"))
source(here::here("code","data.R"))
2 # no touchy! - Needed for Google Drive authentication

## Write report ----------------------------------------------------------------

rmarkdown::render(input = paste0(dir_code, "00_community_report.Rmd"), 
                  output_format = "officedown::rdocx_document", 
                  output_dir = dir_out_todaysrun, 
                  output_file = paste0("00_community_report_", maxyr, ".docx"))

# Presentation slides ----------------------------------------------------------

source(here::here("code","functions.R"))
source(here::here("code","data.R"))

# Build Presentation
rmarkdown::render(input = paste0(dir_code, "00_presentation.Rmd"), 
                  output_dir = dir_out_todaysrun, 
                  output_file = paste0("00_presentation_", maxyr, ".pptx"))


# Write README -----------------------------------------------------------------

rmarkdown::render(paste0(here::here("code","README.Rmd")),
                  output_dir = here::here(),
                  output_file = paste0("README.md"))

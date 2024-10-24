
# Report knowns ----------------------------------------------------------------

access_to_internet  <- TRUE # redownload google drive tables and docs?
quarto <- FALSE
options(scipen=999)

maxyr <- 2024
compareyr <- 2023
compareyr0 <- 2023
strat_yr <- 2022
SRVY <- "EBS"
ref_compareyr <- ref_compareyr_ebs <- "@2023NEBS" # CHANGE
dir_googledrive <- "https://drive.google.com/drive/folders/1uLrsMoJsmGRe8vB0-QSS_6NT8AuWAKsp"
dl_change_start <- "22-APR-02 11.00.00 PM"
dl_change_end <- "22-OCT-24 11.59.00 PM"

googledrive::drive_deauth()
googledrive::drive_auth(); 
2  # Set this to 1 when first running to allow access in subsequent sessions

# Data Report ------------------------------------------------------------------

## Source Scripts --------------------------------------------------------------

report_title <- "data" 
# devtools::install_github("afsc-gap-products/akgfmaps@shp2025", build_vignettes = TRUE)
# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
# devtools::install_github("afsc-gap-products/coldpool")
# devtools::install_github("afsc-gap-products/gapindex")
source(here::here("code","functions.R"))
# source(here::here("code","data_dl.R"))  # Run when there is new data!
source(here::here("code","data.R"))

## Figures and Tables ----------------------------------------------------------

# General figures
rmarkdown::render(paste0(dir_code, "/figtab.Rmd"),
                  output_dir = dir_out_rawdata,
                  output_file = paste0("figtab.docx"))

# Species figures
comb <- report_spp0 %>% 
  dplyr::filter(!is.na(community_order) |	!is.na(datar_order)) %>% 
  dplyr::select(file_name, species_code) 

report_spp3 <- data.frame()
for (i in 1:nrow(comb)){
  temp2 <- eval(expr = parse(text = comb$species_code[i]))
  report_spp3 <- dplyr::bind_rows(report_spp3, 
                            dplyr::bind_cols(comb[i,], 
                                             species_code1 = eval(expr = parse(text = comb$species_code[i]))))
}
comb <- unique(sort(comb$file_name))
for (jj in 1:length(comb)) {
  print(paste0(jj, " of ", length(comb), ": ", comb[jj]))
  a <- report_spp3[which(report_spp3$file_name == comb[jj]), ]
  spp_code <- a$species_code1
  aa <- catch_haul_cruises %>% 
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
source(here::here("code","functions.R"))
source(here::here("code","data.R"))

## Figures and Tables ----------------------------------------------------------

comb <- report_spp1 %>% dplyr::filter(!is.na(order)) %>% dplyr::select(file_name) %>% unlist() %>% unique()
for (jj in 1:length(comb)) {
  a <- report_spp1[which(report_spp1$file_name == comb[jj]), ]
  spp_code <- a$species_code
  aa <- catch_haul_cruises %>% 
    dplyr::filter(species_code %in% spp_code & year == maxyr)
  if (nrow(aa)>0) {
  print(paste0(jj, " of ", length(comb), ": ", comb[jj]))
  rmarkdown::render(paste0(dir_code, "/figtab_spp.Rmd"),
                    output_dir = dir_out_rawdata,
                    output_file = paste0("figtab_spp_", comb[jj],".docx"))
  }
}

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

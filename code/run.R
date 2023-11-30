
# Report knowns ----------------------------------------------------------------

refcontent <- FALSE # produce extra summary text and tables for each spp to help with writing
access_to_internet  <- TRUE # redownload google drive tables and docs?
pres_img <- FALSE
font0 <- "Arial Narrow"
font_size0 <- 12
quarto <- FALSE

maxyr <- 2022
compareyr <- 2021
compareyr0 <- 2019
strat_yr <- 2022
SRVY<-"NEBS"
ref_compareyr <- ref_compareyr_ebs <- "@2021NEBS2022" # CHANGE
# dir_googledrive <- "1x50OKqAyLcqNLYhjQNX84dHLXHcTBnaD" # https://drive.google.com/drive/folders/1x50OKqAyLcqNLYhjQNX84dHLXHcTBnaD
ref_compareyr_nbs <- "@RN909" # community report
# dir_googledrive_comm <- "1uZy1uDB_poml2KKX3R_Qv8qrWG1WLewE" # "https://drive.google.com/drive/folders/1uZy1uDB_poml2KKX3R_Qv8qrWG1WLewE")

maxyr <- 2023
compareyr <- 2022
compareyr0 <- 2021
strat_yr <- 2022
SRVY <- "NEBS"
ref_compareyr <- ref_compareyr_ebs <- "@2022NEBS2023" # CHANGE
dir_googledrive <- "https://drive.google.com/drive/folders/19ttU1_VAlos_3KKjiRqfcMF-k1cpaENN" 
dir_googledrive_comm <- "https://drive.google.com/drive/folders/1gJYWYWzU8Iwi7gQmoSpCFVfxsoV20P2v"

googledrive::drive_deauth()
googledrive::drive_auth()
2  # Set this to 1 when first running to allow access in subsequent sessions

# Data Report ------------------------------------------------------------------

## Source Scripts --------------------------------------------------------------

report_title <- "data" 
# devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
# devtools::install_github("afsc-gap-products/coldpool")
source(here::here("code","functions.R"))
# source('./code/data_dl.R') # devtools::install_github("afsc-gap-products/gapindex") # Run when there is new data!
source(here::here("code","data.R"))

## Figures and Tables ----------------------------------------------------------

# General figures
rmarkdown::render(paste0(dir_code, "/figtab.Rmd"),
                  output_dir = dir_out_rawdata,
                  output_file = paste0("figtab.docx"))

# Species figures
comb <- report_spp1 %>% dplyr::filter(!is.na(order)) %>% dplyr::select(file_name) %>% unlist() %>% unique()
comb <- "yellowfin sole"  # single species for debugging
for (jj in 1:length(comb)) {
  print(paste0(jj, " of ", length(comb), ": ", comb[jj]))
  a <- report_spp1[which(report_spp1$file_name == comb[jj]), ]
  spp_code <- a$species_code
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
                  output_dir = dir_out_chapters, 
                  output_file = paste0("00_data_report_", maxyr, ifelse(refcontent, "_ref", ""), ".docx"))

# Appendix (could be combined with above, but both are just such big files!)
rmarkdown::render(input = paste0(dir_code, "00_data_report_app.Rmd"), 
                  output_format = "officedown::rdocx_document", 
                  output_dir = dir_out_chapters, 
                  output_file = paste0("00_data_report_app_", maxyr, ".docx"))

# Community Highlights ---------------------------------------------------------

report_title <- "community"
dir_googledrive <- dir_googledrive_comm
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
                  output_dir = dir_out_chapters, 
                  output_file = paste0("00_community_report_", maxyr, ifelse(refcontent, "_ref", ""), ".docx"))

# Plan team presentation -------------------------------------------------------

SRVY <- "EBS"
report_title <- "ptpres" 
source(here::here("code","functions.R"))
source(here::here("code","data.R"))

# Presentation Species figures
comb <- report_spp1 %>% 
  dplyr::filter(!is.na(order) & 
                  file_name %in%  
                  c("walleye-pollock", "pacific-cod", "yellowfin-sole", 
                    "northern-rock-sole", "flathead-sole", "alaska-plaice",
                    "pacific-halibut", "alaska-skate", "arrowtooth-flounder", "pacific-ocean-perch")) %>%
  dplyr::select(file_name) %>% 
  unlist() %>% 
  unique()
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

# Build Presentation
rmarkdown::render(input = paste0(dir_code, "00_plan_team_pres.Rmd"), 
                  output_dir = dir_out_chapters, 
                  output_file = paste0("00_plan_team_pres_", maxyr, ".pptx"))


# Strait sci presentation -------------------------------------------------------

SRVY <- "NEBS"
report_title <- "nbspres" 
dir_googledrive <- dir_googledrive_comm
source(here::here("code","functions.R"))
source(here::here("code","data.R"))

# Presentation Species figures
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

# Build Presentation
rmarkdown::render(input = paste0(dir_code, "00_strait_sci_pres.Rmd"), 
                  output_dir = dir_out_chapters, 
                  output_file = paste0("00_strait_sci_pres_", maxyr, ".pptx"))

# Write README -----------------------------------------------------------------

rmarkdown::render(paste0(here::here("code","README.Rmd")),
                  output_dir = here::here(),
                  output_file = paste0("README.md"))


stomach_data <- c()
aa <- list.files(path = "G:/EBSdata/EBS2026/Northwest Explorer/Leg 3/STOMACH DATA/", full.names = TRUE)
aa <- c(aa, list.files(path = "G:/EBSdata/EBS2026/Alaska Knight/Leg 3/CATCH_METIS/stomach/", full.names = TRUE))
for (i in aa) {
  stomach_data <- stomach_data |> 
    dplyr::bind_rows(readr::read_csv(file = i) |> 
                       dplyr::mutate_all(as.numeric))
}

table(stomach_data$SPECIES_CODE)
nrow(stomach_data)

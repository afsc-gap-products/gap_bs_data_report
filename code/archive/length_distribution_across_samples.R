


width <- full_page_portrait_width # 6.5  
header <- pres_size_txt <- ""
legend_font_size <- 12

temp <- matrix(data = c(8857041501,10285,
                        8857040901,10210,
                        8857041901,10120,
                        8857040602,10140,
                        8857040803,10261,
                        8791030701,21740,
                        8791030401,21720,
                        8857040601,10130,
                        8857041801,10115,
                        8857040102,10110,
                        8857040101,10112,
                        8713040105, 435), 
               ncol = 2, byrow = TRUE) |> 
  data.frame()
names(temp) <- c("pred_nodc", "species_code")

temp <- report_spp1 |> 
  dplyr::filter(plot_agecomp) |> 
  dplyr::select(file_name, species_code, print_name) |> 
  dplyr::left_join(spp_info |> dplyr::select(species_code, species_name)) |> 
  dplyr::left_join(temp)

file_name0 <- unique(temp$file_name)

yrs <- c(2026, 2025)

table_raw <- dplyr::bind_rows(
  sizecomp |>
    dplyr::select(survey_definition_id, species_code, length_mm, sex, year, freq = population_count) |> 
    dplyr::filter(species_code %in% temp$species_code &
                    year %in% yrs) |> 
    dplyr::mutate(source = "sizecomp"), 
  
  # foodlab_predprey0 |> 
  #   dplyr::right_join(temp, relationship = "many-to-many") |>
  #   dplyr::right_join(foodlab_haul0 |> 
  #                       dplyr::filter(cruise_type == "Race_Groundfish")) |> 
  #   dplyr::select(haul, vessel, year, cruise, pred_len, pred_sex, year, species_code) |> 
  #   # dplyr::left_join(haul |> dplyr::select(haul, vessel = vessel_id, year, cruise, cruisejoin) |> dplyr::distinct()) |>
  #   dplyr::left_join(cruises |> dplyr::select(survey_definition_id, cruisejoin) |> dplyr::distinct()) |>
  #   dplyr::left_join(foodlab_nodc0 |> dplyr::select(pred_nodc = nodc, species_code = race)) |>
  #   dplyr::select(survey_definition_id, length_mm = pred_len, sex = pred_sex, year, species_code) |> # species_code, 
  #   dplyr::filter(species_code %in% temp$species_code &
  #                   year %in% yrs) |> 
  #   dplyr::group_by(species_code, survey_definition_id, sex, length_mm) |> 
  #   dplyr::summarise(freq = n()) |> 
  #   dplyr::ungroup() |> 
  #   dplyr::mutate(source = "stomach"), 
  
  specimen |> 
    dplyr::left_join(haul |> dplyr::select(hauljoin, cruisejoin)) |>
    dplyr::left_join(cruises |> dplyr::select(survey_definition_id, cruisejoin)) |>
    dplyr::mutate(sex = dplyr::case_when(
      sex == 1 ~ "males", 
      sex == 2 ~ "females", 
      sex == 3 ~ "unsexed" ) ) |> 
    dplyr::filter(species_code %in% temp$species_code &
                    year %in% yrs) |> 
    dplyr::group_by(species_code, survey_definition_id, sex, length_mm, year) |>
    dplyr::summarise(freq = n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(source = "specimen")
  ) |> 
  dplyr::left_join(temp) |> 
  # dplyr::mutate(sex = str_to_sentence(sex), 
  #               sex = factor(sex, 
  #                            levels = c("males", "females", "unsexed"), 
  #                            labels = c("males", "females", "unsexed"),
  #                            ordered = TRUE)# , 
  #               srvy_long = dplyr::case_when(
  #                 srvy == 98 ~ "eastern Bering Sea",
  #                 srvy == 142 ~ "northern Bering Sea")
  #               ) |> 
  
  dplyr::arrange(sex) |> 
  dplyr::filter(!is.na(year)) |> 
  dplyr::filter(!is.na(sex)) |> 
  dplyr::filter(year == 2026)

# for (spp_code in unique(temp$species_code)) {
  
  # type <- unique(lengths$sentancefrag[lengths$species_code == spp_code])
  # type <- type[!is.na(type)]
  # spp_print <- unique(temp$print_name[temp$species_code == spp_code])
  # spp_print <- spp_print[!is.na(spp_print)]
# 
#   table_raw <- table_raw0 |> 
#     dplyr::filter(species_code == spp_code)
        

figure_print <- 
  ggplot2::ggplot(data = table_raw, # |> dplyr::filter(source == "specimen"), # specimen "sizecomp"
                   mapping = aes(x = length_mm,
                                 y = freq,
                                 fill = sex)) +
    ggplot2::geom_bar(position="stack", stat="identity", na.rm = TRUE) +
    ggplot2::scale_fill_viridis_d(direction = -1, 
                                  option = "mako",
                                  begin = .2,
                                  end = .6,
                                  na.value = "transparent", 
                                  drop = FALSE) +
    ggplot2::scale_y_continuous(name = "", # "Frequency", 
                                expand = c(0, 0), 
                                labels = scales::label_comma(accuracy = 1)) +
    ggplot2::scale_x_continuous(name = "", #stringr::str_to_sentence(paste0(type)), 
                                labels = scales::label_comma(accuracy = 1))  +
    # ggplot2::labs(fill = spp_print) +
    ggplot2::guides(
      fill = guide_legend(title.position = "top",
                          title.hjust = 0.5,
                          title.vjust = -0.5)) +
    ggplot2::theme(
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(fill = NA,
                                  colour = "grey20"),
      legend.text = element_text(size = legend_font_size),
      legend.background = element_rect(colour = "transparent", 
                                       fill = "transparent"),
      legend.key = element_rect(colour = "transparent",
                                fill = "transparent"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.box = "horizontal",
      text = element_text(size = 8), 
      legend.box.spacing = unit(0, "pt")) +
    ggplot2::facet_grid(print_name ~ source, scales = "free", axes = "all")
# }



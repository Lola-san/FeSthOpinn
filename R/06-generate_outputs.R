##--------------------------------------------------------------------------------------------------------
## SCRIPT : generate output figures and tables
## 06-generate_outputs.R
##
## Authors : Lola Gilbert
## Last update : 2022-09
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------


# This script should contain FUNCTIONS ONLY


############################## preliminary function : to sum vec embedded in cell ########################
sum_vec <- function(list_of_vec) {
  summed_vec <- rep(0, length(list_of_vec[[1]]))

  for (j in seq_along(list_of_vec)) {
    summed_vec <- summed_vec + list_of_vec[[j]]
  }
  return(summed_vec)
}

############################# output functions ###########################################################

#'
#'
#'
#'
#'
#' function to generate figure displaying total amount of Fe released by the 4 species of pack-ice seals
fig_tot_Fe_released <- function(output_tib,
                                object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                name_file) {

  options(scipen = 999)
  mean_tot <- (output_tib |>
                 dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                 tidyr::unnest(tot_Fe) |>
                 dplyr::summarize(min = min(value),
                                  `2.5_quant` = quantile(value, probs = c(0.025)),
                                  mean = mean(value),
                                  median = median(value),
                                  `97.5_quant` = quantile(value, probs = c(0.975)),
                                  max = max(value)))$mean

  firstquant <- (output_tib |>
                   dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                   tidyr::unnest(tot_Fe) |>
                   dplyr::summarize(min = min(value),
                                    `2.5_quant` = quantile(value, probs = c(0.025)),
                                    mean = mean(value),
                                    median = median(value),
                                    `97.5_quant` = quantile(value, probs = c(0.975)),
                                    max = max(value)))$`2.5_quant`

  lastquant <- (output_tib |>
                  dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                  tidyr::unnest(tot_Fe) |>
                  dplyr::summarize(min = min(value),
                                   `2.5_quant` = quantile(value, probs = c(0.025)),
                                   mean = mean(value),
                                   median = median(value),
                                   `97.5_quant` = quantile(value, probs = c(0.975)),
                                   max = max(value)))$`97.5_quant`


  figure <- output_tib |>
    dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
    tidyr::unnest(tot_Fe) |>
    dplyr::summarize(min = min(value),
                     `2.5_quant` = quantile(value, probs = c(0.025)),
                     mean = mean(value),
                     median = median(value),
                     `97.5_quant` = quantile(value, probs = c(0.975)),
                     max = max(value)) |>
    dplyr::mutate(Param = "Total Fe released (t/yr)")|>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Param, y = mean), stat = "identity", fill = "skyblue") +
    ggplot2::geom_errorbar(ggplot2::aes(x = Param, ymin = `2.5_quant`, ymax = `97.5_quant`),
                           color = "gray40",
                           size = 1,
                           width = 0) +
    ggplot2::geom_point(ggplot2::aes(x = Param, y = mean),
                        color = "gray40",
                        size = 2) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1",
                                                                  1, # nb of areas
                                                                  type = "continuous")) +
    ggplot2::ylim(c(0, 600)) +
    ggplot2::xlab(" ") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::annotate("text", x = 0.75, y = mean_tot + 10, label = paste0("mean = ", round(mean_tot))) +
    ggplot2::annotate("text", x = 0.75, y = firstquant + 20, label = paste0("quantile 2.5% = ", round(firstquant))) +
    ggplot2::annotate("text", x = 0.75, y = lastquant + 20, label = paste0("quantile 97.5% = ", round(lastquant))) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(face = "bold", size = 12))

  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                    width = 6,
                    height = 5)
  } else {
    figure
  }

}



#'
#'
#'
#'
#'
# function to generate figure displaying total amount of Fe released by
# the 4 species of pack-ice seals
fig_tot_Fe_released_comp <- function(output_tib,
                                     object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                     name_file) {

  options(scipen = 999)

  mean_this_study <- (output_tib |>
                        dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                        tidyr::unnest(tot_Fe) |>
                        dplyr::summarize(min = min(value),
                                         `2.5_quant` = quantile(value, probs = c(0.025)),
                                         mean = mean(value),
                                         median = median(value),
                                         `97.5_quant` = quantile(value, probs = c(0.975)),
                                         max = max(value)))$mean

  firstquant_this_study <- (output_tib |>
                              dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                              tidyr::unnest(tot_Fe) |>
                              dplyr::summarize(min = min(value),
                                               `2.5_quant` = quantile(value, probs = c(0.025)),
                                               mean = mean(value),
                                               median = median(value),
                                               `97.5_quant` = quantile(value, probs = c(0.975)),
                                               max = max(value)))$`2.5_quant`
  lastquant_this_study <- (output_tib |>
                             dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                             tidyr::unnest(tot_Fe) |>
                             dplyr::summarize(min = min(value),
                                              `2.5_quant` = quantile(value, probs = c(0.025)),
                                              mean = mean(value),
                                              median = median(value),
                                              `97.5_quant` = quantile(value, probs = c(0.975)),
                                              max = max(value)))$`97.5_quant`

  tib_summary <- tibble::tibble(Releaser = c("Sperm whales (365d) (1)",
                                             "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (4))",
                                             "Antarctic blue whales (365d) (2)",
                                             "Leopard, crabeater, Weddell and Ross seals (365d) (this study)",
                                             "Antarctic blue whales (60-180d) (3)",
                                             "Humpback whales (60-180d) (3)",
                                             "Antarctic fin whales (60-180d) (3)",
                                             "Antarctic minke whales (60-180d) (3)"),
                                "Mean estimate" = c(50, 169, 65, mean_this_study, 15, 221, 367, 630),
                                first_quant = c(NA, NA, NA, firstquant_this_study, 9, 144, 193, 420),
                                last_quant = c(NA, NA, NA, lastquant_this_study, 24, 394, 590, 937))

  figure <- tib_summary|>
    dplyr::mutate(Releaser = factor(Releaser,
                                    levels = c("Sperm whales (365d) (1)",
                                               "Antarctic blue whales (365d) (2)",
                                               "Antarctic blue whales (60-180d) (3)",
                                               "Humpback whales (60-180d) (3)",
                                               "Antarctic fin whales (60-180d) (3)",
                                               "Antarctic minke whales (60-180d) (3)",
                                               "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (4))",
                                               "Leopard, crabeater, Weddell and Ross seals (365d) (this study)"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Releaser, y = `Mean estimate`,
                                   fill = Releaser),
                      stat = "identity", alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Releaser, y = `Mean estimate`,
                                     size = 2),
                        color = "gray40") +
    ggplot2::geom_errorbar(ggplot2::aes(x = Releaser, ymin = first_quant, ymax = last_quant),
                           color = "gray40",
                           width = 0, size = 1) +
    ggplot2::scale_fill_manual(values = wesanderson::wes_palette("FantasticFox1",
                                                                 8, # nb of areas
                                                                 type = "continuous")) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    ggplot2::ylim(c(0, 1000)) +
    ggplot2::xlab(" ") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 17),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 15),
                   axis.text.x = ggplot2::element_text(face = "bold", size = 15,
                                                       #angle = 45, hjust = 1
                   ))

  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                    width = 13,
                    height = 8)
    ggplot2::ggsave(paste0("output/", name_file, ".eps"),
                    width = 13,
                    height = 8,
                    dpi = 300
    )
    ggplot2::ggsave(paste0("output/", name_file, ".tiff"),
                    width = 13,
                    height = 8,
                    dpi = 300,
                    device='tiff')
  } else {
    figure
  }

}



#'
#'
#'
#'
#'
# function to generate figure displaying total amount of Fe released by
# the 4 species of pack-ice seals with results from this study emphasized
# from the intensity of its color
fig_tot_Fe_released_comp_alpha <- function(output_tib,
                                           alpha, # transparency for results
                                           # from other studies, range
                                           # 0 (transparent) to 1 (normal)
                                           name_file) {

  options(scipen = 999)

  mean_this_study <- (output_tib |>
                        dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                        tidyr::unnest(tot_Fe) |>
                        dplyr::summarize(min = min(value),
                                         `2.5_quant` = quantile(value, probs = c(0.025)),
                                         mean = mean(value),
                                         median = median(value),
                                         `97.5_quant` = quantile(value, probs = c(0.975)),
                                         max = max(value)))$mean

  firstquant_this_study <- (output_tib |>
                              dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                              tidyr::unnest(tot_Fe) |>
                              dplyr::summarize(min = min(value),
                                               `2.5_quant` = quantile(value, probs = c(0.025)),
                                               mean = mean(value),
                                               median = median(value),
                                               `97.5_quant` = quantile(value, probs = c(0.975)),
                                               max = max(value)))$`2.5_quant`
  lastquant_this_study <- (output_tib |>
                             dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                             tidyr::unnest(tot_Fe) |>
                             dplyr::summarize(min = min(value),
                                              `2.5_quant` = quantile(value, probs = c(0.025)),
                                              mean = mean(value),
                                              median = median(value),
                                              `97.5_quant` = quantile(value, probs = c(0.975)),
                                              max = max(value)))$`97.5_quant`

  tib_summary <- tibble::tibble(Releaser = c("Sperm whales (365d) (1)",
                                             "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (4))",
                                             "Antarctic blue whales (365d) (2)",
                                             "Leopard, crabeater, Weddell and Ross seals (365d) (this study)",
                                             "Antarctic blue whales (60-180d) (3)",
                                             "Humpback whales (60-180d) (3)",
                                             "Antarctic fin whales (60-180d) (3)",
                                             "Antarctic minke whales (60-180d) (3)"),
                                "Mean estimate" = c(50, 169, 65, mean_this_study, 15, 221, 367, 630),
                                first_quant = c(NA, NA, NA, firstquant_this_study, 9, 144, 193, 420),
                                last_quant = c(NA, NA, NA, lastquant_this_study, 24, 394, 590, 937),
                                alpha_set = c(alpha, alpha, alpha,
                                              1,
                                              alpha, alpha, alpha, alpha))

  figure <- tib_summary|>
    dplyr::mutate(Releaser = factor(Releaser,
                                    levels = c("Sperm whales (365d) (1)",
                                               "Antarctic blue whales (365d) (2)",
                                               "Antarctic blue whales (60-180d) (3)",
                                               "Humpback whales (60-180d) (3)",
                                               "Antarctic fin whales (60-180d) (3)",
                                               "Antarctic minke whales (60-180d) (3)",
                                               "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (4))",
                                               "Leopard, crabeater, Weddell and Ross seals (365d) (this study)"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Releaser, y = `Mean estimate`,
                                   fill = Releaser,
                                   alpha = alpha_set),
                      stat = "identity") +
    ggplot2::geom_point(ggplot2::aes(x = Releaser, y = `Mean estimate`,
                                     size = 2,
                                     #alpha = alpha_set
                                     ),
                        color = "gray40") +
    ggplot2::geom_errorbar(ggplot2::aes(x = Releaser,
                                        ymin = first_quant,
                                        ymax = last_quant,
                                        #alpha = alpha_set
                                        ),
                           color = "gray40",
                           width = 0, size = 1) +
    ggplot2::scale_alpha_identity() +
    ggplot2::scale_fill_manual(values = c("#1D2645FF", "#B4DAE5FF",
                                          "#DE7862FF", "#5A6F80FF",
                                          "#D8AF39FF", "#278B9AFF",
                                          "#AE93BEFF", "#E75B64FF")) +
    ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    ggplot2::ylim(c(0, 1000)) +
    ggplot2::xlab(" ") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 17),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 15),
                   axis.text.x = ggplot2::element_text(face = "bold", size = 15,
                                                       #angle = 45, hjust = 1
                   ))


  ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                  width = 13,
                  height = 8)
  ggplot2::ggsave(paste0("output/", name_file, ".eps"),
                  width = 13,
                  height = 8,
                  dpi = 300
  )
  ggplot2::ggsave(paste0("output/", name_file, ".tiff"),
                  width = 13,
                  height = 8,
                  dpi = 300,
                  device='tiff')


}






#'
#'
#'
#'
#'
# function to generate figure displaying total amount of Fe released by
# the 4 species of pack-ice seals with significance of diff
fig_sp_Fe_with_diff <- function(output_tib,
                                name_file) {

  options(scipen = 999)

  output_tib |>
    dplyr::group_by(Species) |>
    tidyr::unnest(excrete_Fe) |>
    dplyr::rename(excrete_Fe = value) |>
    dplyr::summarize(min = min(excrete_Fe),
                     `2.5_quant` = quantile(excrete_Fe, probs = c(0.025)),
                     mean = mean(excrete_Fe),
                     median = median(excrete_Fe),
                     `97.5_quant` = quantile(excrete_Fe, probs = c(0.975)),
                     max = max(excrete_Fe)) |>
    dplyr::mutate(Species_eng = dplyr::case_when(Species == "Hydrurga leptonyx" ~ "Leopard seals",
                                                 Species == "Lobodon carcinophaga" ~ "Crabeater seals",
                                                 Species == "Ommatophoca rossii" ~ "Ross seals",
                                                 Species == "Leptonychotes weddellii" ~ "Weddell seals"),
                  Species_eng = factor(Species_eng,
                                       levels = c("Leopard seals", "Crabeater seals",
                                                  "Weddell seals", "Ross seals"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Species_eng, y = mean,
                                   fill = Species_eng),
                      stat = "identity", alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Species_eng, y = mean),
                        color = "gray40",
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Species_eng, ymin = `2.5_quant`,
                                        ymax = `97.5_quant`),
                           color = "gray40",
                           width = 0, size = 1) +
    ggplot2::scale_fill_manual(values = wesanderson::wes_palette("Zissou1",
                                                                 4, # nb of areas
                                                                 type = "continuous")) +
    ggplot2::geom_text(label = c("a", "c", "b", "a"),
                       ggplot2::aes(y = `97.5_quant` + 20, x = Species_eng),
                       size = 4) +
    ggplot2::ylim(c(0, 350)) +
    ggplot2::xlab("Species") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(face = "bold",
                                                       #angle = 20, hjust = 1,
                                                       size = 12))

  ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                  width = 7,
                  height = 4)
  ggplot2::ggsave(paste0("output/", name_file, ".eps"),
                  width = 7,
                  height = 4,
                  dpi = 300)
  ggplot2::ggsave(paste0("output/", name_file, ".tiff"),
                  width = 7,
                  height = 4,
                  dpi = 300,
                  device='tiff')


}



#'
#'
#'
#'
#'
#' function to generate figure displaying total amount of Fe released by the
#' 4 species of pack-ice seals
fig_sp_Fe_released <- function(output_tib,
                               object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                               name_file) {

  options(scipen = 999)

  figure <- output_tib |>
    dplyr::group_by(Species) |>
    tidyr::unnest(excrete_Fe) |>
    dplyr::rename(excrete_Fe = value) |>
    dplyr::summarize(min = min(excrete_Fe),
                     `2.5_quant` = quantile(excrete_Fe, probs = c(0.025)),
                     mean = mean(excrete_Fe),
                     median = median(excrete_Fe),
                     `97.5_quant` = quantile(excrete_Fe, probs = c(0.975)),
                     max = max(excrete_Fe)) |>
    dplyr::mutate(Species_eng = dplyr::case_when(Species == "Hydrurga leptonyx" ~ "Leopard seals",
                                                 Species == "Lobodon carcinophaga" ~ "Crabeater seals",
                                                 Species == "Ommatophoca rossii" ~ "Ross seals",
                                                 Species == "Leptonychotes weddellii" ~ "Weddell seals"),
                  Species_eng = factor(Species_eng,
                                       levels = c("Leopard seals", "Crabeater seals",
                                                  "Weddell seals", "Ross seals"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x = Species_eng, y = mean,
                                   fill = Species_eng),
                      stat = "identity", alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(x = Species_eng, y = mean),
                        color = "gray40",
                        size = 2) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Species_eng, ymin = `2.5_quant`,
                                        ymax = `97.5_quant`),
                           color = "gray40",
                           width = 0, size = 1) +
    ggplot2::scale_fill_manual(values = wesanderson::wes_palette("Zissou1",
                                                                 4, # nb of areas
                                                                 type = "continuous")) +
    ggplot2::ylim(c(0, 350)) +
    ggplot2::xlab("Species") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(face = "bold",
                                                       #angle = 20, hjust = 1,
                                                       size = 12))

  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                    width = 7,
                    height = 4)
    ggplot2::ggsave(paste0("output/", name_file, ".eps"),
                    width = 7,
                    height = 4,
                    dpi = 300)
  } else {
    figure
  }

}



#'
#'
#'
#'
#'
#' function to generate table with species - specific population iron release statistics
#' used in Table 3 of the article together with individual iron consumption and release
#' parameters provided in Supp table 1
table_Fe_release_sp <- function(output_tib,
                                object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                name_file) {

  options(scipen = 999)

  table <- output_tib |>
    dplyr::group_by(Species) |>
    tidyr::unnest(excrete_Fe) |>
    dplyr::rename(excrete_Fe = value) |>
    dplyr::summarize(min = min(excrete_Fe),
                     `2.5_quant` = quantile(excrete_Fe, probs = c(0.025)),
                     mean = mean(excrete_Fe),
                     median = median(excrete_Fe),
                     `97.5_quant` = quantile(excrete_Fe, probs = c(0.975)),
                     max = max(excrete_Fe))

  if (object_type == "file") {
    openxlsx::write.xlsx(table,
                         file =paste0("output/", name_file, ".xlsx"))
  } else {
    table
  }

}


#'
#'
#'
#'
#'
#' function to generate supplementary material table with all parameters summary values
supp_table_param <- function(output_tib,
                             object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                             name_file) {

  options(scipen = 999)

  table <- output_tib |>
    dplyr::group_by(Species) |>
    tidyr::unnest(Abund) |>
    dplyr::summarize(min = min(value),
                     `2.5_quant` = quantile(value, probs = c(0.025)),
                     mean = mean(value),
                     median = median(value),
                     `97.5_quant` = quantile(value, probs = c(0.975)),
                     max = max(value)) |>
    dplyr::mutate(Parameter = "Population abundance") |>
    # next parameters
    dplyr::bind_rows(output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(Fe_exc) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Fe release rate"),
                     output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(Mass) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Body mass"),
                     output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(Beta) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Beta"),
                     output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(NRJ_diet) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "E"),
                     output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(Fe_diet) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "x_Fe"),
                     output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(Indi_data) |>
                       tidyr::pivot_longer(cols = c(ADMR:`PercentBM`),
                                           names_to = "Parameter",
                                           values_to = "value") |>
                       dplyr::group_by(Species, `Parameter`) |>
                       dplyr::mutate(Parameter = dplyr::case_when(Parameter == "A_rate" ~ "Assimilation rate",
                                                                  Parameter == "PercentBM" ~ "% of body mass (daily ration)",
                                                                  Parameter == "Ration" ~ "Daily ration (kg)",
                                                                  Parameter == "ADMR" ~ "Average Daily Metabolic Rate (kJ)")) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)),
                     output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(conso_Fe_ind) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Individual daily amount of Fe ingested (mg)"),
                     output_tib |>
                       dplyr::group_by(Species) |>
                       tidyr::unnest(excrete_Fe_ind) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Individual daily amount of Fe released (mg)")
    )

  if (object_type == "file") {
    openxlsx::write.xlsx(table,
                         file =paste0("output/", name_file, ".xlsx"))
  } else {
    table
  }

}


#'
#'
#'
#'
#'
# function to generate supplementary table with data from other studies
# on total yearly Fe release by other megafauna taxa
supp_table_comp <- function(output_tib,
                            name_file) {

  options(scipen = 999)

  summary_this_study <- output_tib |>
    dplyr::group_by(Species) |>
    dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
    tidyr::unnest(tot_Fe) |>
    dplyr::group_by(Species) |>
    dplyr::summarize(min = min(value),
                     `2.5_quant` = quantile(value, probs = c(0.025)),
                     mean = mean(value),
                     median = median(value),
                     `97.5_quant` = quantile(value, probs = c(0.975)),
                     max = max(value))

  means_this_study <- round(summary_this_study$mean, 1)

  firstquant_this_study <- round(summary_this_study$`2.5_quant`, 1)
  lastquant_this_study <- round(summary_this_study$`97.5_quant`, 1)

  CI_this_study <- c(paste(firstquant_this_study[1],
                           lastquant_this_study[1],
                           sep = " - "),
                     paste(firstquant_this_study[2],
                           lastquant_this_study[2],
                           sep = " - "),
                     paste(firstquant_this_study[3],
                           lastquant_this_study[3],
                           sep = " - "),
                     paste(firstquant_this_study[4],
                           lastquant_this_study[4],
                           sep = " - "))

  tib_summary <- tibble::tibble(Species = c("Sperm whales",
                                            "Antarctic blue whales",
                                            "Antarctic blue whales",
                                            "Humpback whales",
                                            "Antarctic fin whales",
                                            "Antarctic minke whales",
                                            "Chinstrap, Adelie and Gentoo penguins",
                                            "Leopard seals",
                                            "Weddell seals",
                                            "Crabeater seals",
                                            "Ross seals"),
                                "Nb of days of presence considered in the study" = c("365",
                                                                                     "365",
                                                                                     "60-180",
                                                                                     "60-180",
                                                                                     "60-180",
                                                                                     "60-180",
                                                                                     "365",
                                                                                     "365",
                                                                                     "365",
                                                                                     "365",
                                                                                     "365"),
                                "Mean Fe yearly release (t/yr)" = c(50, 65, 15, 221, 367, 630, 169, means_this_study),
                                "Confidence interval" = c(NA, NA,
                                                          "9-24", "144-394", "193-590", "420-937",
                                                          NA, CI_this_study
                                ),
                                Reference = c("Lavery et al. 2010",
                                              "Lavery et al. 2014",
                                              "Savoca et al. 2021",
                                              "Savoca et al. 2021",
                                              "Savoca et al. 2021",
                                              "Savoca et al. 2021",
                                              "deduced from Sparaventi et al. 2021",
                                              "this study",
                                              "this study",
                                              "this study",
                                              "this study"
                                )
  )



  openxlsx::write.xlsx(tib_summary,
                       file =paste0("output/", name_file, ".xlsx"))

}



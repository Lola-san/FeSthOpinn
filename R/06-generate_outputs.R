##--------------------------------------------------------------------------------------------------------
## SCRIPT : generate output figures and tables
## 06-generate_outputs.R
##
## Authors : Lola Gilbert
## Last update : 2022-09
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------
# script with function to run the model

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
    ggplot2::geom_errorbar(ggplot2::aes(x = Param, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Param),
                           size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = Param, y = mean, color = Param)) +
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
#' function to generate figure displaying total amount of Fe released by the 4 species of pack-ice seals
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
                                             "Chinstrap, Adelie and Gentoo penguins (165d) (4)",
                                             "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (4))",
                                             "Blue whales (365d) (2)",
                                             "Crabeater, Weddell, Ross and leopard seals (365d) (this study)",
                                             "All SO mysticetes (60-180d) (3)"),
                                "Mean estimate" = c(50, 56, 169, 65, mean_this_study, 1200),
                                first_quant = c(NA, NA, NA, NA, firstquant_this_study, NA),
                                last_quant = c(NA, NA, NA, NA, lastquant_this_study, NA),)

  figure <- tib_summary|>
    dplyr::mutate(Releaser = factor(Releaser,
                                    levels = c("Sperm whales (365d) (1)",
                                               "Blue whales (365d) (2)",
                                               "All SO mysticetes (60-180d) (3)",
                                               "Chinstrap, Adelie and Gentoo penguins (165d) (4)",
                                               "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (4))",
                                               "Crabeater, Weddell, Ross and leopard seals (365d) (this study)"))) |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = Releaser, y = `Mean estimate`, color = Releaser)) +
    ggplot2::geom_errorbar(ggplot2::aes(x = Releaser, ymin = first_quant, ymax = last_quant, color = Releaser),
                           size = 1) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1",
                                                                  6, # nb of areas
                                                                  type = "continuous")) +
    ggplot2::ylim(c(0, 1250)) +
    ggplot2::xlab(" ") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(face = "bold", size = 12, angle = 20, hjust = 1))

  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                    width = 12,
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
#' function to generate figure displaying total amount of Fe released by the 4 species of pack-ice seals
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
    ggplot2::ggplot() +
    ggplot2::geom_errorbar(ggplot2::aes(x = Species, ymin = `2.5_quant`, ymax = `97.5_quant`, color = Species),
                           size = 1) +
    ggplot2::geom_point(ggplot2::aes(x = Species, y = mean, color = Species)) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1",
                                                                  4, # nb of areas
                                                                  type = "continuous")) +
    ggplot2::ylim(c(0, 500)) +
    ggplot2::xlab("Species") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(face = "italic", angle = 20, hjust = 1, size = 12))

  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                    width = 7,
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
#' function to generate supplementary material table with all parameters summary values
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
                                                                  Parameter == "PercentBM" ~ "% of body mass",
                                                                  Parameter == "Ration" ~ "Daily ration (kg)",
                                                                  TRUE ~ Parameter)) |>
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



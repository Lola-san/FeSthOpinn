##--------------------------------------------------------------------------------------------------------
## SCRIPT : Define diets of pinnipeds species
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
    ggplot2::annotate("text", x = 0.75, y = 274, label = "mean = 274") +
    ggplot2::annotate("text", x = 0.75, y = 150, label = "quantile 2.5% = 134") +
    ggplot2::annotate("text", x = 0.75, y = 525, label = "quantile 97.5% = 508") +
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

  tib_summary <- tibble::tibble(Releaser = c("Sperm whales (1)",
                                             "Chinstrap, Adelie and Gentoo penguins (165d) (2)",
                                             "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (2))",
                                             "Blue whales (3)",
                                             "Crabeater, Weddell, Ross and leopard seals (this study)",
                                             "All SO mysticetes (4)"),
                                "Mean estimate" = c(50, 56, 169, 65, mean_this_study, 1200))

  figure <- tib_summary|>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = Releaser, y = `Mean estimate`, color = Releaser)) +
    ggplot2::scale_color_manual(values = wesanderson::wes_palette("FantasticFox1",
                                                                  6, # nb of areas
                                                                  type = "continuous")) +
    ggplot2::ylim(c(0, 1250)) +
    ggplot2::xlab(" ") +
    ggplot2::ylab("Fe released (in t/yr)") +
    ggplot2::theme(legend.position = "none",
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(face = "bold", size = 12),
                   axis.text.x = ggplot2::element_text(face = "bold", size = 12, angle = 20, hjust = 1))

  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                    width = 9,
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
                       tidyr::unnest(Nb_days) |>
                       dplyr::summarize(min = min(value),
                                        `2.5_quant` = quantile(value, probs = c(0.025)),
                                        mean = mean(value),
                                        median = median(value),
                                        `97.5_quant` = quantile(value, probs = c(0.975)),
                                        max = max(value)) |>
                       dplyr::mutate(Parameter = "Number of days of release during a year"),
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



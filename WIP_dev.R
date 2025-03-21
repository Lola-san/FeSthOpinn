##--------------------------------------------------------------------------------------------------------
## SCRIPT : working script - trying things out here to then translate them into function etc etc
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# clean environment
rm(list = ls())


### composition of diet mean values
data <- load_xl(pathxl = "data/prey_compo_compiled.xlsx")

data |>
  dplyr::mutate(Taxa = dplyr::case_when(Taxa %in% c("Krill", "Other zooplankton") ~ "Krill & other zooplankton",
                                        TRUE ~ Taxa)) |>
  dplyr::filter(Taxa %in% c("Fish", "Cephalopod", "Krill & other zooplankton", "Pinniped (muscle)", "Penguins (muscle)")) |>
  dplyr::group_by(Taxa) |>
  dplyr::summarise(mean_nrj = mean(NRJ_ww, na.rm = TRUE),
                  sd_nrj = sd(NRJ_ww, na.rm = TRUE),
                  sd_nrj_percent_mean = sd_nrj/mean_nrj,
                   mean_Fe = mean(`Fe_ww_mg_kg-1`, na.rm = TRUE),
                  sd_Fe = sd(`Fe_ww_mg_kg-1`, na.rm = TRUE),
                  sd_Fe_percent_mean = sd_Fe/mean_Fe )

targets::tar_load(diet_input)

trial <- diet_input |>
  dplyr::mutate(Diet_compo = seq_along(Diet) |>
                  purrr::map(~ purrr::pluck(Diet, .) |>
                               dplyr::rename("Krill & other zooplankton" = Krill,
                                             "Pinniped (muscle)" = Mammal,
                                             "Penguins (muscle)" = Bird) |>
                               tidyr::pivot_longer(cols = c("Fish", "Cephalopod", "Krill & other zooplankton",
                                               "Pinniped (muscle)", "Penguins (muscle)"),
                                      names_to = "Taxa",
                                      values_to = "%W") |>
                               dplyr::left_join(data |>
                                                  dplyr::mutate(Taxa = dplyr::case_when(Taxa %in% c("Krill", "Other zooplankton") ~ "Krill & other zooplankton",
                                                                                        TRUE ~ Taxa)) |>
                                                  dplyr::filter(Taxa %in% c("Fish", "Cephalopod", "Krill & other zooplankton", "Pinniped (muscle)", "Penguins (muscle)")) |>
                                                  dplyr::group_by(Taxa) |>
                                                  dplyr::summarise(NRJ_kJ.kg = mean(NRJ_ww, na.rm = TRUE),
                                                                   Fe_mg.kg = mean(`Fe_ww_mg_kg-1`, na.rm = TRUE))  |>
                                                  # set NRJ content of pinnipeds and penguins to 10
                                                  dplyr::mutate(`NRJ_kJ.kg`= dplyr::case_when(Taxa %in% c("Pinniped (muscle)",
                                                                                                         "Penguins (muscle)") ~ 10,
                                                                                             TRUE ~ `NRJ_kJ.kg`))
                                                  ) |>
                               dplyr::mutate(WNRJ = `%W` * `NRJ_kJ.kg`,
                                             WFe = `%W` * `Fe_mg.kg`) |>
                               dplyr::summarise(mean_NRJ_diet = sum(WNRJ),
                                                mean_Fe_diet = sum(WFe))
                             )
                ) |>
  tidyr::unnest(Diet_compo)



############### trying things out with figures

sum_vec <- function(list_of_vec) {
  summed_vec <- rep(0, length(list_of_vec[[1]]))

  for (j in seq_along(list_of_vec)) {
    summed_vec <- summed_vec + list_of_vec[[j]]
  }
  return(summed_vec)
}



options(scipen = 999)

mean_this_study <- (model_output |>
                      dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                      tidyr::unnest(tot_Fe) |>
                      dplyr::summarize(min = min(value),
                                       `2.5_quant` = quantile(value, probs = c(0.025)),
                                       mean = mean(value),
                                       median = median(value),
                                       `97.5_quant` = quantile(value, probs = c(0.975)),
                                       max = max(value)))$mean

firstquant_this_study <- (model_output |>
                            dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
                            tidyr::unnest(tot_Fe) |>
                            dplyr::summarize(min = min(value),
                                             `2.5_quant` = quantile(value, probs = c(0.025)),
                                             mean = mean(value),
                                             median = median(value),
                                             `97.5_quant` = quantile(value, probs = c(0.975)),
                                             max = max(value)))$`2.5_quant`
lastquant_this_study <- (model_output |>
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
                                           "Crabeater, Weddell, Ross and leopard seals (365d) (this study)",
                                           "Antarctic blue whales (60-180d) (3)",
                                           "Humpback whales (60-180d) (3)",
                                           "Antarctic fin whales (60-180d) (3)",
                                           "Antarctic minke whales (60-180d) (3)"),
                              "Mean estimate" = c(50, 169, 65, mean_this_study, 15, 221, 367, 630),
                              first_quant = c(NA, NA, NA, firstquant_this_study, 9, 144, 193, 420),
                              last_quant = c(NA, NA, NA, lastquant_this_study, 24, 394, 590, 937))

tib_summary|>
  dplyr::mutate(Releaser = factor(Releaser,
                                  levels = c("Sperm whales (365d) (1)",
                                             "Antarctic blue whales (365d) (2)",
                                             "Antarctic blue whales (60-180d) (3)",
                                             "Humpback whales (60-180d) (3)",
                                             "Antarctic fin whales (60-180d) (3)",
                                             "Antarctic minke whales (60-180d) (3)",
                                             "Chinstrap, Adelie and Gentoo penguins (365d) (deduced from (4))",
                                             "Crabeater, Weddell, Ross and leopard seals (365d) (this study)"))) |>
  ggplot2::ggplot() +
  ggplot2::geom_bar(ggplot2::aes(x = Releaser, y = `Mean estimate`, fill = Releaser),
                    stat = "identity",
                    #fill = "skyblue",
                    alpha = 0.7) +
  ggplot2::geom_point(ggplot2::aes(x = Releaser, y = `Mean estimate`),
                      color = "gray40", size = 2) +
  ggplot2::geom_errorbar(ggplot2::aes(x = Releaser, ymin = first_quant, ymax = last_quant),
                         color = "gray40",
                         width = .5, size = 1) +
  ggplot2::scale_fill_manual(values = wesanderson::wes_palette("FantasticFox1",
                                                                9, # nb of areas
                                                                type = "continuous")) +
  ggplot2::ylim(c(0, 1250)) +
  ggplot2::xlab(" ") +
  ggplot2::ylab("Fe released (in t/yr)") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "none",
                 axis.title.y = ggplot2::element_text(face = "bold", size = 17),
                 axis.text.y = ggplot2::element_text(face = "bold", size = 15),
                 axis.text.x = ggplot2::element_text(face = "bold", size = 15, angle = 45, hjust = 1))




##### output$
targets::tar_load(model_output)

sum_vec <- function(list_of_vec) {
  summed_vec <- rep(0, length(list_of_vec[[1]]))

  for (j in seq_along(list_of_vec)) {
    summed_vec <- summed_vec + list_of_vec[[j]]
  }
  return(summed_vec)
}

sum_tib <- tibble::tibble(min = (sum_vec(list(purrr::pluck(model_output, "excrete_Fe_min", 1),
                                        purrr::pluck(model_output, "excrete_Fe_min", 2),
                                        purrr::pluck(model_output, "excrete_Fe_min", 3),
                                        purrr::pluck(model_output, "excrete_Fe_min", 4))))$value,
                          max = (sum_vec(list(purrr::pluck(model_output, "excrete_Fe_max", 1),
                                        purrr::pluck(model_output, "excrete_Fe_max", 2),
                                        purrr::pluck(model_output, "excrete_Fe_max", 3),
                                        purrr::pluck(model_output, "excrete_Fe_max", 4))))$value,
                          mean = (sum_vec(list(purrr::pluck(model_output, "excrete_Fe_mean", 1),
                                        purrr::pluck(model_output, "excrete_Fe_mean", 2),
                                        purrr::pluck(model_output, "excrete_Fe_mean", 3),
                                        purrr::pluck(model_output, "excrete_Fe_mean", 4))))$value)

hist(sum_tib$min)

summary(sum_tib$min)


hist(sum_tib$mean)

summary(sum_tib$mean)

hist(sum_tib$max)

summary(sum_tib$max)


sum_tib |>
  tidyr::pivot_longer(cols = "min":"mean",
                      names_to = "estimate",
                      values_to = "Fe_exc") |>
  ggplot2::ggplot() +
  ggplot2::geom_histogram(ggplot2::aes(x = Fe_exc, fill = estimate))



# to simulate abundance uncertainty
abundance <- function(abund_bar,
                      abund_min, abund_max,
                      n_sim){
  abund_sd <- (abund_max - abund_min)/4
  abund_cv <- abund_sd/abund_bar
  sigma <- sqrt(log1p(abund_cv*abund_cv))
  mu <- log(abund_bar/sqrt(1+abund_cv*abund_cv))
  return(tibble::as_tibble_col(rlnorm(n_sim, mu, sigma)))
}

#hydrlep
hist(abundance(abund_bar = 35500, abund_min = 10900, abund_max = 102600,
               n_sim = 100)$value)
summary(abundance(abund_bar = 35500, abund_min = 10900, abund_max = 102600,
                  n_sim = 100)$value)

hist(truncnorm::rtruncnorm(n = 100,
                           a = 10900,
                           b = 102600,
                           mean = 35500,
                           sd = 22925))
summary(truncnorm::rtruncnorm(n = 100,
                           a = 10900,
                           b = 102600,
                           mean = 35500,
                           sd = 22925))


#lobocar
hist(abundance(abund_bar = 5869400, abund_min = 3699400, abund_max = 8616700,
               n_sim = 100)$value)
summary(abundance(abund_bar = 5869400, abund_min = 3699400, abund_max = 8616700,
                  n_sim = 100)$value)

hist(truncnorm::rtruncnorm(n = 100,
                           a = 3699400,
                           b = 8616700,
                           mean = 5869400,
                           sd = (8616700 - 3699400)/4))
summary(truncnorm::rtruncnorm(n = 100,
                              a = 3699400,
                              b = 8616700,
                              mean = 5869400,
                              sd = (8616700 - 3699400)/4))

#ommaros
hist(abundance(abund_bar = 78500, abund_min = 39400, abund_max = 231200,
               n_sim = 100)$value)
summary(abundance(abund_bar = 78500, abund_min = 39400, abund_max = 231200,
                  n_sim = 100)$value)


hist(truncnorm::rtruncnorm(n = 100,
                           a = 39400,
                           b = 231200,
                           mean = 78500,
                           sd = (231200 - 39400)/4))
summary(truncnorm::rtruncnorm(n = 100,
                              a = 39400,
                              b = 231200,
                              mean = 78500,
                              sd = (231200 - 39400)/4))


#leptwed
hist(abundance(abund_bar = 202000, abund_min = 85345, abund_max = 523140,
               n_sim = 100)$value)
summary(abundance(abund_bar = 202000, abund_min = 85345, abund_max = 523140,
                  n_sim = 100)$value)

hist(truncnorm::rtruncnorm(n = 100,
                           a = 85345,
                           b = 523140,
                           mean = 202000,
                           sd = (523140 - 85345)/4))
summary(truncnorm::rtruncnorm(n = 100,
                              a = 85345,
                              b = 523140,
                              mean = 202000,
                              sd = (523140 - 85345)/4))



### OUTPUT

targets::tar_load(model_output)

sum_vec <- function(list_of_vec) {
  summed_vec <- rep(0, length(list_of_vec[[1]]))

  for (j in seq_along(list_of_vec)) {
    summed_vec <- summed_vec + list_of_vec[[j]]
  }
  return(summed_vec)
}

model_output |>
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
                                                                type = "continuous"),
                              name = "Species") +
  ggplot2::xlab("Species") +
  ggplot2::ylab("Fe release (in t/yr)") +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(face = "italic", angle = 20, hjust = 1, size = 12))

model_output |>
  dplyr::summarise(tot_Fe = list(sum_vec(excrete_Fe))) |>
  tidyr::unnest(tot_Fe) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value))

model_output |>
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
  ggplot2::scale_x_discrete(labels = c("0", "100", "200", "300", "400", "500", "600"),
                            breaks = c(0, 100, 200, 300, 400, 500, 600)) +
  ggplot2::xlab("Total Fe released by crabeater, Weddell, Ross and leopard seals") +
  ggplot2::ylab("Fe released (in t/yr)") +
  ggplot2::theme(legend.position = "none",
                 axis.text.x = ggplot2::element_text(face = "bold", size = 12))


model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(excrete_Fe) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value)) |>
  dplyr::mutate(Parameter = "Fe release")

# create table
model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(excrete_Fe) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value)) |>
  dplyr::mutate(Parameter = "Fe release") |>
  dplyr::bind_rows(model_output |>
                     dplyr::group_by(Species) |>
                     tidyr::unnest(Abund) |>
                     dplyr::summarize(min = min(value),
                                      `2.5_quant` = quantile(value, probs = c(0.025)),
                                      mean = mean(value),
                                      median = median(value),
                                      `97.5_quant` = quantile(value, probs = c(0.975)),
                                      max = max(value)) |>
                     dplyr::mutate(Parameter = "Population abundance"))

model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(Abund) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value))

model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(Mass) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value))

model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(Beta) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value))

model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(NRJ_diet) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value))

model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(Fe_diet) |>
  dplyr::summarize(min = min(value),
                   `2.5_quant` = quantile(value, probs = c(0.025)),
                   mean = mean(value),
                   median = median(value),
                   `97.5_quant` = quantile(value, probs = c(0.975)),
                   max = max(value))


model_output |>
  dplyr::group_by(Species) |>
  tidyr::unnest(conso_diet_ind) |>
  tidyr::pivot_longer(cols = c(Fish:`Penguins (muscle)`),
                      names_to = "Prey group",
                      values_to = "Daily consumption (kg)") |>
  dplyr::group_by(Species, `Prey group`) |>
  dplyr::summarize(min = min(`Daily consumption (kg)`),
                   `2.5_quant` = quantile(`Daily consumption (kg)`, probs = c(0.025)),
                   mean = mean(`Daily consumption (kg)`),
                   median = median(`Daily consumption (kg)`),
                   `97.5_quant` = quantile(`Daily consumption (kg)`, probs = c(0.975)),
                   max = max(`Daily consumption (kg)`))


model_output |>
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
                   max = max(value))

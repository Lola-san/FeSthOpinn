##--------------------------------------------------------------------------------------------------------
## SCRIPT : working script - trying things out here to then translate them into function etc etc
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# clean environment
rm(list = ls())

# create tibble with species and their respective diets

diet_tibb <- tibble::tribble(~ Species, ~ Fish, ~ Cephalopod, ~ Krill, ~ Mammal, ~ Bird, ~ Sources,
                             "Hydrurga leptonyx", 0.25, 0, 0.25, 0.25, 0.25, "Green & Williams 1986, Walker et al 1998, Zhao et al 2004,
                            Hall-Aspland et al 2004, Hall-Aspland et al 2005, Kuhn et al 2006, Forcada et al 2009, Casaux et al 2009,
                            Southwell et al 2012",
                             "Lobodon carcinophaga", 0.05, 0.05, 0.9, 0, 0, "Green & Williams 1986, Zhao et al 2004, Dykyy 2009,
                            Southwell et al 2012, Brault et al 2019",
                             "Ommatophoca rossii", 0.25, 0.65, 0.1, 0, 0, "Skinner et al 1994, Zhao et al 2004, Blix & Nordoy 2007,
                            Southwell et al 2012, Brault et al 2019, Wege et al 2021",
                             "Leptonychotes weddellii", 0.85, 0.18, 0.02, 0, 0, "Casaux et al 1997, Lake et al 2003, Zhao et al 2004,
                            Casaux et al 2006,  Southwell et al 2012, Negri et al 2016, Goetz et al 2017, Hückstädt et al 2017,
                            Brault et al 2019") |>
  tidyr::nest(Diet = c(Fish:Bird))


# load nutrient in preys tibbles
fish_ceph <- readxl::read_excel("data/Nuts_in_preys.xlsx")
krill_bird_mam <- readxl::read_excel("data/Fe_in_preys.xlsx")

# clean up first file : we want only Fish and Ceph from it and we want only Fe (and NRJ...)
head(fish_ceph)
unique(fish_ceph$Taxa)

fish_ceph <- fish_ceph |>
  dplyr::filter(Taxa %in% c("Fish", "Cephalopod")) |>
  dplyr::rename(Species = Sp_prey) |>
  dplyr::select(Taxa, Species, NRJ, Fe)

# clean up second file
head(krill_bird_mam)
colnames(krill_bird_mam)

krill_bird_mam |>
  dplyr::filter(Taxa %in% c("Krill", "Other zooplankton")) |>
  dplyr::summarise(mean_NRJ = mean(nrj_ww_KJ_g, na.rm = TRUE))

krill_bird_mam |>
  dplyr::filter(Taxa %in% c("Penguins", "Seal")) |>
  dplyr::summarise(mean_NRJ = mean(nrj_ww_KJ_g, na.rm = TRUE))

krill_bird_mam <- krill_bird_mam |>
  dplyr::rename(Fe = Fe_mean_ww_mg_kg,
                NRJ = nrj_ww_KJ_g) |>
  # keep only values of concentration in muscle for seals and penguins (and na for krill)
  dplyr::filter(Tissue %in% c("Muscle", NA)) |>
  # define NRJ densities values (mean of values from litterature for Krill, total guess for Penguins & seals)
  dplyr::mutate(NRJ = dplyr::case_when(Taxa %in% c("Krill", "Other zooplankton") ~ 5.04,
                                Taxa %in% c("Penguins", "Seal") ~ 10,
                                TRUE ~ NRJ)) |>
  dplyr::select(Taxa, Species, NRJ, Fe)

# bind the two tables
compo_preys <- rbind(fish_ceph, krill_bird_mam) |>
  # there are zooplankton lines that were used to compute mean of nrj but with no Fe concentrations
  dplyr::filter(!(is.na(Fe))) |>
  # adapt taxa name with those used for definition of diet
  dplyr::mutate(Taxa = dplyr::case_when(Taxa == "Other zooplankton" ~ "Krill", # in this first approach
                                        Taxa == "Seal" ~ "Mammal",
                                        Taxa == "Penguins" ~ "Bird",
                                        TRUE ~ Taxa))


compo_preys |>
  dplyr::group_by(Taxa) |>
  # bootstrap : sample nsim species from each taxa
  dplyr::slice_sample(n = 1e2, replace = TRUE)




### composition of diet mean values
data <- load_xl(pathxl = "data/prey_compo_compiled.xlsx")

data |>
  dplyr::mutate(Taxa = dplyr::case_when(Taxa %in% c("Krill", "Other zooplankton") ~ "Krill & other zooplankton",
                                        TRUE ~ Taxa)) |>
  dplyr::filter(Taxa %in% c("Fish", "Cephalopod", "Krill & other zooplankton", "Pinniped (muscle)", "Penguins (muscle)")) |>
  dplyr::group_by(Taxa) |>
  dplyr::summarise(mean_nrj = mean(NRJ_ww, na.rm = TRUE),
                  sd_nrj = sd(NRJ_ww, na.rm = TRUE),
                   mean_Fe = mean(`Fe_ww_mg_kg-1`, na.rm = TRUE),
                  sd_Fe = sd(`Fe_ww_mg_kg-1`, na.rm = TRUE) )

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

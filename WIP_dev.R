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

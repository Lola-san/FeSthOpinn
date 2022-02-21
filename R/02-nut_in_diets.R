##--------------------------------------------------------------------------------------------------------
## SCRIPT : Compute Fe and nrj contents of diets
## 02-nuts_in_diets.R
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# This script should contain FUNCTIONS ONLY

# dataset should be loaded with load_xl function defined in script 00

#'
#'
#'
#'
#'
#'
clean_bind_compo_tibbles <- function(Bob_dataset, Ant_dataset){
  ### clean up ###
  # first Bay of Biscay dataset
  # we want only Fish and Ceph from it and we want only Fe (and NRJ...)
  fish_ceph <- Bob_dataset |>
    dplyr::filter(Taxa %in% c("Fish", "Cephalopod")) |>
    dplyr::rename(Species = Sp_prey) |>
    dplyr::select(Taxa, Species, NRJ, Fe)

  # second dataset with data on krill, seals and penguins
  krill_bird_mam <- Ant_dataset |>
    dplyr::rename(Fe = Fe_mean_ww_mg_kg,
                  NRJ = nrj_ww_KJ_g) |>
    # keep only values of concentration in muscle for seals and penguins (and na for krill)
    dplyr::filter(Tissue %in% c("Muscle", NA)) |>
    # define NRJ densities values (mean of values from litterature for Krill, total guess for Penguins & seals)
    dplyr::mutate(NRJ = dplyr::case_when(Taxa %in% c("Krill", "Other zooplankton") ~ 5.04,
                                         Taxa %in% c("Penguins", "Seal") ~ 10,
                                         TRUE ~ NRJ)) |>
    dplyr::select(Taxa, Species, NRJ, Fe)

  ### bind the two tables ###
  rbind(fish_ceph, krill_bird_mam) |>
    # there are zooplankton lines that were used to compute mean of nrj but with no Fe concentrations
    dplyr::filter(!(is.na(Fe))) |>
    # adapt taxa name with those used for definition of diet
    dplyr::mutate(Taxa = dplyr::case_when(Taxa == "Other zooplankton" ~ "Krill", # in this first approach
                                          Taxa == "Seal" ~ "Mammal",
                                          Taxa == "Penguins" ~ "Bird",
                                          TRUE ~ Taxa))

}


#'
#'
#'
#'
#'
#'
######################## bootstrapping ########################################
# function to bootstrap the composition of each prey group
# i.e sample with replacement samples of each prey group
# to simulate a larger dataset that we have,
# here we artificially generate nsim sample per prey group
bootstrap_compo <- function(compo_tib, nsim) {

  compo_tib |>
    dplyr::group_by(Taxa) |>
    # bootstrap : sample nsim species from each taxa
    dplyr::slice_sample(n = nsim, replace = TRUE)

}


#'
#'
#'
#'
#'
##################### compute nutrient content of diet ########################
compute_nut_in_diet <- function(diet_tibb, compo_tibb_boot) {

  diet_tibb |>
    # 1 - join the nut concentration bootstrapped tables per Taxa to the %W of each prey
    dplyr::mutate(Nut_W = seq_along(Diet) |>
                    purrr::map(~ purrr::pluck(Diet, .) |>
                                 tidyr::pivot_longer(cols = c('Fish':'Bird'),
                                                     names_to = "Taxa",
                                                     values_to = "W") |>
                                 # add column with %W in diet associated to each taxa, for each pred (ie.line)
                                 dplyr::left_join(y = compo_tibb_boot |>
                                                    dplyr::select(-c(Species)),
                                                  by = "Taxa")
                    )) |>
    # 2 - compute W*elemental concentration
    dplyr::mutate(
      Nut_W = seq_along(Nut_W) |>
        purrr::map(~ purrr::pluck(Nut_W, .) |>
                     dplyr::mutate(NRJ = NRJ*(W/100),
                                   Fe = Fe*(W/100)) |>
                     # change it to get one column per taxa
                     # and one line, each cell containing a full bootstrap tibble
                     # of elemental concentration, size nsim*nelements
                     dplyr::select(-c(W)) |>
                     tidyr::nest(Nut = c("NRJ":"Fe")) |>
                     tidyr::pivot_wider(names_from = Taxa,
                                        values_from = Nut)
                     ),
      # 3 - compute the mean concentration of diet by summing these values across taxa
      Nut_diet = seq_along(Nut_W) |>
        purrr::map(~ purrr::pluck(Nut_W, ., 1, 1) +
                     purrr::pluck(Nut_W, ., 2, 1) +
                     purrr::pluck(Nut_W, ., 3, 1) +
                     purrr::pluck(Nut_W, ., 4, 1) +
                     purrr::pluck(Nut_W, ., 5, 1) ),
      # NRJ should be a separated column as it will be used to compute the daily ration
      NRJ_diet = seq_along(Nut_diet) |>
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Nut_diet, ., "NRJ")*1e3)), # from kJ per g to kJ per kg
      # same for Fe
      Fe_diet = seq_along(Nut_diet) |>
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Nut_diet, ., "Fe"))), # mg per kg
    ) |>
    # delete now unused (and very heavy!) columns
    dplyr::select(-c(Nut_W, Nut_diet))

}

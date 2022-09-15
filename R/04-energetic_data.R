##--------------------------------------------------------------------------------------------------------
## SCRIPT : Define diets of pinnipeds species
## 04-energetic_data.R
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# This script should contain FUNCTIONS ONLY

#'
#'
#'
#'
# function to add Mass, Beta and Fe excretion rate to dataset with diet, nut in diet and abundance data
add_nrjtic <- function(diet_nut_abund_tibb) {

  diet_nut_abund_tibb |>
    dplyr::mutate(Mass_mean = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 350,
                                               Species == "Lobodon carcinophaga" ~ 200,
                                               Species == "Ommatophoca rossii" ~ 160,
                                               Species == "Leptonychotes weddellii" ~ 300),
                  Mass_min = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 150,
                                              Species == "Lobodon carcinophaga" ~ 120,
                                              Species == "Ommatophoca rossii" ~ 100,
                                              Species == "Leptonychotes weddellii" ~ 120),
                  Mass_max = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 600,
                                              Species == "Lobodon carcinophaga" ~ 410,
                                              Species == "Ommatophoca rossii" ~ 250,
                                              Species == "Leptonychotes weddellii" ~ 600),
                  # mean chose from all references ranges
                  Beta_mean = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 3,
                                               Species == "Lobodon carcinophaga" ~ 2,
                                               Species == "Ommatophoca rossii" ~ 2,
                                               Species == "Leptonychotes weddellii" ~ 2),
                  Beta_min = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 2,
                                              Species == "Lobodon carcinophaga" ~ 1.5,
                                              Species == "Ommatophoca rossii" ~ 1.5,
                                              Species == "Leptonychotes weddellii" ~ 1.5),
                  Beta_max = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 3.5,
                                              Species == "Lobodon carcinophaga" ~ 3,
                                              Species == "Ommatophoca rossii" ~ 3,
                                              Species == "Leptonychotes weddellii" ~ 3),
                  Fe_exc = 0.80
    ) |>
    tidyr::nest(Mass = c(Mass_mean, Mass_min, Mass_max),
                Beta = c(Beta_mean, Beta_min, Beta_max))

}

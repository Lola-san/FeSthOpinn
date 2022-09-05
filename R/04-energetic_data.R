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
    dplyr::mutate(Mass = dplyr::case_when(Species == "Hydrurga leptonyx" ~ (338+348)/2,
                                          Species == "Lobodon carcinophaga" ~ 177,
                                          Species == "Ommatophoca rossii" ~ 158,
                                          Species == "Leptonychotes weddellii" ~ 252),
                  # data is from Castellini et al. 2009 for all but leopard seals and from Forcada et al 2009 for leopard seals
                  Beta = 3,
                  Fe_exc = 0.80
                  ) |>
    tidyr::nest(Mass = c(Mass),
                Beta = c(Beta))

}

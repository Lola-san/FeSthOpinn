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
    dplyr::mutate(Mass = dplyr::case_when(Species == "Hydrurga leptonyx" ~ (546+382)/2,
                                          Species == "Lobodon carcinophaga" ~ (216+196)/2,
                                          Species == "Ommatophoca rossii" ~ (163+128)/2,
                                          Species == "Leptonychotes weddellii" ~ (162+154)/2),
                  Mass_min = Mass - Mass*0.1,
                  Mass_max = Mass + Mass*0.1,
                  Beta = 3,
                  Beta_min = Beta - 0.5,
                  Beta_max = Beta + 0.5,
                  Fe_exc = 0.9
                  ) |>
    tidyr::nest(Mass = c(Mass,
                         Mass_min,
                         Mass_max),
                Beta = c(Beta, Beta_min, Beta_max))

}

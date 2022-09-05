##--------------------------------------------------------------------------------------------------------
## SCRIPT : Define abundance of pinnipeds species in the Southern Ocean
## 03-abundance_data.R
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# This script should contain FUNCTIONS ONLY

# main reference : Southwell et al 2012 (review)
# we use estimates from APIS surveys

# we use the min, mean and max estimated values
# they are just to be added to the dataset with diets
# there are three values for the three surveyed areas


#'
#'
#'
#'
#'
### add abundance columns to dataset with diets and nut in diets
add_abund <- function(tibb_diet_nut) {
  # tibb_diet_nut |>
  #   dplyr::mutate(Abund_min = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 3500 + 3700 + 3700,
  #                                              Species == "Lobodon carcinophaga" ~ 1219000 +
  #                                                1754000 + 726400,
  #                                              Species == "Ommatophoca rossii" ~ 11700 + 27700,
  #                                              Species == "Leptonychotes weddellii" ~ 144000 + 77000),
  #                 Abund_max = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 65000 + 23100 + 14500,
  #                                              Species == "Lobodon carcinophaga" ~ 2472000 +
  #                                                4748000 + 1396700,
  #                                              Species == "Ommatophoca rossii" ~ 43700 + 187500,
  #                                              Species == "Leptonychotes weddellii" ~ 759000 + 576000),
  #                 Abund_mean = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 15000 + 13200 + 7300,
  #                                               Species == "Lobodon carcinophaga" ~ 1736000 +
  #                                                 3187000 + 946400,
  #                                               Species == "Ommatophoca rossii" ~ 22600 + 55900,
  #                                               Species == "Leptonychotes weddellii" ~ 331000 + 302000)
  #   )

  tibb_diet_nut |>
    # parameters have been determined using the Parameter solver software and mean, min and max values provided in Southwell et al. 2012 for
    # Hydrlept, Lobocar and Ommaros and LaRue et al 2021 for Leptwed
    dplyr::mutate(Abund_mean = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 39380,
                                                Species == "Lobodon carcinophaga" ~ 5778820,
                                                Species == "Ommatophoca rossii" ~105670,
                                                Species == "Leptonychotes weddellii" ~ 235150),
                  Abund_sd = dplyr::case_when(Species == "Hydrurga leptonyx" ~ 7750,
                                                Species == "Lobodon carcinophaga" ~ 126115,
                                                Species == "Ommatophoca rossii" ~ 15890,
                                                Species == "Leptonychotes weddellii" ~ 36325)
    ) |>
    tidyr::nest(Abund = c(Abund_mean, Abund_sd))
}

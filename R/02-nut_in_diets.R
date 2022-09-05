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
compute_means_per_prey_group <- function(Compo_dataset){
  Compo_dataset |>
    dplyr::mutate(Taxa = dplyr::case_when(Taxa %in% c("Krill", "Other zooplankton") ~ "Krill & other zooplankton",
                                          TRUE ~ Taxa)) |>
    dplyr::filter(Taxa %in% c("Fish", "Cephalopod", "Krill & other zooplankton", "Pinniped (muscle)", "Penguins (muscle)")) |>
    dplyr::group_by(Taxa) |>
    dplyr::summarise(NRJ_kJ.kg = mean(NRJ_ww, na.rm = TRUE),
                     Fe_mg.kg = mean(`Fe_ww_mg_kg-1`, na.rm = TRUE))  |>
    # set NRJ content of pinnipeds and penguins to 10 (assumption)
    dplyr::mutate(`NRJ_kJ.kg`= dplyr::case_when(Taxa %in% c("Pinniped (muscle)",
                                                            "Penguins (muscle)") ~ 10,
                                                TRUE ~ `NRJ_kJ.kg`))
}



#'
#'
#'
#'
#'
##################### compute nutrient content of diet ########################
compute_mean_compo_diet <- function(diet_tibb, compo_tibb) {

  diet_tibb |>
    dplyr::mutate(Diet_compo = seq_along(Diet) |>
                    purrr::map(~ purrr::pluck(Diet, .) |>
                                 tidyr::pivot_longer(cols = c("Fish", "Cephalopod", "Krill & other zooplankton",
                                                              "Pinniped (muscle)", "Penguins (muscle)"),
                                                     names_to = "Taxa",
                                                     values_to = "%W") |>
                                 dplyr::left_join(compo_tibb) |>
                                 dplyr::mutate(WNRJ = `%W` * `NRJ_kJ.kg`,
                                               WFe = `%W` * `Fe_mg.kg`) |>
                                 dplyr::summarise(mean_NRJ_diet = sum(WNRJ),
                                                  mean_Fe_diet = sum(WFe))
                               )
                  ) |>
    tidyr::unnest(Diet_compo)

}

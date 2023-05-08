##--------------------------------------------------------------------------------------------------------
## SCRIPT : test significance of differences between levels of iron release
## at the individual and populational scale
## 08-test_differences.R
##
## Authors : Lola Gilbert
## Last update : 2023-05 (after review 1)
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# This script should contain FUNCTIONS ONLY

#'
#'
#'
#'
# test the significance of differences between levels of iron release
# at the daily individual release level
test_diff_ind <- function(output_tib) {

  output_tib |>
    dplyr::group_by(Species) |>
    tidyr::unnest(excrete_Fe_ind) |>
    dplyr::select(Species, value) |>
    tidyr::pivot_wider(names_from = Species,
                       values_from = value,
                       values_fn = list) |>
    tidyr::unnest(cols = c(`Hydrurga leptonyx`, `Lobodon carcinophaga`,
                           `Ommatophoca rossii`, `Leptonychotes weddellii`)) |>
    dplyr::mutate(t_leop_crab = dplyr::case_when(`Hydrurga leptonyx` > `Lobodon carcinophaga` ~ 1,
                                                   TRUE ~ 0),
                  t_leop_wedd = dplyr::case_when(`Hydrurga leptonyx` > `Leptonychotes weddellii` ~ 1,
                                                     TRUE ~ 0),
                  t_leop_ross = dplyr::case_when(`Hydrurga leptonyx` > `Ommatophoca rossii` ~ 1,
                                                 TRUE ~ 0),
                  t_crab_wedd = dplyr::case_when(`Lobodon carcinophaga` > `Leptonychotes weddellii` ~ 1,
                                                 TRUE ~ 0),
                  t_crab_ross = dplyr::case_when(`Lobodon carcinophaga` > `Ommatophoca rossii` ~ 1,
                                                   TRUE ~ 0),
                  t_wedd_ross = dplyr::case_when(`Leptonychotes weddellii` > `Ommatophoca rossii` ~ 1,
                                                 TRUE ~ 0),
    ) |>
    dplyr::summarise(t_leop_crab = mean(t_leop_crab),
                     t_leop_wedd = mean(t_leop_wedd),
                     t_leop_ross = mean(t_leop_ross),
                     t_crab_wedd = mean(t_crab_wedd),
                     t_crab_ross = mean(t_crab_ross),
                     t_wedd_ross = mean(t_wedd_ross)) |>
    tidyr::pivot_longer(cols = c("t_leop_crab":"t_wedd_ross"),
                        names_to = "Test",
                        values_to = "p_sp1_superior_to_sp2") |>
    dplyr::mutate(Sp1 = dplyr::case_when(stringr::str_starts(Test, "t_leop") ~ "Hydrurga leptonyx",
                                           stringr::str_starts(Test, "t_crab") ~ "Lobodon carcinophaga",
                                         stringr::str_starts(Test, "t_wedd") ~ "Leptonychotes weddellii"),
                  Sp2 = dplyr::case_when(stringr::str_ends(Test, "_crab") ~ "Lobodon carcinophaga",
                                           stringr::str_ends(Test, "_wedd") ~ "Leptonychotes weddellii",
                                         stringr::str_ends(Test, "_ross") ~ "Ommatophoca rossii")) |>
    dplyr::select(-Test)
}




#'
#'
#'
#'
# test the significance of differences between levels of iron release
# at the populational level
test_diff_pop <- function(output_tib) {

  output_tib |>
    dplyr::group_by(Species) |>
    tidyr::unnest(excrete_Fe) |>
    dplyr::select(Species, value) |>
    tidyr::pivot_wider(names_from = Species,
                       values_from = value,
                       values_fn = list) |>
    tidyr::unnest(cols = c(`Hydrurga leptonyx`, `Lobodon carcinophaga`,
                           `Ommatophoca rossii`, `Leptonychotes weddellii`)) |>
    dplyr::mutate(t_leop_crab = dplyr::case_when(`Hydrurga leptonyx` > `Lobodon carcinophaga` ~ 1,
                                                 TRUE ~ 0),
                  t_leop_wedd = dplyr::case_when(`Hydrurga leptonyx` > `Leptonychotes weddellii` ~ 1,
                                                 TRUE ~ 0),
                  t_leop_ross = dplyr::case_when(`Hydrurga leptonyx` > `Ommatophoca rossii` ~ 1,
                                                 TRUE ~ 0),
                  t_crab_wedd = dplyr::case_when(`Lobodon carcinophaga` > `Leptonychotes weddellii` ~ 1,
                                                 TRUE ~ 0),
                  t_crab_ross = dplyr::case_when(`Lobodon carcinophaga` > `Ommatophoca rossii` ~ 1,
                                                 TRUE ~ 0),
                  t_wedd_ross = dplyr::case_when(`Leptonychotes weddellii` > `Ommatophoca rossii` ~ 1,
                                                 TRUE ~ 0),
    ) |>
    dplyr::summarise(t_leop_crab = mean(t_leop_crab),
                     t_leop_wedd = mean(t_leop_wedd),
                     t_leop_ross = mean(t_leop_ross),
                     t_crab_wedd = mean(t_crab_wedd),
                     t_crab_ross = mean(t_crab_ross),
                     t_wedd_ross = mean(t_wedd_ross)) |>
    tidyr::pivot_longer(cols = c("t_leop_crab":"t_wedd_ross"),
                        names_to = "Test",
                        values_to = "p_sp1_superior_to_sp2") |>
    dplyr::mutate(Sp1 = dplyr::case_when(stringr::str_starts(Test, "t_leop") ~ "Hydrurga leptonyx",
                                         stringr::str_starts(Test, "t_crab") ~ "Lobodon carcinophaga",
                                         stringr::str_starts(Test, "t_wedd") ~ "Leptonychotes weddellii"),
                  Sp2 = dplyr::case_when(stringr::str_ends(Test, "_crab") ~ "Lobodon carcinophaga",
                                         stringr::str_ends(Test, "_wedd") ~ "Leptonychotes weddellii",
                                         stringr::str_ends(Test, "_ross") ~ "Ommatophoca rossii")) |>
    dplyr::select(-Test)
}

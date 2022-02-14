##--------------------------------------------------------------------------------------------------------
## SCRIPT : Define diets of pinnipeds species
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------


#'
#'
#'
#'
#'
#'
#'
create_diet_tib <- function() {

  tibble::tribble(~ Species, ~ fish, ~ cephalopods, ~ krill, ~ mammal, ~ bird,
                  "Hydrurga leptonyx", 0.25, 0, 0.25, 0.25, 0.25,
                  "Lobodon carcinophaga", 0.05, 0.05, 0.9, 0, 0,
                  "Ommatophoca rossii", 0.25, 0.65, 0.1, 0, 0,
                  "Leptonychotes weddellii", 0.85, 0.18, 0.02, 0, 0) |>
    tidyr::nest(Diet = c(fish:bird))

}

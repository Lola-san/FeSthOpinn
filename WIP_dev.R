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

diet_tibb <- tibble::tribble(~ Species, ~ fish, ~ cephalopods, ~ krill, ~ mammal, ~ bird,
                "Hydrurga leptonyx", 0.25, 0, 0.25, 0.25, 0.25,
                "Lobodon carcinophaga", 0.05, 0.05, 0.9, 0, 0,
                "Ommatophoca rossii", 0.25, 0.65, 0.1, 0, 0,
                "Leptonychotes weddellii", 0.85, 0.18, 0.02, 0, 0)

diet_tibb |>
  tidyr::nest(Diet = c(fish:bird))

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

diet_tibb <- diet_tibb |>
  tidyr::nest(Diet = c(fish:bird))


# load nutrient in preys tibbles
fish_ceph <- readxl::read_excel("data/Nuts_in_preys.xlsx")
krill_bird_mam <- readxl::read_excel("data/Fe_in_preys.xlsx")

# clean up first file : we want only fish and Ceph from it and we want only Fe (and NRJ...)
head(fish_ceph)
unique(fish_ceph$Taxa)

fish_ceph <- fish_ceph |>
  dplyr::filter(Taxa %in% c("Fish", "Cephalopod")) |>
  dplyr::rename(Species = Sp_prey) |>
  dplyr::select(Taxa, Species, NRJ, Fe)

# clean up second file :
head(krill_bird_mam)

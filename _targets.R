################################################################################
# FeSthOpinn
# Lola Gilbert lola.gilbert@univ-lr.fr
#
# February 2022
# _targets.R
#
# Script decomposing with all steps of the analysis with target
################################################################################

library("targets")

# Source all functions contained in all files in the R directory
lapply(list.files(here::here("R"),
                  recursive = TRUE, full.names = T),
       source)

list(
  # create diet tibble
  tar_target(data_diets, create_diet_tib()),
  # define prey composition dataset from Bay of Biscay
  tar_target(data_compo_BoB_file,
             "data/Nuts_in_preys.xlsx",
             format = "file"),
  # define prey composition dataset for krill, seals and penguins
  tar_target(data_compo_Ant_file,
             "data/Fe_in_preys.xlsx",
             format = "file"),
  # load them
  tar_target(data_compo_BoB, load_xl(data_compo_BoB_file)),
  tar_target(data_compo_Ant, load_xl(data_compo_Ant_file))
)

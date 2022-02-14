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
  # load it
  tar_target(data_compo_BoB, load_xl(data_compo_BoB_file))
)

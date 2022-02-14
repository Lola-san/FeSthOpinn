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
  tar_target(data_diets, create_diet_tib())
)

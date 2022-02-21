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
  tar_target(diet_input, create_diet_tib()),
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
  tar_target(data_compo_Ant, load_xl(data_compo_Ant_file)),
  # clean and bind them
  tar_target(data_compo_full, clean_bind_compo_tibbles(data_compo_BoB,
                                                       data_compo_Ant)),
  # bootstrapp composition of the taxa composing the diets
  tar_target(prey_compo_boot, bootstrap_compo(data_compo_full,
                                              nsim = 1e3) ########## NSIM HERE
             ),
  tar_target(diet_nut_input, compute_nut_in_diet(diet_input,
                                                 prey_compo_boot)),
  #### add abundance data
  tar_target(diet_nut_abund_input, add_abund(diet_nut_input)),
  #### add nrjtic data
  tar_target(full_input, add_nrjtic(diet_nut_abund_input)),
  # RUN MODEL
  tar_target(model_output, run_model(full_input,
                                     nsim = 1e3) ########## NSIM HERE
             )
)

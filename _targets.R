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
  # define prey composition dataset
  tar_target(data_compo,
             "data/prey_compo_compiled.xlsx",
             format = "file"),
  # load it
  tar_target(compo_tib, load_xl(data_compo)),
  # compute mean NRJ and Fe content of prey_gps
  tar_target(compo_input, compute_means_per_prey_group(compo_tib)),
  # compute mean NRJ and Fe content of diet
  tar_target(diet_nut_input, compute_mean_compo_diet(diet_input, compo_input)),
  #### add abundance data
  tar_target(diet_nut_abund_input, add_abund(diet_nut_input)),
  #### add nrjtic data
  tar_target(full_input, add_nrjtic(diet_nut_abund_input)),
  ################## RUN MODEL ####################
  tar_target(model_output, run_model(full_input,
                                     nsim = 1e5) ########## NSIM HERE
             ),
  #################### RUN sensitivity analysis ################################
  ############## refers to functions of 07_sensitivity_ana.R ###################
  tar_target(sobol_index_all_sensi, create_sobol_index_tib_sensi(model_output,
                                                                 nsim = 1e5)), ########## NSIM HERE
  tar_target(fig_sensi_output, fig_sensitivy_indices(sobol_index_all_sensi,
                                                     "output",
                                                     "fig_sensi")),
  tar_target(fig_sensi_file, fig_sensitivy_indices(sobol_index_all_sensi,
                                                     "file",
                                                     "fig_sensi")),
  # generate outputs
  tar_target(fig_tot_Fe_output, fig_tot_Fe_released(model_output,
                                                "output",
                                                "fig_tot_Fe")),
  tar_target(fig_tot_Fe_file, fig_tot_Fe_released(model_output,
                                              "file",
                                              "fig_tot_Fe")),
  tar_target(fig_tot_Fe_comp_output, fig_tot_Fe_released_comp(model_output,
                                                    "output",
                                                    "fig_tot_Fe_comp")),
  tar_target(fig_tot_Fe_comp_file, fig_tot_Fe_released_comp(model_output,
                                                  "file",
                                                  "fig_tot_Fe_comp")),
  tar_target(fig_sp_Fe_output, fig_sp_Fe_released(model_output,
                                                    "output",
                                                    "fig_sp_Fe")),
  tar_target(fig_sp_Fe_file, fig_sp_Fe_released(model_output,
                                                  "file",
                                                  "fig_sp_Fe")),
  tar_target(supp_mat1_output, supp_table_param(model_output,
                                         "output",
                                         "supp_mat1")),
  tar_target(supp_mat1_file, supp_table_param(model_output,
                                                "file",
                                                "supp_mat1"))
)

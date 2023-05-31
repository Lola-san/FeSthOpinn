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
  tar_target(diet_input_aft_review, create_diet_tib_after_review()),
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
  tar_target(diet_nut_input_aft_review, compute_mean_compo_diet(diet_input_aft_review,
                                                                compo_input)),
  #### add abundance data
  tar_target(diet_nut_abund_input, add_abund(diet_nut_input)),
  tar_target(diet_nut_abund_input_aft_review, add_abund(diet_nut_input_aft_review)),
  #### add nrjtic data
  tar_target(full_input, add_nrjtic(diet_nut_abund_input)),
  tar_target(full_input_aft_review, add_nrjtic(diet_nut_abund_input_aft_review)),
  ################## RUN MODEL ####################
  tar_target(model_output, run_model(full_input,
                                     nsim = 1e5) ########## NSIM HERE
  ),tar_target(model_output_aft_review, run_model(full_input_aft_review,
                                       nsim = 1e5) ########## NSIM HERE
  ),
  #################### RUN sensitivity analysis ################################
  ############## refers to functions of 07_sensitivity_ana.R ###################
  tar_target(sobol_index_all_sensi, create_sobol_index_tib_sensi(model_output,
                                                                 nsim = 1e5)), ########## NSIM HERE
  tar_target(sobol_index_all_sensi_aft_review, create_sobol_index_tib_sensi(model_output_aft_review,
                                                                 nsim = 1e5)),
  tar_target(fig_sensi_output, fig_sensitivy_indices(sobol_index_all_sensi,
                                                     "output",
                                                     "fig_sensi")),
  tar_target(fig_sensi_file, fig_sensitivy_indices(sobol_index_all_sensi,
                                                   "file",
                                                   "fig_sensi")),
  tar_target(fig_sensi_aft_review, fig_sensitivy_indices(sobol_index_all_sensi_aft_review,
                                                                "file",
                                                                "fig_sensi_aft_review")),
  # generate outputs
  tar_target(table_output_per_sp_output, table_Fe_release_sp(model_output,
                                                             "output",
                                                             "Table_sp")),
  tar_target(table_output_per_sp_file, table_Fe_release_sp(model_output,
                                                           "file",
                                                           "Table_sp")),
  tar_target(table_output_per_sp_file_aft_review, table_Fe_release_sp(model_output_aft_review,
                                                           "file",
                                                           "Table_sp_aft_review")),
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
  tar_target(fig_tot_Fe_comp_file_aft_review, fig_tot_Fe_released_comp(model_output_aft_review,
                                                            "file",
                                                            "fig_tot_Fe_comp_aft_review")),
  tar_target(fig_sp_Fe_output, fig_sp_Fe_released(model_output,
                                                  "output",
                                                  "fig_sp_Fe")),
  tar_target(fig_sp_Fe_file, fig_sp_Fe_released(model_output,
                                                "file",
                                                "fig_sp_Fe")),
  tar_target(fig_sp_Fe_file_aft_review, fig_sp_Fe_released(model_output_aft_review,
                                                "file",
                                                "fig_sp_Fe_aft_review")),
  tar_target(supp_mat1_output, supp_table_param(model_output,
                                                "output",
                                                "supp_mat1")),
  tar_target(supp_mat1_file, supp_table_param(model_output,
                                              "file",
                                              "supp_mat1")),
  tar_target(supp_mat1_file_aft_review, supp_table_param(model_output_aft_review,
                                              "file",
                                              "supp_mat1_aft_review")),
  ## test significance of difference
  # daily individual Fe release
  tar_target(tib_test_diff_ind_output, test_diff_ind(model_output)),
  tar_target(tib_test_diff_ind_output_aft_review,
             test_diff_ind(model_output_aft_review)),
  # population Fe release
  tar_target(tib_test_diff_pop_output, test_diff_pop(model_output)),
  tar_target(tib_test_diff_pop_output_aft_review,
             test_diff_pop(model_output_aft_review)),
  # plot figure with populational levels with this info
  tar_target(fig_sp_Fe_with_diff_file, fig_sp_Fe_with_diff(model_output,
                                                           "fig_sp_Fe_with_diff")),
  # supplementary table 2 with results for other species
  tar_target(supp_table2_comp, supp_table_comp(model_output,
                                               "supp_mat2")),
  tar_target(supp_table2_comp_aft_review, supp_table_comp(model_output_aft_review,
                                               "supp_mat2_aft_review")),
  # plot figure with comparison with other taxa but with our results
  # emphasized
  tar_target(fig_tot_Fe_comp_alpha, fig_tot_Fe_released_comp_alpha(model_output,
                                                                   alpha = 0.3,
                                                                   "fig_tot_Fe_comp_alpha")),
  tar_target(fig_tot_Fe_comp_alpha_aft_review, fig_tot_Fe_released_comp_alpha(model_output_aft_review,
                                                                   alpha = 0.3,
                                                                   "fig_tot_Fe_comp_alpha_aft_review"))

)

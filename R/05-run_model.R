##--------------------------------------------------------------------------------------------------------
## SCRIPT : Define diets of pinnipeds species
## 05-run_model.R
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------
# script with function to run the model

# This script should contain FUNCTIONS ONLY

####################### PRELIMINARY FUNCTIONS ##################################

# to compute the daily need of an individual (Kleber equation) of a given species
kleber <- function(beta, mass, n_sim,
                   assimil_mean = NULL,  assimil_se = 0.05,
                   assimil_min, assimil_max,
                   dietQuality
) {
  # should the daily ration be computed?
  if(!is.null(assimil_mean) && !is.null(dietQuality)) {
    a <- truncnorm::rtruncnorm(n = n_sim,
                               mean = assimil_mean,
                               sd = assimil_se,
                               a = assimil_min,
                               b = assimil_max) # assimilation

    return(tibble::tibble(ADMR = beta * (293.1*mass^(3/4)),
                          A_rate = a,
                          Ration = beta * (293.1*mass^(3/4))/(a*dietQuality),
                          PercentBM = 293.1*beta/(a*dietQuality*mass^(1/4)))
    )
  }
  else { return(list(ADMR = beta * (293.1*mass^(3/4)))) }
}

# to simulate abundance uncertainty
abundance <- function(abund_bar,
                      abund_min,
                      abund_max,
                      n_sim){
  return(tibble::as_tibble_col(truncnorm::rtruncnorm(n = n_sim,
                                                     a = abund_min,
                                                     b = abund_max,
                                                     mean = abund_bar,
                                                     sd = (abund_max - abund_min)/4)))
}

############################# COMPUTATION ######################################

run_model <- function(input_tibb, nsim) {
  input_tibb |>
    dplyr::mutate(
      ######### ADAPT FORMAT OF CONSTANT TO VECTORS FOR COMPUTATIONAL REASONS
      Diet = seq_along(Diet) |>
        purrr::map(~ tidyr::uncount(       # change % to proportion for diet data
          purrr::pluck(Diet, .), nsim)), # the unique line to get the same dimensions as vector to be multiplied by
      ###### SIMULATE UNCERTAINTY IN ABUNDANCE
      Abund = seq_along(Abund) |>
        purrr::map(~ abundance(abund_bar = purrr::pluck(Abund, ., "Abund_mean"),
                               abund_min = purrr::pluck(Abund, ., "Abund_min"),
                               abund_max = purrr::pluck(Abund, ., "Abund_max"),
                               n_sim = nsim)),
      ###### SIMULATE UNCERTAINTY IN MASS, BETA, Fe EXCRETION RATE
      Mass = seq_along(Mass) |>
        purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim,
                                                                 mean = purrr::pluck(Mass, .,"Mass_mean"),
                                                                 sd = 0.2 * purrr::pluck(Mass, .,"Mass_mean"),
                                                                 a = purrr::pluck(Mass, .,"Mass_min"),
                                                                 b = purrr::pluck(Mass, .,"Mass_max")))),
      Beta = seq_along(Beta) |>
        purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim,
                                                                 mean = purrr::pluck(Beta, .,"Beta_mean"),
                                                                 sd = 0.5,
                                                                 a = purrr::pluck(Beta, .,"Beta_min"),
                                                                 b = purrr::pluck(Beta, .,"Beta_max")))),
      Fe_exc = seq_along(Fe_exc) |> # nutrient excretion rate
        purrr::map(~ tibble::as_tibble_col(runif(n = nsim,
                                                 min = Fe_exc - 0.1,
                                                 max = Fe_exc + 0.1))),
      ###### SIMULATE UNCERTAINTY IN MEAN COMPOSITION OF THE DIET
      NRJ_diet = seq_along(Diet) |>
        purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim,
                                                                 mean = purrr::pluck(mean_NRJ_diet, .) * 1e3, # from kJ per g to kJ per kg
                                                                 sd = 0.2 * purrr::pluck(mean_NRJ_diet, .) * 1e3, # from kJ per g to kJ per kg
                                                                 a = 0.2 * purrr::pluck(mean_NRJ_diet, .) * 1e3 # from kJ per g to kJ per kg
        ))),
      Fe_diet = seq_along(Diet) |>
        purrr::map(~ tibble::as_tibble_col(truncnorm::rtruncnorm(n = nsim,
                                                                 mean = purrr::pluck(mean_Fe_diet, .),
                                                                 sd = 0.2 * purrr::pluck(mean_Fe_diet, .),
                                                                 a = 0.2 * purrr::pluck(mean_Fe_diet, .)))),
      ############################ COMPUTE INDIVIDUAL NRJTIC DATA, NEEDS AND CONSUMPTION OF POP ######
      Indi_data = seq_along(Mass) |>
        purrr::map(~ kleber(beta = purrr::pluck(Beta, ., 1),
                            mass = purrr::pluck(Mass, ., 1),
                            n_sim = nsim,
                            assimil_mean = 0.9,
                            assimil_se = 0.05,
                            assimil_min = 0.8,
                            assimil_max = 0.95,
                            dietQuality = purrr::pluck(NRJ_diet, ., 1))),
      # Population consumption and needs
      conso_pop = seq_along(Abund) |> # Annual amount of prey consumed by the population in kg
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund, ., 1)*365*purrr::pluck(Indi_data, ., "Ration"))),
      Needs_pop = seq_along(Abund) |> # Annual need of the population in kJ
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund, ., 1)*365*purrr::pluck(Indi_data, ., "ADMR")))
    ) |>
    ############ COMPUTE MODEL OUTPUTS : Fe EXCRETION  #########################
  dplyr::mutate(conso_diet_ind = seq_along(Indi_data) |> # Species daily consumption of taxa (kg)
                  purrr::map(~ (purrr::pluck(Indi_data, .)$Ration) * purrr::pluck(Diet, .)),
                conso_diet = seq_along(conso_pop) |> # Population annual consumption of taxa (kg)
                  purrr::map(~ purrr::pluck(conso_pop, ., 1) * purrr::pluck(Diet, .)),
                ## Fe CONSUMPTION AND EXCRETION !
                conso_Fe_ind = seq_along(Indi_data) |> # Individual daily consumption of nutrient in mg
                  purrr::map(~ purrr::pluck(Indi_data, ., "Ration") * purrr::pluck(Fe_diet, .)),
                excrete_Fe_ind = seq_along(Fe_exc) |> # Individual daily excretion of nutrient (mg/day)
                  purrr::map(~ purrr::pluck(conso_Fe_ind, .) * purrr::pluck(Fe_exc, .)),
                conso_Fe = seq_along(conso_pop) |> # Annual consumption of nutrient
                  purrr::map(~ (purrr::pluck(conso_pop, ., 1) * purrr::pluck(Fe_diet, .))/1e9), # from mg to tonnes
                excrete_Fe = seq_along(conso_Fe) |> # Annual excretion of nutrient
                  purrr::map(~ purrr::pluck(conso_Fe, .) * purrr::pluck(Fe_exc, .))
  )
}


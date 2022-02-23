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
# to compute standard error from a min and a max
approx_se <- function(lower, upper) { return((upper-lower)/4) }

# to compute the daily need of an individual (Kleber equation) of a given species
kleber <- function(beta, mass, n_sim,
                   assimil_mean = NULL,  assimil_se = 0.05,
                   dietQuality
) {
  # should the daily ration be computed?
  if(!is.null(assimil_mean) && !is.null(dietQuality)) {
    a <- rnorm(n_sim, assimil_mean, assimil_se) # assimilation

    return(tibble::tibble(ADMR = beta * (293.1*mass^(3/4)),
                          A_rate = a,
                          Ration = beta * (293.1*mass^(3/4))/(a*dietQuality),
                          PercentBM = 293.1*beta/(a*dietQuality*mass^(1/4)))
    )
  }
  else { return(list(ADMR = beta * (293.1*mass^(3/4)))) }
}



############################# COMPUTATION ######################################

run_model <- function(input_tibb, nsim) {
  input_tibb |>
    dplyr::mutate(
      ######### ADAPT FORMAT OF CONSTANT TO VECTORS FOR COMPUTATIONAL REASONS
      Diet = seq_along(Diet) |>
        purrr::map(~ tidyr::uncount(       # change % to proportion for diet data
          purrr::pluck(Diet, .)/100, nsim)), # the unique line to get the same dimensions as vector to be multiplied by
      Abund_min = seq_along(Abund_min) |>
        purrr::map(~ tibble::as_tibble_col(runif(n = nsim,
                                                 min = purrr::pluck(Abund_min, .),
                                                 max = purrr::pluck(Abund_min, .)))), # the unique line to get the same dimensions as vector to be multiplied by
      Abund_max = seq_along(Abund_max) |>
        purrr::map(~ tibble::as_tibble_col(runif(n = nsim,
                                                 min = purrr::pluck(Abund_max, .),
                                                 max = purrr::pluck(Abund_max, .)))), # the unique line to get the same dimensions as vector to be multiplied by
      Abund_mean = seq_along(Abund_mean) |>
        purrr::map(~ tibble::as_tibble_col(runif(n = nsim,
                                                 min = purrr::pluck(Abund_mean, .),
                                                 max = purrr::pluck(Abund_mean, .)))), # the unique line to get the same dimensions as vector to be multiplied by
      ###### SIMULATE UNCERTAINTY IN MASS, BETA, EXCRETION
      Mass = seq_along(Mass) |>
        purrr::map(~ tibble::as_tibble_col(rnorm(nsim, Mass[[.]]$Mass,
                                                 approx_se(Mass[[.]]$Mass_min,
                                                           Mass[[.]]$Mass_max)))),
      Beta = seq_along(Beta) |>
        purrr::map(~ tibble::as_tibble_col(rnorm(nsim, Beta[[.]]$Beta,
                                                 approx_se(Beta[[.]]$Beta_min,
                                                           Beta[[.]]$Beta_max)))),
      Fe_exc = seq_along(Fe_exc) |> # nutrient excretion rate
        purrr::map(~ tibble::as_tibble_col(runif(n = nsim,
                                                 min = Fe_exc - 0.05,
                                                 max = Fe_exc + 0.05))),

      ############################ COMPUTE INDIVIDUAL NRJTIC DATA, NEEDS AND CONSUMPTION OF POP ######
      Indi_data = seq_along(Mass) |>
        purrr::map(~ kleber(beta = purrr::pluck(Beta, ., 1),
                            mass = purrr::pluck(Mass, ., 1),
                            n_sim = nsim,
                            assimil_mean = 0.8, assimil_se = 0.05,
                            dietQuality = purrr::pluck(NRJ_diet, ., 1))),
      # Population consumption and needs with min abundance estimates
      conso_pop_min = seq_along(Abund_min) |> # Annual amount of prey consumed by the population in kg
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund_min, ., 1)*365*purrr::pluck(Indi_data, ., "Ration"))),
      Needs_pop_min = seq_along(Abund_min) |> # Annual need of the population in kJ
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund_min, ., 1)*purrr::pluck(Indi_data, ., "ADMR"))),
      # Population consumption and needs with max abundance estimates
      conso_pop_max = seq_along(Abund_max) |> # Annual amount of prey consumed by the population in kg
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund_max, ., 1)*365*purrr::pluck(Indi_data, ., "Ration"))),
      Needs_pop_max = seq_along(Abund_max) |> # Annual need of the population in kJ
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund_max, ., 1)*365*purrr::pluck(Indi_data, ., "ADMR"))),
      # Population consumption and needs with mean abundance estimates
      conso_pop_mean = seq_along(Abund_mean) |> # Annual amount of prey consumed by the population in kg
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund_mean, ., 1)*365*purrr::pluck(Indi_data, ., "Ration"))),
      Needs_pop_mean = seq_along(Abund_mean) |> # Annual need of the population in kJ
        purrr::map(~ tibble::as_tibble_col(purrr::pluck(Abund_mean, ., 1)*365*purrr::pluck(Indi_data, ., "ADMR")))
    ) |>
    ############ COMPUTE MODEL OUTPUTS : Fe EXCRETION  #########################
  dplyr::mutate(conso_diet_ind = seq_along(Indi_data) |> # Species daily consumption of taxa (kg)
                  purrr::map(~ (purrr::pluck(Indi_data, .)$Ration) * purrr::pluck(Diet, .)),
                conso_diet_min = seq_along(conso_pop_min) |> # Population annual consumption of taxa (kg)
                  purrr::map(~ purrr::pluck(conso_pop_min, ., 1) * purrr::pluck(Diet, .)),
                conso_diet_max = seq_along(conso_pop_max) |> # Population annual consumption of taxa (kg)
                  purrr::map(~ purrr::pluck(conso_pop_max, ., 1) * purrr::pluck(Diet, .)),
                conso_diet_mean = seq_along(conso_pop_mean) |> # Population annual consumption of taxa (kg)
                  purrr::map(~ purrr::pluck(conso_pop_mean, ., 1) * purrr::pluck(Diet, .)),
                ## Fe CONSUMPTION AND EXCRETION !
                conso_Fe_ind = seq_along(Indi_data) |> # Individual daily consumption of nutrient in mg
                  purrr::map(~ purrr::pluck(Indi_data, ., "Ration") * purrr::pluck(Fe_diet, .)),
                excrete_Fe_ind = seq_along(Fe_exc) |> # Individual daily excretion of nutrient (mg/day)
                  purrr::map(~ purrr::pluck(conso_Fe_ind, .) * purrr::pluck(Fe_exc, .)),
                conso_Fe_min = seq_along(conso_pop_min) |> # Annual consumption of nutrient
                  purrr::map(~ (purrr::pluck(conso_pop_min, ., 1) * purrr::pluck(Fe_diet, .))/1e9), # from mg to tonnes
                excrete_Fe_min = seq_along(conso_Fe_min) |> # Annual excretion of nutrient
                  purrr::map(~ purrr::pluck(conso_Fe_min, .) * purrr::pluck(Fe_exc, .)),
                conso_Fe_max = seq_along(conso_pop_max) |> # Annual consumption of nutrient
                  purrr::map(~ (purrr::pluck(conso_pop_max, ., 1) * purrr::pluck(Fe_diet, .))/1e9), # from mg to tonnes
                excrete_Fe_max = seq_along(conso_Fe_max) |> # Annual excretion of nutrient
                  purrr::map(~ purrr::pluck(conso_Fe_max, .) * purrr::pluck(Fe_exc, .)),
                conso_Fe_mean = seq_along(conso_pop_mean) |> # Annual consumption of nutrient
                  purrr::map(~ (purrr::pluck(conso_pop_mean, ., 1) * purrr::pluck(Fe_diet, .))/1e9), # from mg to tonnes
                excrete_Fe_mean = seq_along(conso_Fe_mean) |> # Annual excretion of nutrient
                  purrr::map(~ purrr::pluck(conso_Fe_mean, .) * purrr::pluck(Fe_exc, .))
  )
}


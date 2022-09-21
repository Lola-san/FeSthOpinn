##--------------------------------------------------------------------------------------------------------
## SCRIPT : Run sensitivity analysis
## 07-sensitivity_ana.R
##
## Authors : Lola Gilbert
## Last update : 2022-09
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# We use Sobol global method

############################ WITH SENSITIVITY PACKAGE

### small functions for the sensitivity analysis
# create function to compute y from mat
compute_y_sensi <- function(param_mat) {
  # param_mat is the matrix of parameters
  ADMR <- param_mat[, 3]*293.1*(param_mat[, 2]^0.737)
  Ration <- ADMR / (param_mat[, 6]*param_mat[, 4])
  conso_pop <- param_mat[, 1]*365*Ration
  conso_Fe <- (conso_pop*param_mat[, 5])/1e9
  excrete_Fe <- conso_Fe*param_mat[, 7]

  return(excrete_Fe)
}

#'
#'
#'
#'
# function to:
# compute the sensitivity analysis index for all lines of the result table
# each line being a species in an Eco_area in a Geo_area
create_sobol_index_tib_sensi <- function(output_tib,
                                         nsim) {

  # tibble where results we be stored
  df_Si_Sti <- tibble::tibble(Species = NA,
                              Input = NA,
                              Sensitivity = NA, # wether it's first order or total sobol indices
                              original = NA, #mean
                              bias = NA,
                              "std. error" = NA,
                              "min. c.i." = NA,
                              "max. c.i." = NA)


  for (rw in 1:nrow(output_tib)) {

    # sampling matrix
    # change distributions of inputs according to data or bibliography/assumptions
    parammatX1 <- matrix(data = c(sample(purrr::pluck(output_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Fe_diet", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(truncnorm::rtruncnorm(n = nsim,
                                                               mean = 0.9,
                                                               sd = 0.05,
                                                               a = 0.85,
                                                               b = 0.95), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Fe_exc", rw, 1), size = nsim/5, replace = FALSE)),
                         ncol = 7, nrow = nsim/5)

    parammatX2 <- matrix(data = c(sample(purrr::pluck(output_tib, "Abund", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Mass", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Beta", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "NRJ_diet", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Fe_diet", rw, 1), size = nsim/5, replace = FALSE),
                                  sample(truncnorm::rtruncnorm(n = nsim,
                                                               mean = 0.9,
                                                               sd = 0.05,
                                                               a = 0.85,
                                                               b = 0.95), size = nsim/5, replace = FALSE),
                                  sample(purrr::pluck(output_tib, "Fe_exc", rw, 1), size = nsim/5, replace = FALSE)),
                         ncol = 7, nrow = nsim/5)

    #output <- sample(purrr::pluck(results_tib, "excrete_nut", rw, "N"), size = nsim/5, replace = FALSE)

    sens <- sensitivity::sobolSalt(model = compute_y_sensi, X1 = parammatX1, X2 = parammatX2,
                                   scheme = "A", nboot = 1e3, conf = 0.95)


    df_Si_first <- tibble::tibble(Species = purrr::pluck(output_tib, "Species", rw),
                                  Input = rownames(sens$S),
                                  Sensitivity = "First order indices",
                                  original = purrr::pluck(sens$S, "original"),
                                  bias = purrr::pluck(sens$S, "bias"),
                                  "std. error" = purrr::pluck(sens$S, "std. error"),
                                  "min. c.i." = purrr::pluck(sens$S, "min. c.i."),
                                  "max. c.i." = purrr::pluck(sens$S, "max. c.i.")) |>
      dplyr::mutate(Input = dplyr::case_when(Input == "X1" ~ "Abundance",
                                             Input == "X2" ~ "Body mass",
                                             Input == "X3" ~ "Beta",
                                             Input == "X4" ~ "Mean NRJ content of diet",
                                             Input == "X5" ~ "Mean Fe content of diet",
                                             Input == "X6" ~ "Assimilation rate",
                                             Input == "X7" ~ "Fe release rate"
      ))

    df_Si_tot <- tibble::tibble(Species = purrr::pluck(output_tib, "Species", rw),
                                Input = rownames(sens$T),
                                Sensitivity = "Total order indices",
                                original = purrr::pluck(sens$T, "original"),
                                bias = purrr::pluck(sens$S, "bias"),
                                "std. error" = purrr::pluck(sens$T, "std. error"),
                                "min. c.i." = purrr::pluck(sens$T, "min. c.i."),
                                "max. c.i." = purrr::pluck(sens$T, "max. c.i.")) |>
      dplyr::mutate(Input = dplyr::case_when(Input == "X1" ~ "Abundance",
                                             Input == "X2" ~ "Body mass",
                                             Input == "X3" ~ "Beta",
                                             Input == "X4" ~ "Mean NRJ content of diet",
                                             Input == "X5" ~ "Mean Fe content of diet",
                                             Input == "X6" ~ "Assimilation rate",
                                             Input == "X7" ~ "Fe release rate"
      ))


    df_Si_Sti <- rbind(df_Si_Sti,
                       df_Si_first,
                       df_Si_tot)


  }

  df_Si_Sti <- df_Si_Sti[-1, ]

  df_Si_Sti
}


#'
#'
#'
#'
#'
#'
#' function to create figure showing sensitivity index for all parameters
fig_sensitivy_indices <- function(sensi_tib,
                                  object_type, # either "file" if need to be generated in the output folder, or "output" for use in Rmd
                                  name_file) {
  figure <- sensi_tib |>
    ggplot2::ggplot() +
    ggplot2::geom_boxplot(ggplot2::aes(x = Input, y = original, fill = Sensitivity)) +
    ggplot2::scale_fill_manual(values = c("#278B9AFF", "#E75B64FF")) +
    ggplot2::ylab("Sobol sensivity indice") +
    ggplot2::xlab("Model parameter") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1, size = 12),
                   axis.title.x = ggplot2::element_text(face = "bold", size = 14),
                   axis.text.y = ggplot2::element_text(size = 12),
                   axis.title.y = ggplot2::element_text(face = "bold", size = 14),
                   legend.title = ggplot2::element_blank())

  if (object_type == "file") {
    ggplot2::ggsave(paste0("output/", name_file, ".jpg"),
                    width = 8,
                    height = 5)
  } else {
    figure
  }
}

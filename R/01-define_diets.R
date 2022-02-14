##--------------------------------------------------------------------------------------------------------
## SCRIPT : Define diets of pinnipeds species
## 01-define_diets.R
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# This script should contain FUNCTIONS ONLY

#'
#'
#'
#'
#'
#'
#'
create_diet_tib <- function() {

  tibble::tribble(~ Species, ~ fish, ~ cephalopods, ~ krill, ~ mammal, ~ bird, ~ Sources,
                  "Hydrurga leptonyx", 0.25, 0, 0.25, 0.25, 0.25, "Green & Williams 1986, Walker et al 1998, Zhao et al 2004,
                            Hall-Aspland et al 2004, Hall-Aspland et al 2005, Kuhn et al 2006, Forcada et al 2009, Casaux et al 2009,
                            Southwell et al 2012",
                  "Lobodon carcinophaga", 0.05, 0.05, 0.9, 0, 0, "Green & Williams 1986, Zhao et al 2004, Dykyy 2009,
                            Southwell et al 2012, Brault et al 2019",
                  "Ommatophoca rossii", 0.25, 0.65, 0.1, 0, 0, "Skinner et al 1994, Zhao et al 2004, Blix & Nordoy 2007,
                            Southwell et al 2012, Brault et al 2019, Wege et al 2021",
                  "Leptonychotes weddellii", 0.85, 0.18, 0.02, 0, 0, "Casaux et al 1997, Lake et al 2003, Zhao et al 2004,
                            Casaux et al 2006,  Southwell et al 2012, Negri et al 2016, Goetz et al 2017, Hückstädt et al 2017,
                            Brault et al 2019") |>
    tidyr::nest(Diet = c(fish:bird))

}

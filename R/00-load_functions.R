##--------------------------------------------------------------------------------------------------------
## SCRIPT : Compute Fe and nrj contents of diets
## 00-load_functions.R
##
## Authors : Lola Gilbert
## Last update : 2022-02
## R version 4.1.2 (2021-11-01) -- "Bird Hippie"
##--------------------------------------------------------------------------------------------------------

# load excel files
load_xl <- function(pathxl) {
  readxl::read_excel(pathxl)
}

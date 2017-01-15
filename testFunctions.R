# Instructions to update package
# https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
library(roxygen2)
library(devtools)
devtools::install_github("hadley/tidyverse")
library(tidyverse)
setwd('~/censusFunctions')
document()
#setwd("..")
#install('censusFunctions')
#create("matrices")
censusFunction1(1978)
censusFunction1(1976)
censusFunction1(1977)

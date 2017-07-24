#' initial setup code
#' This function wraps setup code
#' @param arg Defaults to true.
#' @keywords setup
#' @export preSetUp
#' @examples
#' preSetUp(TRUE)
#' preSetUp()

preSetUp <- function(arg = TRUE) {

library(thesisdown)
setwd("/Volumes/Bmac/febThesis/index")
#installCensusFunctions("local")
Sys.setenv(TZ="Europe/Berlin")
devtools::install_local("/Volumes/Bmac/censusFunctions")
library(censusFunctions)
## source("../includes/setColors.R")  ## hide pretty colors for faster load
year <- 2004:2014
stockData("saved")

}
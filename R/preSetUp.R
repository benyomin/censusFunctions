#' initial setup code ; v.0.5 load frames with regions
#' This function wraps setup code
#' @param arg Defaults to true.
#' @family setup
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
# devtools::install_local("/Volumes/Bmac/censusFunctions")
library(censusFunctions)
## source("../includes/setColors.R")  ## hide pretty colors for faster load
year <<- 2004:2014

## "2017-08-03 12:06:40 EEST"
## mergedRenters2    <<- readRDS("../savedData/mergedRenters2.rds")
## mergedOwners2     <<- readRDS("../savedData/mergedOwners2.rds")
## mergedData2       <<- readRDS("../savedData/mergedData2.rds")
## cityList          <<- readRDS("../savedData/cityList.rds")

  ## new version

makeRegions("load")
return("preSepUp() successful.")
}

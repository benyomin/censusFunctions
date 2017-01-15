#' Loads the required packages for the
#' Housing Wealth Effect Analysis.
#' Provides for simple dependency management to aid in reproducibility.
#' v.Jan12-2017 - changes to tidyverse instead of tidyr
#' @param load Defaults to TRUE.
#' @keywords packages, packageManagement, dependencies, library, require, requirements
#' @export
#' @examples
#' loadWealthEffectPackages()
loadWealthEffectPackages <- function(load=TRUE){
  if(load==TRUE){
        install_if_needed("tidyverse")
        library(tidyverse)
        install_if_needed("reshape2")
        library(reshape2)
        install_if_needed("roxygen2")
        library(roxygen2)
        install_if_needed("foreign")
        library(foreign)
        install_if_needed("rJava")
        library(rJava)
        install_if_needed("xlsx")
        library(xlsx)
        install_if_needed("RCurl")
        library(RCurl)
        install_if_needed("zoo")
        library(zoo)
        install_if_needed("dtplyr")
        library(dtplyr)
        install_if_needed("gdata")
        library(gdata)
        #install_if_needed("operators")
        #library(operators)
        #install_if_needed("grid")
        #library(grid)
        install_if_needed("bookdown")
        library(bookdown)
        install_if_needed("scales")
        library(scales)
        install_if_needed("Hmisc")
        library(Hmisc)
        install_if_needed("memisc")
        library(memisc)
    }
    else {
        print("Helper Packages not loaded by censusFunctions.")
    }
}

#' Loads the required packages for the
#' Housing Wealth Effect Analysis.
#' Provides for simple dependency management to aid in reproducibility.
#' @param load Defaults to TRUE.
#' @keywords packages, packageManagement, dependencies, library, require, requirements
#' @export
#' @examples
#' loadWealthEffectPackages()
loadWealthEffectPackages <- function(load=TRUE){
    if(load==TRUE){
        install_if_needed("dplyr")
        library(dplyr)
        install_if_needed("reshape2")
        library(reshape2)
        install_if_needed("tidyr")
        library(tidyr)
        install_if_needed("roxygen2")
        library(roxygen2)
        install_if_needed("purrr")
        library(purrr)
        install_if_needed("broom")
        library(broom)
        install_if_needed("foreign")
        library(foreign)
        install_if_needed("rJava")
        library(rJava)
        install_if_needed("xlsx")
        library(xlsx)
        install_if_needed("magrittr")
        library(magrittr)
        install_if_needed("ggplot2")
        library(ggplot2)
        install_if_needed("RCurl")
        library(RCurl)
        install_if_needed("zoo")
        library(zoo)
        install_if_needed("data.table")
        library(data.table)
        install_if_needed("gdata")
        library(gdata)
        install_if_needed("readr")
        library(readr)
        library(dtplyr)
        install_if_needed("operators")
        library(operators)
        install_if_needed("grid")
        library(grid)
    }
    else {
        print("Helper Packages not loaded by censusFunctions.")
    }
}

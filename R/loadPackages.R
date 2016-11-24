#' Loads the required packages for the
#' Housing Wealth Effect Analysis.
#' @param load Defaults to TRUE.
#' @keywords packages packageManagement dependencies library require requirements
#' @export
#' @examples
#' loadWealthEffectPackages()
loadWealthEffectPackages <- function(load=TRUE){
    if(load==TRUE){
        library(dplyr)
        library(reshape2)
        library(tidyr)
        library(roxygen2)
        library(purrr)
        library(broom)
        library(foreign)
        library(rJava)
        library(xlsx)
        library(magrittr)
        library(ggplot2)

    }
    else {
        print("Helper Packages not loaded by censusFunctions.")
    }
}

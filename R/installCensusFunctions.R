#' Helper function to change directories. v.1.3
#'
#' This function installs the local or github version of Israel Census helper functions.
#' @param location Either local or github.
#' @keywords dir, directory
#' @export
#' @family setup
#' @examples
#' installCensusFunctions('local')
#' installCensusFunctions('github')
installCensusFunctions <- function(location) {
    if (location == "local") {
        chngDir("index")
        devtools::install("~/censusFunctions")  ##local version
        library(censusFunctions)
        return("install local development version of censusFunctions")
    } else if (location == "github") {
        devtools::install_github("benyomin/censusFunctions")
        library(censusFunctions)
        return("installed from github")
    } else {
        return("invalid option, error code 999dd43dgp")
    }
}

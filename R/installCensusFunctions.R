#' Helper function to change directories.
#'
#' This function installs the local or github version of Israel Census helper functions.
#' @param location Either local or github.
#' @keywords dir, directory
#' @export
#' @examples
#' installCensusFunctions("local")
#' installCensusFunctions("github")
installCensusFunctions <- function(location){
  if(arg=="local"){
    chngDir("package")
    devtools::install("~/censusFunctions")  ##local version
    library(censusFunctions)
    chngDir("paper")
   return("install local development version of censusFunctions")
  }else if(arg=="github"){
    devtools::install_github("benyomin/censusFunctions")
    library(censusFunctions)
    return("installed from github")
  }else{return("invalid option, error code 999dd43dgp")}}

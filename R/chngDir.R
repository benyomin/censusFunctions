#' Helper function to change directories.
#'
#' This function moves between 2 directories.
#' @param arg Paper or package?
#' @keywords dir, directory
#' @export
#' @examples
#' chngDir(package)
#' chngDir(paper)
chngDir <- function(arg){
  if(arg=="package"){
    setwd('~/')
    ##      cd to local package censusFunctions
  }else if(arg=="paper"){
    setwd('~/proposal/secret/')
    ##      cd to where we build the pdf
  }else{return("invalid option, error code q3543dgp")}}

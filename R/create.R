#' Gather statistics from multiple years and create a matrix
#'
#' This function imports Israeli census data.
#' @param arg1 What am I creating? Defaults to matrices.
#' @keywords process, aggregate, data
#' @export
#' @examples
#' create("matrices")
#' create("means")
create <- function(arg1){
    if(arg1=="matrices"){
        source("../includes/familyConsumption.R",  echo=FALSE)
        return(1)
    }else if (arg1=="means"){
        source("../includes/meanConsumption.R",    echo=FALSE)
        return(1)
    }else{return(0)}
}

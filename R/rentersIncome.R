#' Divide into owners and renters. v. .79 - doesn't run
#'
#' Creates dataframes based on payment of rent..
#' @param arg Placeholder.
#' @keywords renter, owner, subset
#' @export
#' @examples
#' rentersIncome()
rentersIncome <- function(arg = TRUE) {
    someyears <- c(2004:2014)
    meanYRenters <- c(mean(ownership("renters", 2004)[, 1]), mean(ownership("renters", 
        2005)[, 1]), mean(ownership("renters", 2006)[, 1]), mean(ownership("renters", 
        2007)[, 1]), mean(ownership("renters", 2008)[, 1]), mean(ownership("renters", 
        2009)[, 1]), mean(ownership("renters", 2010)[, 1]), mean(ownership("renters", 
        2011)[, 1]), mean(ownership("renters", 2012)[, 1]), mean(ownership("renters", 
        2013)[, 1]), mean(ownership("renters", 2014)[, 1]))
    matYRenters <- as.data.frame(cbind(someyears, meanYRenters))
    names(matYRenters)[2] <- "Renter"
    return(matYRenters)
}

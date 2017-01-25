#' Creates dataframes expYYYYowners and expYYYYrenters,
#' calls weighHouseholds() ; 
#' This function creates subsets of renters and owners.
#' @param arg Defaults to true.
#' @keywords owners, renters, subset
#' @export
#' @examples
#' subsetByOwnership()
#' subsetByOwnership(1)
#' subsetByOwnership(FALSE)

subsetByOwnership <- function(arg=TRUE){
  if(arg==TRUE){
    weighHouseholds("renters", "income")
    weighHouseholds("renters", "consumption")
    weighHouseholds("owners", "income")
    weighHouseholds("owners", "consumption")
        return("created subsets owners/renters for all years")
  }else{
        return("not implemented, error: aorns9q4o")
  }
}

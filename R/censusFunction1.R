#' A Census Function
#'
#' This function allows you to do something.
#' @param year
#' @keywords individual, household
#' @export
#' @examples
#' cat_function()
cat_function <- function(year=TRUE){
    if(year==1976){
        print("This is the census data for 1976.")
    }
    else if(year==1977){
        print("The year is 1977")
      }
    else {
        print("Census data for those years not yet implemented.")
    }
}

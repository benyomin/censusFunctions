#' A Census Function
#'
#' This function allows you to do something.
#' @param year The year that data will be returned for.
#' @keywords individual, household
#' @export
#' @examples
#' censusFunction2()
censusFunction1 <- function(year=TRUE){
    if(year){

    }
    else if(year==1977){
        print("The year is 1977")
      }
    else {
        print("Census data for those years not yet implemented.")
    }
}
# assumes:
#source("stats.r")
#import data
#source("importCensus.r")
# #how many columns are there? = 74
# ncol(file456)
# #age
# summary(file456$AGE)
# #age is grouped by 0-4 years old, 5-9, 10-14, etc. until 85+
# hist(file456$AGE)
# #subset a few columns
# subSetInd2014<-dplyr::select(file456,YEARSUR,WPLDIST,USSRBORN,INCSAL)
# ##subset to only 3 cities, easier to copy DFW boxplot code
# subCitiesInd2014 <- subSetInd2014 %>% dplyr::filter(WPLDIST=="Center"    |
#                                                         WPLDIST=="Tel-Aviv"  |
#                                                         WPLDIST=="Gush Dan")

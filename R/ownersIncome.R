#' Divide into owners and renters.
#'
#' Creates dataframes based on payment of rent..
#' @param arg Placeholder.
#' @keywords renter, owner, subset
#' @export
#' @examples
#' ownersIncome()
ownersIncome <- function(arg=TRUE){
  someyears   <- c(2004:2014)
  meanYOwners<-c(mean(ownership("owners",  2004)[,1]),
                  mean(ownership("owners",  2005)[,1]),
                  mean(ownership("owners",  2006)[,1]),
                  mean(ownership("owners",  2007)[,1]),
                  mean(ownership("owners",  2008)[,1]),
                  mean(ownership("owners",  2009)[,1]),
                  mean(ownership("owners",  2010)[,1]),
                  mean(ownership("owners",  2011)[,1]),
                  mean(ownership("owners",  2012)[,1]),
                  mean(ownership("owners",  2013)[,1]),
                  mean(ownership("owners",  2014)[,1])
                 )
  matYOwners <-as.data.frame(cbind(someyears, meanYOwners ))
  names(matYOwners)[2]<-"Owner-occupied"
return(matYOwners)
}

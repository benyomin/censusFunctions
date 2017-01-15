#' Applies Weights to households.
#'
#' This function weighs Israeli census data. Returns 1 of 6 options mat[owner/renter]Weighted[Income/Consumption/NHC].
#' @param metric Takes values of income, consumption or NHC.
#' @keywords process, aggregate, data
#' @export
#' @examples
#' tidyFrame("income")
#' tidyFrame("consumption")
#' tidyFrame("NHC")
tidyFrames <- function(metric){
  if(metric=="income"){
 renterWeightedIncome   <-censusFunctions::weighHouseholds("renters","income")
 ownerWeightedIncome    <-censusFunctions::weighHouseholds("owners","income")
 allWeightedIncome <-
  Hmisc::Merge(ownerWeightedIncome,
               renterWeightedIncome, id=~someyears)

 allWeightedIncome <- tidyr::gather(allWeightedIncome, someyears)
names(allWeightedIncome)[1]<-"year"
 names(allWeightedIncome)[2]<-"key"
 return(allWeightedIncome)
  }else if(metric=="consumption"){
 renterWeightedConsumption <-censusFunctions::weighHouseholds("renters","consumption")
 ownerWeightedConsumption  <-censusFunctions::weighHouseholds("owners","consumption")
       someyears   <- c(2004:2014)
allWeightedConsumption <-
  Hmisc::Merge(ownerWeightedConsumption,
               renterWeightedConsumption, id=~someyears)
allWeightedConsumption <<- tidyr::gather(allWeightedConsumption, someyears)
names(allWeightedConsumption)[1]<-"year"
names(allWeightedConsumption)[2]<-"key"
return(allWeightedConsumption)
  }else if(metric=="NHC"){
    renterWeightedNHC   <-censusFunctions::weighHouseholds("renters","NHC")
    ownerWeightedNHC    <-censusFunctions::weighHouseholds("owners","NHC")
    allWeightedNHC <-
  Hmisc::Merge(ownerWeightedNHC,
               renterWeightedNHC, id=~someyears)
    allWeightedNHC <- tidyr::gather(allWeightedNHC, someyears)
    names(allWeightedNHC)[1]<-"year"
    names(allWeightedNHC)[2]<-"key"
return(allWeightedNHC)
  }else{return("only metrics income/consumption/NHC")}
    }

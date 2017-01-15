#' Applies Weights to households.
#'
#' This function weighs Israeli census data.
#' @param arg1 Defaults to TRUE.
#' @keywords process, aggregate, data
#' @export
#' @examples
#' weighHouseholds(1)
#' weighHouseholds(0)
weighHouseholds <- function(arg1=1){
    if(arg1==TRUE){
      ##create variables

   allWeightedNHC      = 0
   renterWeightedNHC   = 0
   ownerWeightedNHC    = 0
allWeightedIncome      = 0
renterWeightedIncome   = 0
ownerWeightedIncome    = 0
allWeightedConsumption = 0
renterWeightedConsumption = 0
ownerWeightedConsumption  = 0
someyears   <- c(2004:2014)
##existing variables
##exp2004df {
           ##exp2004s
           ##exp2004owners  :: derives from exp2004s
           ##exp2004renters :: derives from exp2004s } 
##previous new column $NHC was added to exp2004owners/exp2004renters
#1)#add weighting column to exp2004s
##it will propagate through to owners/renters
#2)#modify code for $NHC to include proper weighting
##weightedNHC = weight * NHC
#head(exp2004s[,3]) :: total consumption
#exp2004s$HHNUM     Household number
#head(exp2004s[,7]) Household number
                                        #head(exp2004s[,6]) Weight
meanAllWeightedIncome <-
                   c(mean(exp2004s$allWeightedIncome),
                     mean(exp2005s$allWeightedIncome),
                     mean(exp2006s$allWeightedIncome),
                     mean(exp2007s$allWeightedIncome),
                     mean(exp2008s$allWeightedIncome),
                     mean(exp2009s$allWeightedIncome),
                     mean(exp2010s$allWeightedIncome),
                     mean(exp2011s$allWeightedIncome),    
                     mean(exp2012s$allWeightedIncome),
                     mean(exp2013s$allWeightedIncome),
                     mean(exp2014s$allWeightedIncome))
matAllWeightedIncome <-as.data.frame(cbind(someyears,meanAllWeightedIncome))

meanAllWeightedConsumption <-
                   c(mean(exp2004s$allWeightedConsumption),
                     mean(exp2005s$allWeightedConsumption),
                     mean(exp2006s$allWeightedConsumption),
                     mean(exp2007s$allWeightedConsumption),
                     mean(exp2008s$allWeightedConsumption),
                     mean(exp2009s$allWeightedConsumption),
                     mean(exp2010s$allWeightedConsumption),
                     mean(exp2011s$allWeightedConsumption),    
                     mean(exp2012s$allWeightedConsumption),
                     mean(exp2013s$allWeightedConsumption),
                     mean(exp2014s$allWeightedConsumption))
matAllWeightedConsumption <-as.data.frame(cbind(someyears,meanAllWeightedConsumption))
meanOwnerWeightedIncome<- 
                   c(mean(exp2004owners$ownerWeightedIncome),
                     mean(exp2005owners$ownerWeightedIncome),
                     mean(exp2006owners$ownerWeightedIncome),
                     mean(exp2007owners$ownerWeightedIncome),
                     mean(exp2008owners$ownerWeightedIncome),
                     mean(exp2009owners$ownerWeightedIncome),
                     mean(exp2010owners$ownerWeightedIncome),
                     mean(exp2011owners$ownerWeightedIncome),    
                     mean(exp2012owners$ownerWeightedIncome),
                     mean(exp2013owners$ownerWeightedIncome),
                     mean(exp2014owners$ownerWeightedIncome))
matOwnerWeightedIncome <-as.data.frame(cbind(someyears,meanOwnerWeightedIncome))
meanOwnerWeightedConsumption<- 
                   c(mean(exp2004owners$ownerWeightedConsumption),
                     mean(exp2005owners$ownerWeightedConsumption),
                     mean(exp2006owners$ownerWeightedConsumption),
                     mean(exp2007owners$ownerWeightedConsumption),
                     mean(exp2008owners$ownerWeightedConsumption),
                     mean(exp2009owners$ownerWeightedConsumption),
                     mean(exp2010owners$ownerWeightedConsumption),
                     mean(exp2011owners$ownerWeightedConsumption),    
                     mean(exp2012owners$ownerWeightedConsumption),
                     mean(exp2013owners$ownerWeightedConsumption),
                     mean(exp2014owners$ownerWeightedConsumption))
matOwnerWeightedConsumption <-as.data.frame(cbind(someyears,meanOwnerWeightedConsumption))
meanOwnerWeightedNHC<- 
                   c(mean(exp2004owners$ownerWeightedNHC),
                     mean(exp2005owners$ownerWeightedNHC),
                     mean(exp2006owners$ownerWeightedNHC),
                     mean(exp2007owners$ownerWeightedNHC),
                     mean(exp2008owners$ownerWeightedNHC),
                     mean(exp2009owners$ownerWeightedNHC),
                     mean(exp2010owners$ownerWeightedNHC),
                     mean(exp2011owners$ownerWeightedNHC),    
                     mean(exp2012owners$ownerWeightedNHC),
                     mean(exp2013owners$ownerWeightedNHC),
                     mean(exp2014owners$ownerWeightedNHC))
matOwnerWeightedNHC <-as.data.frame(cbind(someyears,meanOwnerWeightedNHC))
meanRenterWeightedIncome<- 
                   c(mean(exp2004renters$renterWeightedIncome),
                     mean(exp2005renters$renterWeightedIncome),
                     mean(exp2006renters$renterWeightedIncome),
                     mean(exp2007renters$renterWeightedIncome),
                     mean(exp2008renters$renterWeightedIncome),
                     mean(exp2009renters$renterWeightedIncome),
                     mean(exp2010renters$renterWeightedIncome),
                     mean(exp2011renters$renterWeightedIncome),    
                     mean(exp2012renters$renterWeightedIncome),
                     mean(exp2013renters$renterWeightedIncome),
                     mean(exp2014renters$renterWeightedIncome))
meanRenterWeightedConsumption<- 
                   c(mean(exp2004renters$renterWeightedConsumption),
                     mean(exp2005renters$renterWeightedConsumption),
                     mean(exp2006renters$renterWeightedConsumption),
                     mean(exp2007renters$renterWeightedConsumption),
                     mean(exp2008renters$renterWeightedConsumption),
                     mean(exp2009renters$renterWeightedConsumption),
                     mean(exp2010renters$renterWeightedConsumption),
                     mean(exp2011renters$renterWeightedConsumption),    
                     mean(exp2012renters$renterWeightedConsumption),
                     mean(exp2013renters$renterWeightedConsumption),
                     mean(exp2014renters$renterWeightedConsumption))
meanRenterWeightedNHC<- 
                   c(mean(exp2004renters$renterWeightedNHC),
                     mean(exp2005renters$renterWeightedNHC),
                     mean(exp2006renters$renterWeightedNHC),
                     mean(exp2007renters$renterWeightedNHC),
                     mean(exp2008renters$renterWeightedNHC),
                     mean(exp2009renters$renterWeightedNHC),
                     mean(exp2010renters$renterWeightedNHC),
                     mean(exp2011renters$renterWeightedNHC),    
                     mean(exp2012renters$renterWeightedNHC),
                     mean(exp2013renters$renterWeightedNHC),
                     mean(exp2014renters$renterWeightedNHC))

matRenterWeightedIncome <-as.data.frame(cbind(someyears,meanRenterWeightedIncome))
matRenterWeightedConsumption <-as.data.frame(cbind(someyears,meanRenterWeightedConsumption))
matRenterWeightedNHC <-as.data.frame(cbind(someyears,meanRenterWeightedNHC))
## combine the matrices via "merge"
allWeightedIncome <-
  Hmisc::Merge(matAllWeightedIncome, matOwnerWeightedIncome,
               matRenterWeightedIncome, id=~someyears)
allWeightedConsumption <-
  Hmisc::Merge(matAllWeightedConsumption, matOwnerWeightedConsumption,
               matRenterWeightedConsumption, id=~someyears)
allWeightedNHC <-
  Hmisc::Merge(matOwnerWeightedNHC,
               matRenterWeightedNHC, id=~someyears)
## make a long, tidy data frame
allWeightedIncome <- tidyr::gather(allWeightedIncome, someyears)
names(allWeightedIncome)[1]<-"year"
names(allWeightedIncome)[2]<-"key"

allWeightedConsumption <<- tidyr::gather(allWeightedConsumption, someyears)
names(allWeightedConsumption)[1]<-"year"
names(allWeightedConsumption)[2]<-"key"

allWeightedNHC <- tidyr::gather(allWeightedNHC, someyears)
names(allWeightedNHC)[1]<-"year"
names(allWeightedNHC)[2]<-"key"

        return(1)
    }else if (arg1==0){
        return(1)
    }else{return(0)}
}

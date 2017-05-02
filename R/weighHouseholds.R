#' Applies Weights to households.
#' Calls ownership() ; returns expYYYY[renters/owners]
#' This function weighs Israeli census data. Returns 1 of 6 options mat[owner/renter]Weighted[Income/Consumption/NHC].
#' @param class Takes values of renters or owners.
#' @param metric Takes values of income, consumption or NHC.
#' @keywords process, aggregate, data
#' @export
#' @examples
#' weighHouseholds('renters','income')
#' weighHouseholds('owners','consumption')
#' weighHouseholds('renters','NHC')

#### uses new column names [1] 'grossIncome' 'rentalIncome' 'totalConsumption' [4]
#### 'rentEXP' 'imputedRent' 'WEIGHT' [7] 'HHNUM' 'year'
weighHouseholds <- function(class, metric) {
    ## declare variables
    someyears <- c(2004:2014)
    ## make data available for parsing subset only 8 colums from >100 subset renters
    ## double arrow <<- to make globally available
    exp2004renters <<- ownership("renters", 2004)
    exp2005renters <<- ownership("renters", 2005)
    exp2006renters <<- ownership("renters", 2006)
    exp2007renters <<- ownership("renters", 2007)
    exp2008renters <<- ownership("renters", 2008)
    exp2009renters <<- ownership("renters", 2009)
    exp2010renters <<- ownership("renters", 2010)
    exp2011renters <<- ownership("renters", 2011)
    exp2012renters <<- ownership("renters", 2012)
    exp2013renters <<- ownership("renters", 2013)
    exp2014renters <<- ownership("renters", 2014)
    ## subset owners double arrow <<- to make globally available
    exp2004owners <<- ownership("owners", 2004)
    exp2005owners <<- ownership("owners", 2005)
    exp2006owners <<- ownership("owners", 2006)
    exp2007owners <<- ownership("owners", 2007)
    exp2008owners <<- ownership("owners", 2008)
    exp2009owners <<- ownership("owners", 2009)
    exp2010owners <<- ownership("owners", 2010)
    exp2011owners <<- ownership("owners", 2011)
    exp2012owners <<- ownership("owners", 2012)
    exp2013owners <<- ownership("owners", 2013)
    exp2014owners <<- ownership("owners", 2014)
    
    if (class == "renters") {
        if (metric == "keymoney") {
            years = (2004:2014)
            # ## call for side effect of <<-
            Map(ownership, "keymoney", years)
            Map(ownership, "notkey", years)
        } else if (metric == "income") {
            exp2004renters$renterWeightedIncome <<- (exp2004renters$grossIncome * 
                exp2004renters$WEIGHT)/100
            exp2005renters$renterWeightedIncome <<- (exp2005renters$grossIncome * 
                exp2005renters$WEIGHT)/100
            exp2006renters$renterWeightedIncome <<- (exp2006renters$grossIncome * 
                exp2006renters$WEIGHT)/100
            exp2007renters$renterWeightedIncome <<- (exp2007renters$grossIncome * 
                exp2007renters$WEIGHT)/100
            exp2008renters$renterWeightedIncome <<- (exp2008renters$grossIncome * 
                exp2008renters$WEIGHT)/100
            exp2009renters$renterWeightedIncome <<- (exp2009renters$grossIncome * 
                exp2009renters$WEIGHT)/100
            exp2010renters$renterWeightedIncome <<- (exp2010renters$grossIncome * 
                exp2010renters$WEIGHT)/100
            exp2011renters$renterWeightedIncome <<- (exp2011renters$grossIncome * 
                exp2011renters$WEIGHT)/100
            exp2012renters$renterWeightedIncome <<- (exp2012renters$grossIncome * 
                exp2012renters$WEIGHT)/100
            exp2013renters$renterWeightedIncome <<- (exp2013renters$grossIncome * 
                exp2013renters$WEIGHT)/100
            exp2014renters$renterWeightedIncome <<- (exp2014renters$grossIncome * 
                exp2014renters$WEIGHT)/100
            meanRenterWeightedIncome <- c(mean(exp2004renters$renterWeightedIncome), 
                mean(exp2005renters$renterWeightedIncome), mean(exp2006renters$renterWeightedIncome), 
                mean(exp2007renters$renterWeightedIncome), mean(exp2008renters$renterWeightedIncome), 
                mean(exp2009renters$renterWeightedIncome), mean(exp2010renters$renterWeightedIncome), 
                mean(exp2011renters$renterWeightedIncome), mean(exp2012renters$renterWeightedIncome), 
                mean(exp2013renters$renterWeightedIncome), mean(exp2014renters$renterWeightedIncome))
            matRenterWeightedIncome <- as.data.frame(cbind(someyears, meanRenterWeightedIncome))
            return(matRenterWeightedIncome)
        } else if (metric == "consumption") {
            exp2004renters$renterWeightedConsumption <<- (exp2004renters[, 3] * exp2004renters$WEIGHT)/100
            exp2005renters$renterWeightedConsumption <<- (exp2005renters[, 3] * exp2005renters$WEIGHT)/100
            exp2006renters$renterWeightedConsumption <<- (exp2006renters[, 3] * exp2006renters$WEIGHT)/100
            exp2007renters$renterWeightedConsumption <<- (exp2007renters[, 3] * exp2007renters$WEIGHT)/100
            exp2008renters$renterWeightedConsumption <<- (exp2008renters[, 3] * exp2008renters$WEIGHT)/100
            exp2009renters$renterWeightedConsumption <<- (exp2009renters[, 3] * exp2009renters$WEIGHT)/100
            exp2010renters$renterWeightedConsumption <<- (exp2010renters[, 3] * exp2010renters$WEIGHT)/100
            exp2011renters$renterWeightedConsumption <<- (exp2011renters[, 3] * exp2011renters$WEIGHT)/100
            exp2012renters$renterWeightedConsumption <<- (exp2012renters[, 3] * exp2012renters$WEIGHT)/100
            exp2013renters$renterWeightedConsumption <<- (exp2013renters[, 3] * exp2013renters$WEIGHT)/100
            exp2014renters$renterWeightedConsumption <<- (exp2014renters[, 3] * exp2014renters$WEIGHT)/100
            meanRenterWeightedConsumption <- c(mean(exp2004renters$renterWeightedConsumption), 
                mean(exp2005renters$renterWeightedConsumption), mean(exp2006renters$renterWeightedConsumption), 
                mean(exp2007renters$renterWeightedConsumption), mean(exp2008renters$renterWeightedConsumption), 
                mean(exp2009renters$renterWeightedConsumption), mean(exp2010renters$renterWeightedConsumption), 
                mean(exp2011renters$renterWeightedConsumption), mean(exp2012renters$renterWeightedConsumption), 
                mean(exp2013renters$renterWeightedConsumption), mean(exp2014renters$renterWeightedConsumption))
            matRenterWeightedConsumption <- as.data.frame(cbind(someyears, meanRenterWeightedConsumption))
            return(matRenterWeightedConsumption)
        } else if (metric == "NHC") {
            exp2004renters$renterWeightedNHC <<- (exp2004renters$NHC * exp2004renters$WEIGHT)/100
            exp2005renters$renterWeightedNHC <<- (exp2005renters$NHC * exp2005renters$WEIGHT)/100
            exp2006renters$renterWeightedNHC <<- (exp2006renters$NHC * exp2006renters$WEIGHT)/100
            exp2007renters$renterWeightedNHC <<- (exp2007renters$NHC * exp2007renters$WEIGHT)/100
            exp2008renters$renterWeightedNHC <<- (exp2008renters$NHC * exp2008renters$WEIGHT)/100
            exp2009renters$renterWeightedNHC <<- (exp2009renters$NHC * exp2009renters$WEIGHT)/100
            exp2010renters$renterWeightedNHC <<- (exp2010renters$NHC * exp2010renters$WEIGHT)/100
            exp2011renters$renterWeightedNHC <<- (exp2011renters$NHC * exp2011renters$WEIGHT)/100
            exp2012renters$renterWeightedNHC <<- (exp2012renters$NHC * exp2012renters$WEIGHT)/100
            exp2013renters$renterWeightedNHC <<- (exp2013renters$NHC * exp2013renters$WEIGHT)/100
            exp2014renters$renterWeightedNHC <<- (exp2014renters$NHC * exp2014renters$WEIGHT)/100
            meanRenterWeightedNHC <- c(mean(exp2004renters$renterWeightedNHC), mean(exp2005renters$renterWeightedNHC), 
                mean(exp2006renters$renterWeightedNHC), mean(exp2007renters$renterWeightedNHC), 
                mean(exp2008renters$renterWeightedNHC), mean(exp2009renters$renterWeightedNHC), 
                mean(exp2010renters$renterWeightedNHC), mean(exp2011renters$renterWeightedNHC), 
                mean(exp2012renters$renterWeightedNHC), mean(exp2013renters$renterWeightedNHC), 
                mean(exp2014renters$renterWeightedNHC))
            matRenterWeightedNHC <- as.data.frame(cbind(someyears, meanRenterWeightedNHC))
            return(matRenterWeightedNHC)
        } else {
            return("valid metric args are income/consumption/NHC")
        }
    } else if (class == "owners") {
        if (metric == "income") {
            exp2004owners$ownerWeightedIncome <<- (exp2004owners$grossIncome * exp2004owners$WEIGHT)/100
            exp2005owners$ownerWeightedIncome <<- (exp2005owners$grossIncome * exp2005owners$WEIGHT)/100
            exp2006owners$ownerWeightedIncome <<- (exp2006owners$grossIncome * exp2006owners$WEIGHT)/100
            exp2007owners$ownerWeightedIncome <<- (exp2007owners$grossIncome * exp2007owners$WEIGHT)/100
            exp2008owners$ownerWeightedIncome <<- (exp2008owners$grossIncome * exp2008owners$WEIGHT)/100
            exp2009owners$ownerWeightedIncome <<- (exp2009owners$grossIncome * exp2009owners$WEIGHT)/100
            exp2010owners$ownerWeightedIncome <<- (exp2010owners$grossIncome * exp2010owners$WEIGHT)/100
            exp2011owners$ownerWeightedIncome <<- (exp2011owners$grossIncome * exp2011owners$WEIGHT)/100
            exp2012owners$ownerWeightedIncome <<- (exp2012owners$grossIncome * exp2012owners$WEIGHT)/100
            exp2013owners$ownerWeightedIncome <<- (exp2013owners$grossIncome * exp2013owners$WEIGHT)/100
            exp2014owners$ownerWeightedIncome <<- (exp2014owners$grossIncome * exp2014owners$WEIGHT)/100
            meanOwnerWeightedIncome <- c(mean(exp2004owners$ownerWeightedIncome), 
                mean(exp2005owners$ownerWeightedIncome), mean(exp2006owners$ownerWeightedIncome), 
                mean(exp2007owners$ownerWeightedIncome), mean(exp2008owners$ownerWeightedIncome), 
                mean(exp2009owners$ownerWeightedIncome), mean(exp2010owners$ownerWeightedIncome), 
                mean(exp2011owners$ownerWeightedIncome), mean(exp2012owners$ownerWeightedIncome), 
                mean(exp2013owners$ownerWeightedIncome), mean(exp2014owners$ownerWeightedIncome))
            matOwnerWeightedIncome <- as.data.frame(cbind(someyears, meanOwnerWeightedIncome))
            return(matOwnerWeightedIncome)
        } else if (metric == "consumption") {
            exp2004owners$ownerWeightedConsumption <<- (exp2004owners[, 3] * exp2004owners$WEIGHT)/100
            exp2005owners$ownerWeightedConsumption <<- (exp2005owners[, 3] * exp2005owners$WEIGHT)/100
            exp2006owners$ownerWeightedConsumption <<- (exp2006owners[, 3] * exp2006owners$WEIGHT)/100
            exp2007owners$ownerWeightedConsumption <<- (exp2007owners[, 3] * exp2007owners$WEIGHT)/100
            exp2008owners$ownerWeightedConsumption <<- (exp2008owners[, 3] * exp2008owners$WEIGHT)/100
            exp2009owners$ownerWeightedConsumption <<- (exp2009owners[, 3] * exp2009owners$WEIGHT)/100
            exp2010owners$ownerWeightedConsumption <<- (exp2010owners[, 3] * exp2010owners$WEIGHT)/100
            exp2011owners$ownerWeightedConsumption <<- (exp2011owners[, 3] * exp2011owners$WEIGHT)/100
            exp2012owners$ownerWeightedConsumption <<- (exp2012owners[, 3] * exp2012owners$WEIGHT)/100
            exp2013owners$ownerWeightedConsumption <<- (exp2013owners[, 3] * exp2013owners$WEIGHT)/100
            exp2014owners$ownerWeightedConsumption <<- (exp2014owners[, 3] * exp2014owners$WEIGHT)/100
            meanOwnerWeightedConsumption <- c(mean(exp2004owners$ownerWeightedConsumption), 
                mean(exp2005owners$ownerWeightedConsumption), mean(exp2006owners$ownerWeightedConsumption), 
                mean(exp2007owners$ownerWeightedConsumption), mean(exp2008owners$ownerWeightedConsumption), 
                mean(exp2009owners$ownerWeightedConsumption), mean(exp2010owners$ownerWeightedConsumption), 
                mean(exp2011owners$ownerWeightedConsumption), mean(exp2012owners$ownerWeightedConsumption), 
                mean(exp2013owners$ownerWeightedConsumption), mean(exp2014owners$ownerWeightedConsumption))
            matOwnerWeightedConsumption <- as.data.frame(cbind(someyears, meanOwnerWeightedConsumption))
            return(matOwnerWeightedConsumption)
        } else if (metric == "NHC") {
            exp2004owners$ownerWeightedNHC <<- (exp2004owners$NHC * exp2004owners$WEIGHT)/100
            exp2005owners$ownerWeightedNHC <<- (exp2005owners$NHC * exp2005owners$WEIGHT)/100
            exp2006owners$ownerWeightedNHC <<- (exp2006owners$NHC * exp2006owners$WEIGHT)/100
            exp2007owners$ownerWeightedNHC <<- (exp2007owners$NHC * exp2007owners$WEIGHT)/100
            exp2008owners$ownerWeightedNHC <<- (exp2008owners$NHC * exp2008owners$WEIGHT)/100
            exp2009owners$ownerWeightedNHC <<- (exp2009owners$NHC * exp2009owners$WEIGHT)/100
            exp2010owners$ownerWeightedNHC <<- (exp2010owners$NHC * exp2010owners$WEIGHT)/100
            exp2011owners$ownerWeightedNHC <<- (exp2011owners$NHC * exp2011owners$WEIGHT)/100
            exp2012owners$ownerWeightedNHC <<- (exp2012owners$NHC * exp2012owners$WEIGHT)/100
            exp2013owners$ownerWeightedNHC <<- (exp2013owners$NHC * exp2013owners$WEIGHT)/100
            exp2014owners$ownerWeightedNHC <<- (exp2014owners$NHC * exp2014owners$WEIGHT)/100
            meanOwnerWeightedNHC <- c(mean(exp2004owners$ownerWeightedNHC), mean(exp2005owners$ownerWeightedNHC), 
                mean(exp2006owners$ownerWeightedNHC), mean(exp2007owners$ownerWeightedNHC), 
                mean(exp2008owners$ownerWeightedNHC), mean(exp2009owners$ownerWeightedNHC), 
                mean(exp2010owners$ownerWeightedNHC), mean(exp2011owners$ownerWeightedNHC), 
                mean(exp2012owners$ownerWeightedNHC), mean(exp2013owners$ownerWeightedNHC), 
                mean(exp2014owners$ownerWeightedNHC))
            matOwnerWeightedNHC <- as.data.frame(cbind(someyears, meanOwnerWeightedNHC))
            return(matOwnerWeightedNHC)
        } else {
            return("invalid metric")
        }
    } else {
        return("takes renters or owners for arg class")
    }
}
## create variables

## allWeightedNHC = 0 renterWeightedNHC = 0 ownerWeightedNHC = 0 allWeightedIncome
## = 0 renterWeightedIncome = 0 ownerWeightedIncome = 0 allWeightedConsumption = 0
## renterWeightedConsumption = 0 ownerWeightedConsumption = 0 someyears <-
## c(2004:2014)

## #exp2004s$allWeightedIncome <-exp2004s$Q1 * exp2004s$WEIGHT
## #exp2004s$allWeightedConsumption <-exp2004s[,3]*exp2004s$WEIGHT

## exp2004owners$ownerWeightedIncome<<-(exp2004owners$Q1 *
## exp2004owners$WEIGHT)/100
## exp2004owners$ownerWeightedConsumption<<-(exp2004owners[,3] *
## exp2004owners$WEIGHT)/100 exp2004owners$ownerWeightedNHC<<-(exp2004owners$NHC *
## exp2004owners$WEIGHT)/100
## exp2004renters$renterWeightedIncome<<-(exp2004renters$Q1 *
## exp2004renters$WEIGHT)/100
## exp2004renters$renterWeightedConsumption<<-(exp2004renters[,3] *
## exp2004renters$WEIGHT)/100
## exp2004renters$renterWeightedNHC<<-(exp2004renters$NHC *
## exp2004renters$WEIGHT)/100



## return (1) # existing variables exp2004df { exp2004s exp2004owners :: derives
## from exp2004s exp2004renters :: derives from exp2004s } previous new column
## $NHC was added to exp2004owners/exp2004renters 1)#add weighting column to
## exp2004s it will propagate through to owners/renters 2)#modify code for $NHC to
## include proper weighting weightedNHC = weight * NHC head(exp2004s[,3]) :: total
## consumption exp2004s$HHNUM Household number head(exp2004s[,7]) Household number
## #head(exp2004s[,6]) Weight meanAllWeightedIncome <-
## c(mean(exp2004s$allWeightedIncome), mean(exp2005s$allWeightedIncome),
## mean(exp2006s$allWeightedIncome), mean(exp2007s$allWeightedIncome),
## mean(exp2008s$allWeightedIncome), mean(exp2009s$allWeightedIncome),
## mean(exp2010s$allWeightedIncome), mean(exp2011s$allWeightedIncome),
## mean(exp2012s$allWeightedIncome), mean(exp2013s$allWeightedIncome),
## mean(exp2014s$allWeightedIncome)) matAllWeightedIncome
## <-as.data.frame(cbind(someyears,meanAllWeightedIncome))

## meanAllWeightedConsumption <- c(mean(exp2004s$allWeightedConsumption),
## mean(exp2005s$allWeightedConsumption), mean(exp2006s$allWeightedConsumption),
## mean(exp2007s$allWeightedConsumption), mean(exp2008s$allWeightedConsumption),
## mean(exp2009s$allWeightedConsumption), mean(exp2010s$allWeightedConsumption),
## mean(exp2011s$allWeightedConsumption), mean(exp2012s$allWeightedConsumption),
## mean(exp2013s$allWeightedConsumption), mean(exp2014s$allWeightedConsumption))
## matAllWeightedConsumption
## <-as.data.frame(cbind(someyears,meanAllWeightedConsumption))
## meanOwnerWeightedIncome<- c(mean(exp2004owners$ownerWeightedIncome),
## mean(exp2005owners$ownerWeightedIncome),
## mean(exp2006owners$ownerWeightedIncome),
## mean(exp2007owners$ownerWeightedIncome),
## mean(exp2008owners$ownerWeightedIncome),
## mean(exp2009owners$ownerWeightedIncome),
## mean(exp2010owners$ownerWeightedIncome),
## mean(exp2011owners$ownerWeightedIncome),
## mean(exp2012owners$ownerWeightedIncome),
## mean(exp2013owners$ownerWeightedIncome),
## mean(exp2014owners$ownerWeightedIncome)) matOwnerWeightedIncome
## <-as.data.frame(cbind(someyears,meanOwnerWeightedIncome))


## # combine the matrices via 'merge' allWeightedIncome <-
## Hmisc::Merge(matAllWeightedIncome, matOwnerWeightedIncome,
## matRenterWeightedIncome, id=~someyears) allWeightedConsumption <-
## Hmisc::Merge(matAllWeightedConsumption, matOwnerWeightedConsumption,
## matRenterWeightedConsumption, id=~someyears) allWeightedNHC <-
## Hmisc::Merge(matOwnerWeightedNHC, matRenterWeightedNHC, id=~someyears) ## make
## a long, tidy data frame allWeightedIncome <- tidyr::gather(allWeightedIncome,
## someyears) names(allWeightedIncome)[1]<-'year'
## names(allWeightedIncome)[2]<-'key'

## allWeightedConsumption <<- tidyr::gather(allWeightedConsumption, someyears)
## names(allWeightedConsumption)[1]<-'year'
## names(allWeightedConsumption)[2]<-'key'

## allWeightedNHC <- tidyr::gather(allWeightedNHC, someyears)
## names(allWeightedNHC)[1]<-'year' names(allWeightedNHC)[2]<-'key'


#' Divide into owners and renters.
#' renters is imputed rent ==0 and includes keymoney and notkey
#' keymoney is renting households whose rent<=120 NIS ($30 USD)
#' notkey is renting households   whose rent >120 NIS ($30 USD)
#' keymoney and notkey rely on renters being called first
#' Is called by: weighHouseholds(), takes expYYYYs
#' Creates dataframes based on payment of rent.
#' @param class owners or renters, defaults to owners.
#' @param year Numeric, from 2004 to 2014
#' @keywords renter, owner, subset
#' @export
#' @examples
#' ownership('renters', 2012)
#' ownership('notkey', 2012)
#' ownership('keymoney', 2012)
#' ownership('owners',  2004)

#### new version ## based on >0 in imputed rent ## uses new column names [1]
#### 'grossIncome' 'rentalIncome' 'totalConsumption' [4] 'rentEXP' 'imputedRent'
#### 'WEIGHT' [7] 'HHNUM' 'year'
ownership <- function(class, year) {
    someyears <- c(2004:2014)
    if (class == "renters") {
        ## renters have 0 imputed rent
        if (year == 2004) {
            return(subset(exp2004s, imputedRent == 0))
        } else if (year == 2005) {
            return(subset(exp2005s, imputedRent == 0))
        } else if (year == 2006) {
            return(subset(exp2006s, imputedRent == 0))
        } else if (year == 2007) {
            return(subset(exp2007s, imputedRent == 0))
        } else if (year == 2008) {
            return(subset(exp2008s, imputedRent == 0))
        } else if (year == 2009) {
            return(subset(exp2009s, imputedRent == 0))
        } else if (year == 2010) {
            return(subset(exp2010s, imputedRent == 0))
        } else if (year == 2011) {
            return(subset(exp2011s, imputedRent == 0))
        } else if (year == 2012) {
            return(subset(exp2012s, imputedRent == 0))
        } else if (year == 2013) {
            return(subset(exp2013s, imputedRent == 0))
        } else if (year == 2014) {
            return(subset(exp2014s, imputedRent == 0))
        } else {
            return(0)
        }
    } else if (class == "keymoney") {
        if (year == 2004) {
            exp2004keyMoney <<- subset(exp2004renters, rentEXP <= 120)
            return(exp2004keyMoney)
        } else if (year == 2005) {
            exp2005keyMoney <<- subset(exp2005renters, rentEXP <= 120)
            return(exp2005keyMoney)
        } else if (year == 2006) {
            exp2006keyMoney <<- subset(exp2006renters, rentEXP <= 120)
            return(exp2006keyMoney)
        } else if (year == 2007) {
            exp2007keyMoney <<- subset(exp2007renters, rentEXP <= 120)
            return(exp2007keyMoney)
        } else if (year == 2008) {
            exp2008keyMoney <<- subset(exp2008renters, rentEXP <= 120)
            return(exp2008keyMoney)
        } else if (year == 2009) {
            exp2009keyMoney <<- subset(exp2009renters, rentEXP <= 120)
            return(exp2009keyMoney)
        } else if (year == 2010) {
            exp2010keyMoney <<- subset(exp2010renters, rentEXP <= 120)
            return(exp2010keyMoney)
        } else if (year == 2011) {
            exp2011keyMoney <<- subset(exp2011renters, rentEXP <= 120)
            return(exp2011keyMoney)
        } else if (year == 2012) {
            exp2012keyMoney <<- subset(exp2012renters, rentEXP <= 120)
            return(exp2012keyMoney)
        } else if (year == 2013) {
            exp2013keyMoney <<- subset(exp2013renters, rentEXP <= 120)
            return(exp2013keyMoney)
        } else if (year == 2014) {
            exp2014keyMoney <<- subset(exp2014renters, rentEXP <= 120)
            return(exp2014keyMoney)
        } else {
            return(0)
        }
    } else if (class == "notkey") {
        if (year == 2004) {
            exp2004notKey <<- subset(exp2004renters, rentEXP > 120)
            return(exp2004notKey)
        } else if (year == 2005) {
            exp2005notKey <<- subset(exp2005renters, rentEXP > 120)
            return(exp2005notKey)
        } else if (year == 2006) {
            exp2006notKey <<- subset(exp2006renters, rentEXP > 120)
            return(exp2006notKey)
        } else if (year == 2007) {
            exp2007notKey <<- subset(exp2007renters, rentEXP > 120)
            return(exp2007notKey)
        } else if (year == 2008) {
            exp2008notKey <<- subset(exp2008renters, rentEXP > 120)
            return(exp2008notKey)
        } else if (year == 2009) {
            exp2009notKey <<- subset(exp2009renters, rentEXP > 120)
            return(exp2009notKey)
        } else if (year == 2010) {
            exp2010notKey <<- subset(exp2010renters, rentEXP > 120)
            return(exp2010notKey)
        } else if (year == 2011) {
            exp2011notKey <<- subset(exp2011renters, rentEXP > 120)
            return(exp2011notKey)
        } else if (year == 2012) {
            exp2012notKey <<- subset(exp2012renters, rentEXP > 120)
            return(exp2012notKey)
        } else if (year == 2013) {
            exp2013notKey <<- subset(exp2013renters, rentEXP > 120)
            return(exp2013notKey)
        } else if (year == 2014) {
            exp2014notKey <<- subset(exp2014renters, rentEXP > 120)
            return(exp2014notKey)
        } else {
            return(0)
        }
    } else {
        ## class='owners' owners have imputed rent on property
        if (year == 2004) {
            return(subset(exp2004s, imputedRent > 0))
        } else if (year == 2005) {
            return(subset(exp2005s, imputedRent > 0))
        } else if (year == 2006) {
            return(subset(exp2006s, imputedRent > 0))
        } else if (year == 2007) {
            return(subset(exp2007s, imputedRent > 0))
        } else if (year == 2008) {
            return(subset(exp2008s, imputedRent > 0))
        } else if (year == 2009) {
            return(subset(exp2009s, imputedRent > 0))
        } else if (year == 2010) {
            return(subset(exp2010s, imputedRent > 0))
        } else if (year == 2011) {
            return(subset(exp2011s, imputedRent > 0))
        } else if (year == 2012) {
            return(subset(exp2012s, imputedRent > 0))
        } else if (year == 2013) {
            return(subset(exp2013s, imputedRent > 0))
        } else if (year == 2014) {
            return(subset(exp2014s, imputedRent > 0))
        } else {
            return(0)
        }
    }
}
#### old version #### based on >0 in rental expense## ownership <- function(class,
#### year){ someyears <- c(2004:2014) if(class=='renters'){
#### if(year==2004){return(subset(exp2004s, Q294>0)) }else
#### if(year==2005){return(subset(exp2005s, Q308>0)) }else
#### if(year==2006){return(subset(exp2006s, Q303>0)) }else
#### if(year==2007){return(subset(exp2007s, Q321>0)) }else
#### if(year==2008){return(subset(exp2008s, Q325>0)) }else
#### if(year==2009){return(subset(exp2009s, Q358>0)) }else
#### if(year==2010){return(subset(exp2010s, Q361>0)) }else
#### if(year==2011){return(subset(exp2012s, Q366>0)) }else
#### if(year==2012){return(subset(exp2012s, Q366>0)) }else
#### if(year==2013){return(subset(exp2013s, Q382>0)) }else
#### if(year==2014){return(subset(exp2014s, Q383>0)) }else{return(0)} }else{
#### ##class='owners' if(year==2004){return(subset(exp2004s, Q294==0)) }else
#### if(year==2005){return(subset(exp2005s, Q308==0)) }else
#### if(year==2006){return(subset(exp2006s, Q303==0)) }else
#### if(year==2007){return(subset(exp2007s, Q321==0)) }else
#### if(year==2008){return(subset(exp2008s, Q325==0)) }else
#### if(year==2009){return(subset(exp2009s, Q358==0)) }else
#### if(year==2010){return(subset(exp2010s, Q361==0)) }else
#### if(year==2011){return(subset(exp2012s, Q366==0)) }else
#### if(year==2012){return(subset(exp2012s, Q366==0)) }else
#### if(year==2013){return(subset(exp2013s, Q382==0)) }else
#### if(year==2014){return(subset(exp2014s, Q383==0)) }else{return(0)}}}

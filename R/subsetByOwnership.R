#' Creates dataframes expYYYYowners and expYYYYrenters,
#' calls weighHouseholds() ;   v. 3.65
# This function creates subsets of renters and owners. and key money
#' @param arg Defaults to true.
#' @keywords owners, renters, subset
#' @export
#' @examples
#' subsetByOwnership('listKeyMoney')
#' subsetByOwnership()
#' ## this should be called after loadData("versionD")
#' subsetByOwnership('versionD')
#' ## this was the previous version
#' subsetByOwnership('better')
#'
#' subsetByOwnership('listOwners')
#' subsetByOwnership('listNotKey')
#' subsetByOwnership('listHouseholds')
#' subsetByOwnership('listRenters')
#' subsetByOwnership('listKeyMoney')
#' subsetByOwnership(1)
#' subsetByOwnership(FALSE)

subsetByOwnership <- function(arg = TRUE) {
           if (arg == TRUE) {
        weighHouseholds("renters", "income")
        weighHouseholds("renters", "consumption")
        weighHouseholds("owners", "income")
        weighHouseholds("owners", "consumption")
        weighHouseholds("renters", "keymoney")

return("created subsets owners/renters for all years")
    } else if (arg == "listKeyMoney") {
        keyMoneyList <- list(exp2004keyMoney, exp2005keyMoney, exp2006keyMoney, exp2007keyMoney,
            exp2008keyMoney, exp2009keyMoney, exp2010keyMoney, exp2011keyMoney, exp2012keyMoney,
            exp2013keyMoney, exp2014keyMoney)
return(keyMoneyList)
    } else if (arg == "listNotKey") {
        notKeyList <- list(exp2004notKey, exp2005notKey, exp2006notKey, exp2007notKey,
            exp2008notKey, exp2009notKey, exp2010notKey, exp2011notKey, exp2012notKey,
            exp2013notKey, exp2014notKey)
return(notKeyList)
    } else if (arg == "listOwners") {
        ownersList <- list(exp2004owners, exp2005owners, exp2006owners, exp2007owners,
            exp2008owners, exp2009owners, exp2010owners, exp2011owners, exp2012owners,
            exp2013owners, exp2014owners)
return(ownersList)
    } else if (arg == "listRenters") {
        rentersList <- list(exp2004renters, exp2005renters, exp2006renters, exp2007renters,
            exp2008renters, exp2009renters, exp2010renters, exp2011renters, exp2012renters,
            exp2013renters, exp2014renters)
return(rentersList)
    } else if (arg == "listHouseholds") {
        householdsList <- list(exp2004s, exp2005s, exp2006s, exp2007s, exp2008s,
            exp2009s, exp2010s, exp2011s, exp2012s, exp2013s, exp2014s)
return(householdsList)
    } else if (arg == "versionD") {

renters2004  <- dplyr::filter( fam2004e,  RENT == "Yes")
renters2005  <- dplyr::filter( fam2005e,  RENT == "Yes")
renters2006  <- dplyr::filter( fam2006e,  RENT == "Yes")
renters2007  <- dplyr::filter( fam2007e,  RENT == "Yes")
renters2008  <- dplyr::filter( fam2008e,  RENT == "Yes")
renters2009  <- dplyr::filter( fam2009e,  RENT == "Yes")
renters2010  <- dplyr::filter( fam2010e,  RENT == "Yes")
renters2011  <- dplyr::filter( fam2011e,  RENT == "Yes")
renters2012  <- dplyr::filter( fam2012e,  RENT == "Yes")
renters2013  <- dplyr::filter( fam2013e,  RENT == "Yes")
renters2014  <- dplyr::filter( fam2014e,  RENT == "Yes")

owners2004  <- dplyr::filter( fam2004e,   OWNER == "Yes")
owners2005  <- dplyr::filter( fam2005e,   OWNER == "Yes")
owners2006  <- dplyr::filter( fam2006e,   OWNER == "Yes")
owners2007  <- dplyr::filter( fam2007e,   OWNER == "Yes")
owners2008  <- dplyr::filter( fam2008e,   OWNER == "Yes")
owners2009  <- dplyr::filter( fam2009e,   OWNER == "Yes")
owners2010  <- dplyr::filter( fam2010e,   OWNER == "Yes")
owners2011  <- dplyr::filter( fam2011e,   OWNER == "Yes")
owners2012  <- dplyr::filter( fam2012e,   OWNER == "Yes")
owners2013  <- dplyr::filter( fam2013e,   OWNER == "Yes")
owners2014  <- dplyr::filter( fam2014e,   OWNER == "Yes")

allfamilies <<-  bind_rows(list(fam2004e,
     fam2005e,
     fam2006e,
     fam2007e,
     fam2008e,
     fam2009e,
     fam2010e,
     fam2011e,
     fam2012e,
     fam2013e,
     fam2014e))

 allRenters <<-  bind_rows(list(renters2004,
     renters2005,
     renters2006,
     renters2007,
     renters2008,
     renters2009,
     renters2010,
     renters2011,
     renters2012,
     renters2013,
     renters2014))

 allOwners <<-  bind_rows(list(owners2004,
     owners2005,
     owners2006,
     owners2007,
     owners2008,
     owners2009,
     owners2010,
     owners2011,
     owners2012,
     owners2013,
     owners2014))

return("Merged allRenters and allOwners from versionD. Should be ready for regression.")
    } else if (arg == "better") {
renters2004  <- dplyr::filter( fam2004,  RENT == "Yes")
# head(renters2004)  #check
renters2005  <- dplyr::filter( fam2005,  RENT == "Yes")
renters2006  <- dplyr::filter( fam2006,  RENT == "Yes")
renters2007  <- dplyr::filter( fam2007,  RENT == "Yes")
renters2008  <- dplyr::filter( fam2008,  RENT == "Yes")
renters2009  <- dplyr::filter( fam2009,  RENT == "Yes")
renters2010  <- dplyr::filter( fam2010,  RENT == "Yes")
renters2011  <- dplyr::filter( fam2011,  RENT == "Yes")
renters2012  <- dplyr::filter( fam2012,  RENT == "Yes")
renters2013  <- dplyr::filter( fam2013, RENT == "Yes")
renters2014  <- dplyr::filter( fam2014, RENT == "Yes")

rentersList  <<- c(
renters2004,
renters2005,
renters2006,
renters2007,
renters2008,
renters2009,
renters2010,
renters2011,
renters2012,
renters2013,
renters2014)

owners2004  <- dplyr::filter( fam2004,   OWNER == "Yes")
# head(owners2004)  #check
owners2005  <- dplyr::filter( fam2005,   OWNER == "Yes")
owners2006  <- dplyr::filter( fam2006,   OWNER == "Yes")
owners2007  <- dplyr::filter( fam2007,   OWNER == "Yes")
owners2008  <- dplyr::filter( fam2008,   OWNER == "Yes")
owners2009  <- dplyr::filter( fam2009,   OWNER == "Yes")
owners2010  <- dplyr::filter( fam2010,   OWNER == "Yes")
owners2011  <- dplyr::filter( fam2011,   OWNER == "Yes")
owners2012  <- dplyr::filter( fam2012,   OWNER == "Yes")
owners2013  <- dplyr::filter( fam2013,  OWNER == "Yes")
owners2014  <- dplyr::filter( fam2014,  OWNER == "Yes")

ownersList <<- c(
owners2004,
owners2005,
owners2006,
owners2007,
owners2008,
owners2009,
owners2010,
owners2011,
owners2012,
owners2013,
owners2014)

    } else {
        return("not implemented, error: aorns9q4o")
    }
}

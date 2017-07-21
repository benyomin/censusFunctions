#' Creates dataframes expYYYYowners and expYYYYrenters,
#' calls weighHouseholds() ; 
#' This function creates subsets of renters and owners. and key money
#' @param arg Defaults to true.
#' @keywords owners, renters, subset
#' @export
#' @examples
#' subsetByOwnership('listKeyMoney')
#' subsetByOwnership()
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
    } else {
        return("not implemented, error: aorns9q4o")
    }
}

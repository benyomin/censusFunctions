#' combine all years into a single dataframe to facilitate regression.
#'
#' This function combines multiple years to a dataframe. v: 2.0
#' @param arg Sentence describing arg.
#' @keywords combine, dataframe, year
#' @export combineFrames
#' @family combine
#' @examples
#' combineFrames()
#' combineFrames("old")
#' combineFrames("versionC")
combineFrames <- function(arg = "old") {
if (arg == "old"){
    family2004a <- familiesList[[1]]
    family2005a <- familiesList[[2]]
    family2006a <- familiesList[[3]]
    family2007a <- familiesList[[4]]
    family2008a <- familiesList[[5]]
    family2009a <- familiesList[[6]]
    family2010a <- familiesList[[7]]
    family2011a <- familiesList[[8]]
    family2012a <- familiesList[[9]]
    family2013a <- familiesList[[10]]
    family2014a <- familiesList[[11]]
    ## all column names must be the same before a merge
    names(family2005a) <- names(family2004a)
    names(family2006a) <- names(family2004a)
    names(family2007a) <- names(family2004a)
    names(family2008a) <- names(family2004a)
    names(family2009a) <- names(family2004a)
    names(family2010a) <- names(family2004a)
    names(family2011a) <- names(family2004a)
    names(family2012a) <- names(family2004a)
    names(family2013a) <- names(family2004a)
    names(family2014a) <- names(family2004a)
    ## check
    names(family2011a) == names(family2005a)
    mergedData1 <<- do.call(rbind, list(family2004a, family2005a, family2006a, family2007a,
        family2008a, family2009a, family2010a, family2011a, family2012a, family2013a, family2014a))
    # str(family2004) ## 6k rows
    # str(mergedData1) ## 76k rows ## dataFrames combined successfully
    ## mergedData <- merge( family2004, family2005, family2006, family2007,
    ## family2008, family2009, family2010, family2011, family2012, family2013,
    ## family2014, all=T)
}else if(arg == "versionC"){
99

return("YYYYc frames merged.")}
}

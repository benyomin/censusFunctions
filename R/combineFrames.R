#' combine all years into a single dataframe to facilitate regression.
#'
#' This function combines multiple years to a dataframe.
#' @param arg Sentence describing arg.
#' #' @keywords combine, dataframe, year
#' @export
#' @examples
#' combineFrames(TRUE)
#' combineFrames(FALSE)
#' combineFrames()
combineFrames <- function(arg = TRUE) {
    
    family2004$stock
    head(family2005)
    names(family2005)
    str(family2005)  ## get number of rows
    
    family2004a <- family2004
    family2005a <- family2005
    family2006a <- family2006
    family2007a <- family2007
    family2008a <- family2008
    family2009a <- family2009
    family2010a <- family2010
    family2011a <- family2011
    family2012a <- family2012
    family2013a <- family2013
    family2014a <- family2014
    
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
    
    mergedData1 <<- do.call(rbind, list(family2004, family2005, family2006, family2007,
        family2008, family2009, family2010, family2011, family2012, family2013, family2014))
    
    # str(family2004) ## 6k rows
    
    # str(mergedData1) ## 76k rows ## dataFrames combined successfully
    
    ## mergedData <- merge( family2004, family2005, family2006, family2007,
    ## family2008, family2009, family2010, family2011, family2012, family2013,
    ## family2014, all=T)
}

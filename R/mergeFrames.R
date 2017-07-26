#' Merge all years (in familiesList) into a single dataframe to facilitate regression.
#'
#' This function combines multiple years to a dataframe. v: 3.0
#' @param arg Defaults to 1. 2 is not implemented.
#' @keywords combine, merge, regression, dataframe, year
#' @export mergeFrames
#' @family combine
#' @examples
#' mergeFrames(1)
#' mergeFrames(2)
#' mergeFrames()
mergeFrames <- function(arg = 1) {
  if(arg == 1){

    ## assertthat::see_if(assertthat::are_equal(familiesList[[1]] %>% names,
    ##           familiesList[[2]]  %>% names ))

    ## familiesList[[3]] %>% names
    ## familiesList[[4]]%>% names

    ## familiesList[[5]]%>% names

    ## familiesList[[6]]%>% names

    ## familiesList[[7]]%>% names

    ## familiesList[[8]]%>% names

    ## familiesList[[9]]%>% names

    ## familiesList[[10]]%>% names

    ## familiesList[[11]]%>% names

  mergedData2 <<-
    do.call(rbind, familiesList)

return("You are now ready to regress. DataFrame mergeData2 created.")

    }else if(arg == 2){

return("not yet implemented - placeholder 97a")

    ## all column names must be the same before a merge
    
    ## names(family2005a) <- names(family2004a)
    ## names(family2006a) <- names(family2004a)
    ## names(family2007a) <- names(family2004a)
    ## names(family2008a) <- names(family2004a)
    ## names(family2009a) <- names(family2004a)
    ## names(family2010a) <- names(family2004a)
    ## names(family2011a) <- names(family2004a)
    ## names(family2012a) <- names(family2004a)
    ## names(family2013a) <- names(family2004a)
    ## names(family2014a) <- names(family2004a)
    
    ## ## check
    ## names(family2011a) == names(family2005a)
    
    ## mergedData1 <<- do.call(rbind, list(family2004a, family2005a, family2006a, family2007a,
    ##     family2008a, family2009a, family2010a, family2011a, family2012a, family2013a, family2014a))
    
    # str(family2004) ## 6k rows
    
    # str(mergedData1) ## 76k rows ## dataFrames combined successfully
    
    ## mergedData <- merge( family2004, family2005, family2006, family2007,
    ## family2008, family2009, family2010, family2011, family2012, family2013,
    ## family2014, all=T)
    }else{
return("not implemented mergeFrames(returnCode 99x)")
    }
}

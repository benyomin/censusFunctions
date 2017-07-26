#' Merge all years (in familiesList) into a single dataframe to facilitate regression.
#'
#' This function combines multiple years to a dataframe. v: 3.0
#' @param arg Defaults to 1. 2 is not implemented.
#' @keywords combine, merge, regression, dataframe, year
#' @export mergeFrames
#' @family combine
#' @examples
#' mergeFrames("merge")
#' mergeFrames("writeOut")
#' mergeFrames()
mergeFrames <- function(arg = 3) {
  if(arg == "merge"){

mergedData2 <- plyr::ldply(familiesList, data.frame)
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

  ## mergedData2 <<-
  ##   do.call(rbind, familiesList)

return("You are now ready to regress. DataFrame mergeData2 created.")

    }else if(arg == "writeOut"){


dput(mergedData2, file = "../dataframes/mergedData2.txt")


return("wrote mergedData2 to file")


    }else if(arg == "saved"){

mergedData2 <<- dget("../dataframes/mergedData2.txt")

return("read saved version of mergedData2")
    }else{
return("not implemented mergeFrames(returnCode 99x)")
    }
}

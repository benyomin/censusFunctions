#' Merge all years (in familiesList) into a single dataframe to facilitate regression.
#'
#' This function combines multiple years to a dataframe. v: 3.4
#' @param arg Defaults to 1. 2 is not implemented.
#' @keywords combine, merge, regression, dataframe, year
#' @export mergeFrames
#' @family combine
#' @examples
#' mergeFrames("merge")
#' mergeFrames("manipulateAugVersion")
#' mergeFrames("writeOut")
#' mergeFrames("writeOutBetter")
#' mergeFrames("writeOutBetter2")
#' mergeFrames("writeOutAugVersion")
#' mergeFrames("saved")
#' mergeFrames("loadBetter")
#' mergeFrames("loadBetter2")
#' mergeFrames("renters")
#' mergeFrames("owners")
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
    }else if(arg == "renters"){

mergedRenters <<- plyr::ldply(rentersList, data.frame)

return("success renters")
    }else if(arg == "owners"){

mergedOwners  <<- plyr::ldply(ownersList, data.frame)

return("success owners")
    }else if(arg == "writeOutRenters"){

dput(mergedRenters, file = "../dataframes/mergedRenters.txt")

return("success writeOutRenters")
    }else if(arg == "writeOutOwners"){
#dput(ownersList,   file = "../dataframes/ownersList.txt")
dput(mergedOwners, file = "../dataframes/mergedOwners.txt")

return("success writeOutOwners")

    }else if(arg == "writeOut"){

dput(mergedData2, file = "../dataframes/mergedData2.txt")

return("wrote mergedData2 to file")

    }else if(arg == "manipulateAugVersion"){

mergeFrames("renters")
mergeFrames("owners")

return(" compleat - oay9rsdoe")

    }else if(arg == "loadBetter2"){
mergedRenters <<- readRDS("../savedData/mergedRenters4.rds")
mergedOwners  <<- readRDS("../savedData/mergedOwners4.rds")
return("loaded better version (2) of merged data frames.")

    }else if(arg == "writeOutBetter2"){
      saveRDS(mergedRenters, "../savedData/mergedRenters4.rds")
      saveRDS(mergedOwners,  "../savedData/mergedOwners4.rds")
return(" wrote Better Version (2) to savedData dir")

    }else if(arg == "loadBetter"){
mergedRenters <<- readRDS("../savedData/mergedRenters3.rds")
mergedOwners  <<- readRDS("../savedData/mergeOwners3.rds")
return("loaded better version of merged data frames.")

    }else if(arg == "writeOutBetter"){
      saveRDS(mergedRenters, "../savedData/mergedRenters3.rds")
      saveRDS(mergedOwners,  "../savedData/mergedOwners3.rds")
return(" wrote Better Version to savedData dir")

    }else if(arg == "writeOutAugVersion"){
      saveRDS(mergedRenters, "../savedData/mergedRenters.rds")
      saveRDS(mergedOwners,  "../savedData/mergedOwners.rds")
return(" wrote to savedData dir")

    }else if(arg == "saved"){

mergedData2     <<- dget("../dataframes/mergedData2.txt")
mergedData2[]   <<- lapply(mergedData2, unclass)

      mergedRenters   <<- dget("../dataframes/mergedRenters.txt")
      mergedRenters[] <<- lapply(mergedRenters, unclass)
#mergedRenters[] <<- lapply(mergedRenters, unclass)

mergedOwners    <<- dget("../dataframes/mergedOwners.txt")
mergedOwners[]  <<- lapply(mergedOwners, unclass)

return("read saved version of mergedData2")
    }else if(arg == "savedAug"){

Rent6 <<- readRDS("../savedData/mergedRenters.rds")
Own6  <<- readRDS("../savedData/mergedOwners.rds")

return("read saved version of August data")
    }else{
return("not implemented mergeFrames(returnCode 99x)")
    }
}

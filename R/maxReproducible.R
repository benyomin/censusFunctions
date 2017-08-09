#' Work with saved data or from raw census downloads?  v: 2
#' Defaults to saved. 1 for raw. 2 for saved. 3 to import more saved.  4. to write out what was made by two.
#' @param level 1..6, defaults to 5
#' @keywords import, saved, reproducibility
#' @export maxReproducible
#' @family setup
#' @examples
#' maxReproducible(1)
#' maxReproducible(2)
#' maxReproducible(3)
#' maxReproducible(4)
#' maxReproducible(5)
#' maxReproducible(6)
#' maxReproducible(7)
#' maxReproducible()
maxReproducible <- function(level = 5){
        if(level == 1){
    importFamilyData("raw")
    importFamilyData("writeOut")
return("added $size column to familyList[[1 to 13]]")
  }else if(level == 2){

    makeFamiliesList()

return("imported data from saved files")
  }else if(level == 3){
    ## replace with faster loading format
##     Sys.setenv(TZ="Europe/Berlin")
##     lubridate::now("Europe/Berlin")
## [1] "2017-08-04 13:39:23 CEST"
#      saveRDS(familiesList, "../savedData/XFI_correctSize.rds")

familiesList <<- readRDS("../savedData/XFI_correctSize.rds")
    #familiesList <<-   dget("../dataframes/XFI_correctSize.txt")
return("imported combined columns from XFI_correctSize")

  }else if(level == 4){
    ## this creates the data imported by maxReproducible(3)
    dput(familiesList, "../dataframes/XFI_correctSize.txt")
    mergeFrames("writeOutOwners")
    mergeFrames("writeOUtRenters")

return("saved the $size column onto familiesList")
  }else if(level == 5){
familiesList <<- readRDS("../savedData/XFI_correctSize.rds")
#    familiesList <<-   dget("../dataframes/XFI_correctSize.txt")
mergeFrames("saved")

return("imported mergedData2, \n mergedRenters, \n and
                     mergedOwners")
  }else if( level == 6){
    return("import of this data moved to preSetUp()")

      }else if(level == 7){
familiesList <<- readRDS("../savedData/XFI_correctSize.rds")

 subsetCities("make1")
 subsetCities("addregion")
 subsetCities("writeOutRegions")

return("imported familiesList, returned rentersList and ownersList.")

      }else if(level == 8){
        ## imports years
        ## adds $size
        ## adds $stock

      makeFamiliesList("new")
return("added $size and $stock")

      }else{
return("not yet implemented")
  }
}

#' Work with saved data or from raw census downloads?  v: 0.8
#' Defaults to saved. 1 for raw. 2 for saved. 3 to import more saved.  4. to write out what was made by two.
#' @keywords import, saved, reproducibility
#' @export maxReproducible
#' @examples
#' maxReproducible(1)
#' maxReproducible(2)
#' maxReproducible(3)
#' maxReproducible(4)
#' maxReproducible()
maxReproducible <- function(level = 2){
  if(level == 1){
  importFamilyData("raw")
  importFamilyData("writeOut")
return("added $size column to familyList[[1 to 13]]")
  }else if(level == 2){
#importData("familyProcessed")
#importData("savedIND")
#importData("saved")
#importData("importExpFamInd")
makeFamiliesList()
## needs to expose df family2014 as well

return("imported data from saved files")
  }else if(level == 3){


# familiesList <<- dget("../dataframes/XFI_stock_size_List.txt")
familiesList <<- dget("../dataframes/XFI_correctSize.txt")

return("imported combined columns o9on99lo")

  }else if(level ==4){
    ## this creates the data imported by maxReproducible(3)
dput(familiesList, "../dataframes/XFI_correctSize.txt")

  }else{
   return("the only options are 1:4")
  }
}

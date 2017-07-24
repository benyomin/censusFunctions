#' Work with saved data or from raw census downloads? Defaults to saved. 1 for raw. 2 for saved. Takes one argument.
#' @keywords import, saved, reproducibility
#' @export maxReproducible
#' @examples
#' importData(1)
#' importData(2)
#' importData()
maxReproducible <- function(level = 2){
  if(level == 1){
  importFamilyData("raw")
  importFamilyData("writeOut")
return("added $size column to familyList[[1 to 13]]")
  }else if(level == 2){
importData("familyProcessed")
importData("savedIND")
importData("saved")
## needs to expose df family2014 as well

return("imported data from saved files")
    }else{   ##"the only options are Y and N"
        return("the only options are 1 for 'raw' and 2 for 'saved'")
    }}


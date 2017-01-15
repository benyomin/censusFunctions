#' Import data to workspace
#'
#' This function imports Israeli census data.
#' @param source Source for data, "raw" imports from .por files. "saved" files were previously parsed in R.
#' @keywords import, saved, spss, por, raw 
#' @export
#' @examples
#' importData("raw")
#' importData("saved")
sampleFunction <- function(source){
    if(source=="raw"){
        source("../includes/importExpenditureRaw.R",  echo=FALSE)
        return("Imported Housing Expenditure Survey from Central Bureau of Statistics raw files.")
    }else if(source=="saved"){
        source("../includes/getProcessedData.R", echo=FALSE)
        return("Imported selected portions of Housing Expenditure Survey.")
    }else{"the only options are Y and N"
        return("the only options are 'raw' and 'saved'")
    }
}

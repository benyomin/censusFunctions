#' Import data to workspace
#'
#' This function imports Israeli census data.
#' @param source Source for data, "raw" imports from .por files. "saved" files were previously parsed in R.
#' @keywords import, saved, spss, por, raw, load
#' @export
#' @examples
#' importData("raw")
#' importData("saved")
#' importData("writeOut")
importData <- function(source){
    if(source=="raw"){
        source("../includes/importExpenditureRaw.R",  echo=FALSE)
        return("Imported Housing Expenditure Survey from Central Bureau of Statistics raw files.")
    }else if(source=="saved"){

      ##  source("../includes/getProcessedData.R", echo=FALSE)

exp2004s <-dget("../dataframes/exp2004s.txt")
exp2005s <-dget("../dataframes/exp2005s.txt")
exp2006s <-dget("../dataframes/exp2006s.txt")
exp2007s <-dget("../dataframes/exp2007s.txt")
exp2008s <-dget("../dataframes/exp2008s.txt")
exp2009s <-dget("../dataframes/exp2009s.txt")
exp2010s <-dget("../dataframes/exp2010s.txt")
exp2011s <-dget("../dataframes/exp2011s.txt")
exp2012s <-dget("../dataframes/exp2012s.txt")
exp2013s <-dget("../dataframes/exp2013s.txt")
exp2014s <-dget("../dataframes/exp2014s.txt")

      return("Imported selected portions of Housing Expenditure Survey. Returns exp2004-14s")
    }else if(source=="writeOut"){
########################
## write out new data ##
########################
dput(exp2004s, file="../dataframes/exp2004s.txt")
dput(exp2005s, file="../dataframes/exp2005s.txt")
dput(exp2006s, file="../dataframes/exp2006s.txt")
dput(exp2007s, file="../dataframes/exp2007s.txt")
dput(exp2008s, file="../dataframes/exp2008s.txt")
dput(exp2009s, file="../dataframes/exp2009s.txt")
dput(exp2010s, file="../dataframes/exp2010s.txt")
dput(exp2011s, file="../dataframes/exp2011s.txt")
dput(exp2012s, file="../dataframes/exp2012s.txt")
dput(exp2013s, file="../dataframes/exp2013s.txt")
dput(exp2014s, file="../dataframes/exp2014s.txt")
        return("wrote out dataframes")

    }else{   ##"the only options are Y and N"
        return("the only options are 'raw' and 'saved'")
    }}


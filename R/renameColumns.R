#' rename columns from raw data.
#'
#' This function imports Israeli census data.
#' @param arg Sentence describing arg.
#' #' @keywords rename, parse
#' @export
#' @examples
#' renameColumns(TRUE)
#' renameColumns(FALSE)
#' renameColumns()
renameColumns <- function(arg){
realList<-paste0("exp",as.character(2004:2014),"s")
## for loop:
for (df in realList){
    df.tmp <-get(df)
    names(df.tmp) <-c("grossIncome","rentalIncome","totalConsumption","rentEXP","imputedRent","WEIGHT","HHNUM")
    assign(df,df.tmp)
}
##
years<-c(2004:2014)
dfs<-list(exp2004s,exp2005s,exp2006s,exp2007s,
          exp2008s,exp2009s,exp2010s,exp2011s,
          exp2012s,exp2013s,exp2014s)
dfs<-Map(cbind,dfs,year=years)
exp2004s<-dfs[[1]]
exp2005s<-dfs[[2]]
exp2006s<-dfs[[3]]
exp2007s<-dfs[[4]]
exp2008s<-dfs[[5]]
exp2009s<-dfs[[6]]
exp2010s<-dfs[[7]]
exp2011s<-dfs[[8]]
exp2012s<-dfs[[9]]
exp2013s<-dfs[[10]]
exp2014s<-dfs[[11]]
}


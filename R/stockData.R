#' Import stock data from exchange
#'
#' This function imports mid-year prices from the tlv-125 index.
#' @param arg import- get it, add- add it as a column to the data frame, write- write the new dataframe
#' @keywords import, stocks, tlv-125, non-housing wealth, write
#' @export
#' @examples
#' stockData("import")
#' stockData("writeOutStock")
#' stockData("saved")
#' stockData("createSaved")
#' stockData("add")
stockData <- function(arg){
if(arg=="import"){
#import the new TLV stock data from the csv
tlv128<-readr::read_csv("../rawData/TLV-125midYearPrice.csv")
#View(tlv125)
tlv126<-tlv128[,c(1,4)] # select date and closing price columns
#View(tlv126)
tlv127<-tlv126[13:1,]  # remove extra rows
tlv125<-as.data.frame(tlv127)
rownames(tlv125)<-c(2004:2016)
#tlv128["2004",2] #"YYYY",2nd column is closing P. @ last day in June.
          return(tlv125)
}else  if(arg=="writeOutStock"){
       dput(wStockFamily2004, file="../dataframes/wStockFamily2004.txt")
       dput(wStockFamily2005, file="../dataframes/wStockFamily2005.txt")
       dput(wStockFamily2006, file="../dataframes/wStockFamily2006.txt")
       dput(wStockFamily2007, file="../dataframes/wStockFamily2007.txt")
       dput(wStockFamily2008, file="../dataframes/wStockFamily2008.txt")
       dput(wStockFamily2009, file="../dataframes/wStockFamily2009.txt")
       dput(wStockFamily2010, file="../dataframes/wStockFamily2010.txt")
       dput(wStockFamily2011, file="../dataframes/wStockFamily2011.txt")
       dput(wStockFamily2012, file="../dataframes/wStockFamily2012.txt")
       dput(wStockFamily2013, file="../dataframes/wStockFamily2013.txt")
       dput(wStockFamily2014, file="../dataframes/wStockFamily2014.txt")
          return("wrote out dataFrames with stock prices")
}else  if(arg=="add"){
  ## manipulates the frames created by
  ## importData("familyProcessed")
         wStockFamily2004<<-family2004
         wStockFamily2005<<-family2005
         wStockFamily2006<<-family2006
         wStockFamily2007<<-family2007
         wStockFamily2008<<-family2008
         wStockFamily2009<<-family2009
         wStockFamily2010<<-family2010
         wStockFamily2011<<-family2011
         wStockFamily2012<<-family2012
         wStockFamily2013<<-family2013
         wStockFamily2014<<-family2014
  
         wStockFamily2004$stock<<-tlv125["2004",2]
         wStockFamily2005$stock<<-tlv125["2005",2]
         wStockFamily2006$stock<<-tlv125["2006",2]
         wStockFamily2007$stock<<-tlv125["2007",2]
         wStockFamily2008$stock<<-tlv125["2008",2]
         wStockFamily2009$stock<<-tlv125["2009",2]
         wStockFamily2010$stock<<-tlv125["2010",2]
         wStockFamily2011$stock<<-tlv125["2011",2]
         wStockFamily2012$stock<<-tlv125["2012",2]
         wStockFamily2013$stock<<-tlv125["2013",2]
         wStockFamily2014$stock<<-tlv125["2014",2]
}else  if(arg=="saved"){
       wStockFamily2004<<- dget("../dataframes/wStockFamily2004.txt")
       wStockFamily2005<<- dget("../dataframes/wStockFamily2005.txt")
       wStockFamily2006<<- dget("../dataframes/wStockFamily2006.txt")
       wStockFamily2007<<- dget("../dataframes/wStockFamily2007.txt")
       wStockFamily2008<<- dget("../dataframes/wStockFamily2008.txt")
       wStockFamily2009<<- dget("../dataframes/wStockFamily2009.txt")
       wStockFamily2010<<- dget("../dataframes/wStockFamily2010.txt")
       wStockFamily2011<<- dget("../dataframes/wStockFamily2011.txt")
       wStockFamily2012<<- dget("../dataframes/wStockFamily2012.txt")
       wStockFamily2013<<- dget("../dataframes/wStockFamily2013.txt")
       wStockFamily2014<<- dget("../dataframes/wStockFamily2014.txt")
          return("imported saved data :: ;043q9ogwf")
}else if(arg=="createSaved"){
  ## censusFunctions::importData("familyProcessed")
## *  version: raw_data_import
## -- creates the usual saved version
## -- maximum reproducibility
## ** import raw stock prices
## ** add to data frames
## ** write out stock prices
  tlv125 <- censusFunctions::stockData("import")
  censusFunctions::stockData("add")
  censusFunctions::stockData("writeOutStock")
          return("added stock values and saved data wStockFamilyYYYY")
}else{   ##"the only options are Y and N"
          return("feature not implemented :: ;049035;")}}


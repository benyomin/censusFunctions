#' Import stock data from exchange v. 2.4
#'
#' This function imports mid-year prices from the tlv-125 index.
#' @param arg import- get it, add- add it as a column to the data frame, write- write the new dataframe
#' @family manipulate
#' @keywords import, stocks, tlv-125, non-housing wealth, write
#' @export
#' @examples
#' stockData('import')
#' stockData('writeOutStock')
#' stockData('saved')
#' stockData('createSaved')
#' stockData('add')
#' stockData('lagged')

stockData <- function(arg) {
    if (arg == "import") {
        # import the new TLV stock data from the csv
        tlv128 <- readr::read_csv("../rawData/TLV-125midYearPrice.csv")
        # View(tlv125)
        tlv126 <- tlv128[, c(1, 4)]  # select date and closing price columns
        # View(tlv126)
        tlv127 <- tlv126[13:1, ]  # remove extra rows
        tlv125 <- as.data.frame(tlv127)
        rownames(tlv125) <- c(2004:2016)
        # tlv128['2004',2] #'YYYY',2nd column is closing P. @ last day in June.
        return(tlv125)
    } else if (arg == "writeOutStock") {
        dput(wStockFamily2004, file = "../dataframes/wStockFamily2004.txt")
        dput(wStockFamily2005, file = "../dataframes/wStockFamily2005.txt")
        dput(wStockFamily2006, file = "../dataframes/wStockFamily2006.txt")
        dput(wStockFamily2007, file = "../dataframes/wStockFamily2007.txt")
        dput(wStockFamily2008, file = "../dataframes/wStockFamily2008.txt")
        dput(wStockFamily2009, file = "../dataframes/wStockFamily2009.txt")
        dput(wStockFamily2010, file = "../dataframes/wStockFamily2010.txt")
        dput(wStockFamily2011, file = "../dataframes/wStockFamily2011.txt")
        dput(wStockFamily2012, file = "../dataframes/wStockFamily2012.txt")
        dput(wStockFamily2013, file = "../dataframes/wStockFamily2013.txt")
        dput(wStockFamily2014, file = "../dataframes/wStockFamily2014.txt")
        return("wrote out dataFrames with stock prices")
    } else if (arg == "add") {
        wStockFamily2004 <<- family2004
        wStockFamily2005 <<- family2005
        wStockFamily2006 <<- family2006
        wStockFamily2007 <<- family2007
        wStockFamily2008 <<- family2008
        wStockFamily2009 <<- family2009
        wStockFamily2010 <<- family2010
        wStockFamily2011 <<- family2011
        wStockFamily2012 <<- family2012
        wStockFamily2013 <<- family2013
        wStockFamily2014 <<- family2014
        wStockFamily2004$stock <<- tlv125["2004", 2]
        wStockFamily2005$stock <<- tlv125["2005", 2]
        wStockFamily2006$stock <<- tlv125["2006", 2]
        wStockFamily2007$stock <<- tlv125["2007", 2]
        wStockFamily2008$stock <<- tlv125["2008", 2]
        wStockFamily2009$stock <<- tlv125["2009", 2]
        wStockFamily2010$stock <<- tlv125["2010", 2]
        wStockFamily2011$stock <<- tlv125["2011", 2]
        wStockFamily2012$stock <<- tlv125["2012", 2]
        wStockFamily2013$stock <<- tlv125["2013", 2]
        wStockFamily2014$stock <<- tlv125["2014", 2]
return("sucess: 8888gwf")
    } else if (arg == "saved") {
        wStockFamily2004 <<- dget("../dataframes/wStockFamily2004.txt")
        wStockFamily2005 <<- dget("../dataframes/wStockFamily2005.txt")
        wStockFamily2006 <<- dget("../dataframes/wStockFamily2006.txt")
        wStockFamily2007 <<- dget("../dataframes/wStockFamily2007.txt")
        wStockFamily2008 <<- dget("../dataframes/wStockFamily2008.txt")
        wStockFamily2009 <<- dget("../dataframes/wStockFamily2009.txt")
        wStockFamily2010 <<- dget("../dataframes/wStockFamily2010.txt")
        wStockFamily2011 <<- dget("../dataframes/wStockFamily2011.txt")
        wStockFamily2012 <<- dget("../dataframes/wStockFamily2012.txt")
        wStockFamily2013 <<- dget("../dataframes/wStockFamily2013.txt")
        wStockFamily2014 <<- dget("../dataframes/wStockFamily2014.txt")
return("imported saved data :: ;043q9ogwf")
    } else if (arg == "createSaved") {
        tlv125 <- censusFunctions::stockData("import")
        censusFunctions::stockData("add")
        censusFunctions::stockData("writeOutStock")
return("added stock values and saved data wStockFamilyYYYY")
    } else if (arg == "lagged") {

    ### import the new TLV stock data from the csv
    tlv129 <- readr::read_csv("../rawData/Data_20170824.csv")
    tlv131 <- tlv129[, c(1, 4)]  # select date and closing price columns
    ## tlv131[39,] ## 2003 mid-year
    ## Date `Closing Index Value`
    ##        <chr>                 <dbl>

    fam2004f$stocktminus1 <<- as.double(tlv131[39, 2])
    fam2004f$stocktminus2 <<- as.double(tlv131[284,2])
    fam2005f$stocktminus1 <<- fam2004f$stock[1]
    fam2006f$stocktminus1 <<- fam2005f$stock[1]
    fam2007f$stocktminus1 <<- fam2006f$stock[1]
    fam2008f$stocktminus1 <<- fam2007f$stock[1]
    fam2009f$stocktminus1 <<- fam2008f$stock[1]
    fam2010f$stocktminus1 <<- fam2009f$stock[1]
    fam2011f$stocktminus1 <<- fam2010f$stock[1]
    fam2012f$stocktminus1 <<- fam2011f$stock[1]
    fam2013f$stocktminus1 <<- fam2012f$stock[1]
    fam2014f$stocktminus1 <<- fam2013f$stock[1]

    fam2005f$stocktminus2 <<- as.double(tlv131[39, 2])
    fam2006f$stocktminus2 <<- fam2004f$stock[1]
    fam2007f$stocktminus2 <<- fam2005f$stock[1]
    fam2008f$stocktminus2 <<- fam2006f$stock[1]
    fam2009f$stocktminus2 <<- fam2007f$stock[1]
    fam2010f$stocktminus2 <<- fam2008f$stock[1]
    fam2011f$stocktminus2 <<- fam2009f$stock[1]
    fam2012f$stocktminus2 <<- fam2010f$stock[1]
    fam2013f$stocktminus2 <<- fam2011f$stock[1]
    fam2014f$stocktminus2 <<- fam2012f$stock[1]

## find difference between actual change in stock market and
## expected change given by previous one-period change

fam2004f$diffPrior  <<-(fam2004f$stocktminus1[1] - fam2004f$stocktminus2[1])/ fam2004f$stocktminus2[1]
fam2004f$diffNow    <<-(fam2004f$stock           - fam2004f$stocktminus1[1])/ fam2004f$stocktminus1[1]
fam2004f$Wshock_NH  <<- fam2004f$diffNow         - fam2004f$diffPrior
## diffNow - diffPrior == ErrorTerm ??
## head(fam2004f$Wshock_NH)
## 0.04639981   ## a positive 4.6% shock to non-housing wealth in 2004

fam2005f$diffPrior  <<-(fam2005f$stocktminus1[1] - fam2005f$stocktminus2[1])/ fam2005f$stocktminus2[1]
fam2005f$diffNow    <<-(fam2005f$stock           - fam2005f$stocktminus1[1])/ fam2005f$stocktminus1[1]
fam2005f$Wshock_NH  <<- fam2005f$diffNow         - fam2005f$diffPrior

fam2006f$diffPrior  <<-(fam2006f$stocktminus1[1] - fam2006f$stocktminus2[1])/ fam2006f$stocktminus2[1]
fam2006f$diffNow    <<-(fam2006f$stock           - fam2006f$stocktminus1[1])/ fam2006f$stocktminus1[1]
fam2006f$Wshock_NH  <<- fam2006f$diffNow         - fam2006f$diffPrior

fam2007f$diffPrior  <<-(fam2007f$stocktminus1[1] - fam2007f$stocktminus2[1])/ fam2007f$stocktminus2[1]
fam2007f$diffNow    <<-(fam2007f$stock           - fam2007f$stocktminus1[1])/ fam2007f$stocktminus1[1]
fam2007f$Wshock_NH  <<- fam2007f$diffNow         - fam2007f$diffPrior

fam2008f$diffPrior  <<-(fam2008f$stocktminus1[1] - fam2008f$stocktminus2[1])/ fam2008f$stocktminus2[1]
fam2008f$diffNow    <<-(fam2008f$stock           - fam2008f$stocktminus1[1])/ fam2008f$stocktminus1[1]
fam2008f$Wshock_NH  <<- fam2008f$diffNow         - fam2008f$diffPrior

fam2009f$diffPrior  <<-(fam2009f$stocktminus1[1] - fam2009f$stocktminus2[1])/ fam2009f$stocktminus2[1]
fam2009f$diffNow    <<-(fam2009f$stock           - fam2009f$stocktminus1[1])/ fam2009f$stocktminus1[1]
fam2009f$Wshock_NH  <<- fam2009f$diffNow         - fam2009f$diffPrior

fam2010f$diffPrior <<-(fam2010f$stocktminus1[1] - fam2010f$stocktminus2[1])/ fam2010f$stocktminus2[1]
fam2010f$diffNow   <<-(fam2010f$stock           - fam2010f$stocktminus1[1])/ fam2010f$stocktminus1[1]
fam2010f$Wshock_NH <<- fam2010f$diffNow         - fam2010f$diffPrior

fam2011f$diffPrior <<-(fam2011f$stocktminus1[1] - fam2011f$stocktminus2[1])/ fam2011f$stocktminus2[1]
fam2011f$diffNow   <<-(fam2011f$stock           - fam2011f$stocktminus1[1])/ fam2011f$stocktminus1[1]
fam2011f$Wshock_NH <<- fam2011f$diffNow         - fam2011f$diffPrior

fam2012f$diffPrior <<-(fam2012f$stocktminus1[1] - fam2012f$stocktminus2[1])/ fam2012f$stocktminus2[1]
fam2012f$diffNow   <<-(fam2012f$stock           - fam2012f$stocktminus1[1])/ fam2012f$stocktminus1[1]
fam2012f$Wshock_NH <<- fam2012f$diffNow         - fam2012f$diffPrior

fam2013f$diffPrior <<-(fam2013f$stocktminus1[1] - fam2013f$stocktminus2[1])/ fam2013f$stocktminus2[1]
fam2013f$diffNow   <<-(fam2013f$stock           - fam2013f$stocktminus1[1])/ fam2013f$stocktminus1[1]
fam2013f$Wshock_NH <<- fam2013f$diffNow         - fam2013f$diffPrior

fam2014f$diffPrior <<-(fam2014f$stocktminus1[1] - fam2014f$stocktminus2[1])/ fam2014f$stocktminus2[1]
fam2014f$diffNow   <<-(fam2014f$stock           - fam2014f$stocktminus1[1])/ fam2014f$stocktminus1[1]
fam2014f$Wshock_NH <<- fam2014f$diffNow         - fam2014f$diffPrior

return("added lagged stock value columns and YYYYf")
    } else {
        return("feature not implemented :: ;049035;")}}

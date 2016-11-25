#' Downloads quarterly housing prices for the years
#' 2006-2016 to the current working directory as 5 excel files
#' @param year defaults to true
#' @keywords housing, cpi, price, download, fetch, CBS, dataset
#' @export
#' @examples
#' downloadHousingPrices(1)        #downloads Excel files from CBS servers
#' downloadHousingPrices(FALSE)    #displays a message
#' downloadHousingPrices(3)       #downloads a test file
#' downloadHousingPrices()         #downloads a test file
## define function
downloadHousingPrices <- function(key=3){
  ## set variables
     ## test file
        test.url ="http://www-eng-x.llnl.gov/documents/a_image.gif"
     ## list of files on CBS site
        ## 2014,15,16q1-q3
        yr14_16.url ="http://www.cbs.gov.il/www/price_new/a6_2_e.xls"
        ## 2013,12
        yr12_13.url="http://www.cbs.gov.il/www/archive/201403/price_new/a6_2_e.xls"
        ## 2011,10
        yr10_11.url="http://www.cbs.gov.il/www/archive/201203/price_new/a6_2_e.xls"
        ## 2009,08
        yr08_09.url="http://www.cbs.gov.il/www/archive/201003/price_new/a6_2_e.xls"
        ## 2007,06
        yr06_07.url="http://www.cbs.gov.il/www/archive/200803/price_new/a6_2_e.xls"
        ## 05,04
        ## no quarterly data publicly available for this year.
        ## http://www.cbs.gov.il/www/archive/200603/price/t16_3_e.xls
     ## change useragent (curl is blocked to prevent scraping of data from the website)
        options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64)")
  if(key==1){
    print("Downloading Housing Prices 2006-2016 from the Central Bureau of Statistics.")
        houseP14_16.xls = getURLContent(yr14_16.url, verbose=TRUE, useragent=getOption("HTTPUserAgent"))
        houseP12_13.xls = getURLContent(yr12_13.url, useragent=getOption("HTTPUserAgent"))
        houseP10_11.xls = getURLContent(yr10_11.url, useragent=getOption("HTTPUserAgent"))
        houseP09_08.xls = getURLContent(yr08_09.url, useragent=getOption("HTTPUserAgent"))
        houseP07_06.xls = getURLContent(yr06_07.url, useragent=getOption("HTTPUserAgent"))
    }
    else if(key==3)
     {
       print("Downloading test file logo.gif to your working directory.")
       f = CFILE("logo.gif", mode="wb")
       curlPerform(url=test.url, verbose=TRUE, useragent=getOption("HTTPUserAgent"), writedata=f@ref)
       close(f) 
    }
    else {
        print("Census data for those years not yet implemented.")
    }
}
# assumes:
#source("stats.r")
#import data
#source("importCensus.r")
# #how many columns are there? = 74
# ncol(file456)
# #age
# summary(file456$AGE)
# #age is grouped by 0-4 years old, 5-9, 10-14, etc. until 85+
# hist(file456$AGE)
# #subset a few columns
# subSetInd2014<-dplyr::select(file456,YEARSUR,WPLDIST,USSRBORN,INCSAL)
# ##subset to only 3 cities, easier to copy DFW boxplot code
# subCitiesInd2014 <- subSetInd2014 %>% dplyr::filter(WPLDIST=="Center"    |
#                                                         WPLDIST=="Tel-Aviv"  |
#                                                         WPLDIST=="Gush Dan")

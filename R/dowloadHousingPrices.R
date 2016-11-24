#' Downloads quarterly housing prices for the years
#' 2006-2016 to the current working directory as 5 excel files
#' @param year defaults to true
#' @keywords housing, cpi, price, download, fetch, CBS, dataset
#' @export
#' @examples
#' downloadHousingPrices()
downloadHousingPrices <- function(year=TRUE){
    if(year){
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
        ## download the excel files
        options(HTTPUserAgent = "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10.6;
                en-US; rv:1.9.2.12) Gecko/20101026 Firefox/3.6.12")
        ## download.file(yr14_16.url, "houseP14_16.xls")

        houseP14_16.xls = getURLContent(yr14_16.url, useragent=getOption("HTTPUserAgent"))
        houseP12_13.xls = getURLContent(yr12_13.url, useragent=getOption("HTTPUserAgent"))
        houseP10_11.xls = getURLContent(yr10_11.url, useragent=getOption("HTTPUserAgent"))
        houseP09_08.xls = getURLContent(yr08_09.url, useragent=getOption("HTTPUserAgent"))
        houseP07_06.xls = getURLContent(yr06_07.url, useragent=getOption("HTTPUserAgent"))
    }
    else if(year==1977){
        print("The year is 1977")
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

#' Downloads quarterly housing prices for the years
#' 2006-2016 to the current working directory as 5 excel files
#' @param year defaults to true
#' @keywords housing, cpi, price, download, fetch, CBS, dataset
#' @export
#' @examples
#' downloadHousingPrices(13)     
#' downloadHousingPrices(FALSE)     
#' downloadHousingPrices(3)       
#' downloadHousingPrices()      

downloadHousingPrices <- function(key=3){
  ## set variables
     ## test file
        test.url   ="http://www-eng-x.llnl.gov/documents/a_image.gif"
     ## list of files on CBS site
        ## 2014q3-q4,15,16q1-q3
        yr14_16.url="http://www.cbs.gov.il/www/price_new/a6_2_e.xls"
        ## 2014q1-q2
        yr14.url   ="http://www.cbs.gov.il/www/archive/201503/price_new/a6_2_e.xls"
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
    ## get data 2014q1 to 2016 q3
    f=CFILE("houseP14_16.xls",mode = "wb") ##creates a file in which to write downloaded data.
    curlPerform(url=yr14_16.url, verbose=TRUE, useragent=getOption("HTTPUserAgent"), writedata=f@ref)
    RCurl::close(f)
    ## get data 2012q1-2013q4
    f=CFILE("houseP12_13.xls", mode="wb")
    curlPerform(url=yr12_13.url, useragent=getOption("HTTPUserAgent"),writedata=f@ref)
    RCurl::close(f)
    ## get data 2010q1-2011q4
    f=CFILE("houseP10_11.xls", mode ="wb")
    curlPerform(url=yr10_11.url, useragent=getOption("HTTPUserAgent"), writedata=f@ref)
    RCurl::close(f)
    ## get data 2008q1-2009q4
    f=CFILE("houseP09_08.xls", mode ="wb")
    curlPerform(url=yr08_09.url, useragent=getOption("HTTPUserAgent"), writedata=f@ref)
    RCurl::close(f)
    ## get data 2006q1-2007q4
    f=CFILE("houseP06_07.xls", mode ="wb")
    curlPerform(url=yr06_07.url, useragent=getOption("HTTPUserAgent"), writedata=f@ref)
    RCurl::close(f)
  }
     else if(key==16)
  {if(file.exists("houseP14_16.xls")){print("2014-16 already downloaded")}
      else{print("downloading 2014-16")
         ## get data 2014q1 to 2016 q3
    g=CFILE("houseP14_16.xls",mode = "wb") ##creates a file in which to write downloaded data.
    curlPerform(url=yr14_16.url, verbose=TRUE, useragent=getOption("HTTPUserAgent"), writedata=g@ref)
    RCurl::close(g)
  }}
  else if(key==14)
     {if(file.exists("houseP14q1-2.xls")){print("2014q1-q2 already downloaded")}
      else{print("downloading q1,q2 of 2014")
       ## get data 2012q1-2013q4
    z=CFILE("houseP14q1-2.xls", mode="wb")
    curlPerform(url=yr14.url, useragent=getOption("HTTPUserAgent"),writedata=z@ref)
    RCurl::close(z)
     }}
  else if(key==13)
     {if(file.exists("houseP12_13.xls")){print("2012-13 already downloaded")}
      else{print("downloading 2012-13")
       ## get data 2012q1-2013q4
    h=CFILE("houseP12_13.xls", mode="wb")
    curlPerform(url=yr12_13.url, useragent=getOption("HTTPUserAgent"),writedata=h@ref)
    RCurl::close(h)
     }}
     else if(key==11)
     {if(file.exists("houseP10_11.xls")){print("2010-11 already downloaded")}
      else{print("downloading 2010-11")
## get data 2010q1-2011q4
    i=CFILE("houseP10_11.xls", mode ="wb")
    curlPerform(url=yr10_11.url, useragent=getOption("HTTPUserAgent"), writedata=i@ref)
    RCurl::close(i)
     }}
     else if(key==9)
     {if(file.exists("houseP08_09.xls")){print("2008-09 already downloaded")}
      else{print("downloading 2008-09")
## get data 2008q1-2009q4
    j=CFILE("houseP08_09.xls", mode ="wb")
    curlPerform(url=yr08_09.url, useragent=getOption("HTTPUserAgent"), writedata=j@ref)
    RCurl::close(j)
     }}
     else if(key==7)
     {if(file.exists("houseP06_07.xls")){print("2006-07 already downloaded")}
      else{print("downloading 2006-07")
    k=CFILE("houseP06_07.xls", mode ="wb")
    curlPerform(url=yr06_07.url, useragent=getOption("HTTPUserAgent"), writedata=k@ref)
    RCurl::close(k)
     }}
     else if(key==3)
     {
       print("Downloading test file logo.gif to your working directory.")
       f = CFILE("logo.gif", mode="wb")
       curlPerform(url=test.url, verbose=TRUE, useragent=getOption("HTTPUserAgent"), writedata=f@ref)
       RCurl::close(f) 
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

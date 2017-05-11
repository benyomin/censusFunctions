#' combine housing prices from multiple excel files
#'
#' This function imports Israeli census data.
#' @param arg1 What am I creating? Defaults to saved.
#' @keywords process, aggregate, data
#' @export
#' @examples
#' combineHousingPrices("saved")
#' combineHousingPrices("excel")
#' combineHousingPrices("raw")
#' combineHousingPrices("writeout")
combineHousingFunctions <- function(arg1) {
  require(zoo)
  require(readxl)
  if (arg1 == "excel") {
## prepare column names
dates <-yearqtr(2006+seq(0,43)/4)
## dates
locations <- c("Total", "Jerusalem", "Tel Aviv", "Haifa", "Gush Dan", "Center and Jerusalem Periphery towns", "South", "Sharon",
               "North", "Qrayot Haifa")
rooms<-c("average","1.5-2 rooms","2.5-3 rooms", "3.5-4 rooms","4.5-5 rooms")
repLocations<-rep(locations, each=5)
rows <-paste(repLocations, rooms)
## read prices from excel
## location of prices on gov. site:  TODO

P06_07<-read_excel("../rawData/houseP06_07.xls",  range = "A6:K56")
P06_07   <- P06_07[,-7]   ##remove annual average
## label columns
colNames06_07 <- c("id","Total",dates[01:08])
colnames(P06_07) <- colNames06_07
#View(P06_07)
#colNames06_07 <- c("id","Total",dates[01:08])
#colNames06_07

## end q4 2007
## begin q1 2008
P08_09<-read_excel("../rawData/houseP08_09.xls",  range = "A6:K56")
P08_09 <- P08_09[,-7]   ##remove annual average
colNames08_09 <- c("id","Total",dates[09:16])
colnames(P08_09)<-colNames08_09
# View(P08_09)

## end q4 2009
## begin q1 2010
P10_11<-read_excel("../rawData/houseP10_11.xls",  range = "A6:K56")
P10_11 <- P10_11[,-7]   ##remove annual average
#View(P10_11)
colNames10_11 <- c("id","Total",dates[17:24])
colnames(P10_11)<-colNames10_11

## end q4 2011
## begin q1 2012
P12_13<-read_excel("../rawData/houseP12_13.xls",  range = "A6:K56")
P12_13<-P12_13[,-7]
colNames12_13 <- c("id","Total",dates[25:32])  ##error
colnames(P12_13)<-colNames12_13
#View(P12_13)

## end q4 2013
## begin q1 2014
P14   <-read_excel("../rawData/houseP14q1-2.xls", range = "A6:K56")
P14   <-P14[,-7]   ##remove annual average
colNames14 <- c("id","Total",dates[29:36])
colnames(P14)<-colNames14
#View(P14)

## end q4 2014
## begin q1 2015
P15_16<-read_excel("../rawData/houseP14_16.xls",  range = "A6:L56")
P15_16   <-P15_16[,-9]
colNames15 <- c("id","Total",dates[35:43])
colnames(P15_16)<-colNames15
P15_16 <- P15_16[, -c(3,4)]

## remove id columns except before 2006
p06_07<-P06_07
p08_09<-P08_09[,-c(1,2)]
p10_11<-P10_11[,-c(1,2)]
p12_13<-P12_13[,-c(1,2)]
p14<-   P14[,-c(1,2)]
p15_16<-P15_16[,-c(1,2)]
prices<-cbind(p06_07,p08_09,p10_11,p12_13,p14,p15_16)
homeprices <- prices
rownames(homeprices) <- rows
homeprices

  print("combing home prices from excel sheets.")
    return(homeprices)
} else if (arg1 == "saved") {
  print("loading home prices from a saved R data file")
    return(homeprices)
} else if (arg1 == "raw") {
  chngDir("index")
  setwd("..")
  getwd()
        return("downloading excel files")
    } else {

        return("writing excel files")
    }
}

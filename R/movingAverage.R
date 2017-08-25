#' Add Moving Average Column to YYYY. v. 1.3
#'
#' This function creates a lagged homeP column.
#' @param arg The year that data will be returned for.
#' @family mutate
#' @keywords average, houseP
#' @export
#' @examples
#' movingAverage()
#' requires:
#' 1: censusFunctions::importData("versionF")
#' 2: load("../savedData/homeprices.Rda")
#' 3: source("../includes/regionalPrices.R")
#' movingAverage("load")
#' adds the tminus1 and tminus2 columns for homeP
#' movingAverage("whshock")
#' calculates the error or unexpected shock difference
#' between (homeP - tminus1) and (tminus1 - tminus2)
movingAverage <- function(arg = "load") {
    if (arg == "load") {

prePrices <- readxl::read_excel("../rawData/allHouseP.xls", range = "B93:I712")

 # str(prePrices)
## need to calculate a val for 2002 avg., only quarterly prices are given
names(prePrices) <-  c("rooms", "currency", "yr", "annual", "q1", "q2", "q3", "q4")

prePrices$annual <- as.double(prePrices$annual)
prePrices$q1     <- as.double(prePrices$q1)
prePrices$q2     <- as.double(prePrices$q2)
prePrices$q3     <- as.double(prePrices$q3)
prePrices$q4     <- as.double(prePrices$q4)
## Jerusalem 1.5 2001 q1 price
## prePrices[1,5]

## prePrices[1,]$q1
## prePrices[1,]$q2
## prePrices[1,]$q3
## prePrices[1,]$q4

rowsToAdd <- 111-94

JeruTiny2001 <- mean(prePrices[1,]$q1 +
                      prePrices[1,]$q2 +
                      prePrices[1,]$q3 +
                      prePrices[1,]$q4 )

JeruTiny2002 <- mean( prePrices[2,]$q1 +
                      prePrices[2,]$q2 +
                      prePrices[2,]$q3 +
                      prePrices[2,]$q4 )
JeruTiny2003 <- prePrices[3,]$annual

JeruSmall2001 <- mean(prePrices[(1+rowsToAdd),]$q1 +
                      prePrices[(1+rowsToAdd),]$q2 +
                      prePrices[(1+rowsToAdd),]$q3 +
                      prePrices[(1+rowsToAdd),]$q4 )

JeruSmall2002 <- mean(prePrices[(2+rowsToAdd),]$q1 +
                      prePrices[(2+rowsToAdd),]$q2 +
                      prePrices[(2+rowsToAdd),]$q3 +
                      prePrices[(2+rowsToAdd),]$q4 )
JeruSmall2003 <- prePrices[3+rowsToAdd,]$annual
JeruMed2001 <- mean(  prePrices[(1+ rowsToAdd + rowsToAdd),]$q1 +
                      prePrices[(1+ rowsToAdd + rowsToAdd),]$q2 +
                      prePrices[(1+ rowsToAdd + rowsToAdd),]$q3 +
                      prePrices[(1+ rowsToAdd + rowsToAdd),]$q4 )

JeruMed2002 <- mean(  prePrices[(2+ rowsToAdd + rowsToAdd),]$q1 +
                      prePrices[(2+ rowsToAdd + rowsToAdd),]$q2 +
                      prePrices[(2+ rowsToAdd + rowsToAdd),]$q3 +
                      prePrices[(2+ rowsToAdd + rowsToAdd),]$q4 )
JeruMed2003 <-        prePrices[3+  rowsToAdd + rowsToAdd,]$annual
JeruLarge2001 <- mean( prePrices[36, ]$q1 +
                       prePrices[36, ]$q1 +
                       prePrices[36, ]$q1 +
                       prePrices[36, ]$q1)

JeruLarge2002 <- mean( prePrices[37, ]$q1 +
                       prePrices[37, ]$q2 +
                       prePrices[37, ]$q3 +
                       prePrices[37, ]$q4)
JeruLarge2003 <-      prePrices[ 38, ]$annual
#####################    TLV

TLVTiny2003  <- prePrices[71 ,]$annual
TLVSmall2003 <- prePrices[89 ,]$annual
TLVMed2003   <- prePrices[107,]$annual
TLVLarge2003 <- prePrices[125,]$annual

TLVTiny2001 <- mean(prePrices[69,]$q1 +
                     prePrices[69,]$q2 +
                     prePrices[69,]$q3 +
                     prePrices[69,]$q4 )

TLVTiny2002 <- mean(prePrices[70,]$q1 +
                     prePrices[70,]$q2 +
                     prePrices[70,]$q3 +
                     prePrices[70,]$q4 )

TLVSmall2001 <- mean(prePrices[87,]$q1 +
                     prePrices[87,]$q2 +
                     prePrices[87,]$q3 +
                     prePrices[87,]$q4 )

TLVSmall2002 <- mean(prePrices[88,]$q1 +
                     prePrices[88,]$q2 +
                     prePrices[88,]$q3 +
                     prePrices[88,]$q4 )

TLVMed2001 <- mean(prePrices[105,]$q1 +
                     prePrices[105,]$q2 +
                     prePrices[105,]$q3 +
                     prePrices[105,]$q4 )

TLVMed2002 <- mean(prePrices[106,]$q1 +
                     prePrices[106,]$q2 +
                     prePrices[106,]$q3 +
                     prePrices[106,]$q4 )


TLVLarge2001 <- mean(prePrices[123,]$q1 +
                     prePrices[123,]$q2 +
                     prePrices[123,]$q3 +
                     prePrices[123,]$q4 )

TLVLarge2002 <- mean(prePrices[124,]$q1 +
                     prePrices[124,]$q2 +
                     prePrices[124,]$q3 +
                     prePrices[124,]$q4 )

# prePrices[123+51+19,]$q1
#########################3   Haifa ####################3

HaifaTiny2003  <- prePrices[140,]$annual
HaifaSmall2003 <- prePrices[88+52,]$annual
HaifaMed2003   <- prePrices[158,]$annual
HaifaLarge2003 <- prePrices[176,]$annual


HaifaTiny2001 <- mean(prePrices[138,]$q1 +
                     prePrices[138,]$q2 +
                     prePrices[138,]$q3 +
                     prePrices[138,]$q4 )

HaifaTiny2002 <- mean(prePrices[139,]$q1 +
                     prePrices[139,]$q2 +
                     prePrices[139,]$q3 +
                     prePrices[139,]$q4 )

HaifaSmall2001 <- mean(prePrices[87+51,]$q1 +
                     prePrices[87+51,]$q2 +
                     prePrices[87+51,]$q3 +
                     prePrices[87+51,]$q4 )

HaifaSmall2002 <- mean(prePrices[88+51, ]$q1 +
                     prePrices[88+51, ]$q2 +
                     prePrices[88+51, ]$q3 +
                     prePrices[88+51, ]$q4 )

HaifaMed2001 <- mean(prePrices[105+51, ]$q1 +
                     prePrices[105+51, ]$q2 +
                     prePrices[105+51, ]$q3 +
                     prePrices[105+51, ]$q4 )

HaifaMed2002 <- mean(prePrices[106+51, ]$q1 +
                     prePrices[106+51, ]$q2 +
                     prePrices[106+51, ]$q3 +
                     prePrices[106+51, ]$q4 )

HaifaLarge2001 <- mean(prePrices[123+51, ]$q1 +
                     prePrices[123+51, ]$q2 +
                     prePrices[123+51, ]$q3 +
                     prePrices[123+51, ]$q4 )

HaifaLarge2002 <- mean(prePrices[124+51, ]$q1 +
                     prePrices[124+51, ]$q2 +
                     prePrices[124+51, ]$q3 +
                     prePrices[124+51, ]$q4 )
#################  Gush Dan ################33

GushDanTiny2003  <- prePrices[209,]$annual
GushDanSmall2003 <- prePrices[227,]$annual
GushDanMed2003   <- prePrices[245,]$annual
GushDanLarge2003 <- prePrices[245+18,]$annual


GushDanTiny2001 <- mean(prePrices[207,]$q1 +
                        prePrices[207,]$q2 +
                        prePrices[207,]$q3 +
                        prePrices[207,]$q4 )

GushDanTiny2002 <- mean(prePrices[208, ]$q1 +
                        prePrices[208, ]$q2 +
                        prePrices[208, ]$q3 +
                        prePrices[208, ]$q4 )

GushDanSmall2001 <- mean(prePrices[225, ]$q1 +
                         prePrices[225, ]$q2 +
                         prePrices[225, ]$q3 +
                         prePrices[225, ]$q4 )

GushDanSmall2002 <- mean(prePrices[226, ]$q1 +
                         prePrices[226, ]$q2 +
                         prePrices[226, ]$q3 +
                         prePrices[226, ]$q4 )

GushDanMed2001 <- mean(prePrices[243,]$q1 +
                       prePrices[243,]$q2 +
                       prePrices[243,]$q3 +
                       prePrices[243,]$q4 )

GushDanMed2002   <- mean(prePrices[244,]$q1 +
                         prePrices[244,]$q2 +
                         prePrices[244,]$q3 +
                         prePrices[244,]$q4 )

GushDanLarge2001 <- mean(prePrices[243+18, ]$q1 +
                         prePrices[243+18, ]$q2 +
                         prePrices[243+18, ]$q3 +
                         prePrices[243+18, ]$q4 )

GushDanLarge2002 <- mean(prePrices[243+19, ]$q1 +
                         prePrices[243+19, ]$q2 +
                         prePrices[243+19, ]$q3 +
                         prePrices[243+19, ]$q4 )
#################    Center ################33

CenterTiny2003  <- prePrices[278,]$annual
CenterSmall2003 <- prePrices[296,]$annual
CenterMed2003   <- prePrices[314,]$annual
CenterLarge2003 <- prePrices[312+20,]$annual


 CenterTiny2001 <- mean(prePrices[276,]$q1 +
                        prePrices[276,]$q2 +
                        prePrices[276,]$q3 +
                        prePrices[276,]$q4 )

 CenterTiny2002 <- mean(prePrices[276+1, ]$q1 +
                        prePrices[276+1, ]$q2 +
                        prePrices[276+1, ]$q3 +
                        prePrices[276+1, ]$q4 )

 CenterSmall2001 <- mean(prePrices[294, ]$q1 +
                         prePrices[294, ]$q2 +
                         prePrices[294, ]$q3 +
                         prePrices[294, ]$q4 )

 CenterSmall2002 <- mean(prePrices[295, ]$q1 +
                         prePrices[295, ]$q2 +
                         prePrices[295, ]$q3 +
                         prePrices[295, ]$q4 )

 CenterMed2001 <- mean(prePrices[312,]$q1 +
                       prePrices[312,]$q2 +
                       prePrices[312,]$q3 +
                       prePrices[312,]$q4 )

 CenterMed2002   <- mean(prePrices[313,]$q1 +
                         prePrices[313,]$q2 +
                         prePrices[313,]$q3 +
                         prePrices[313,]$q4 )

 CenterLarge2001 <- mean(prePrices[312+18, ]$q1 +
                         prePrices[312+18, ]$q2 +
                         prePrices[312+18, ]$q3 +
                         prePrices[312+18, ]$q4 )

 CenterLarge2002 <- mean(prePrices[312+19, ]$q1 +
                         prePrices[312+19, ]$q2 +
                         prePrices[312+19, ]$q3 +
                         prePrices[312+19, ]$q4 )

#################    South ################33

SouthTiny2003  <- prePrices[347,]$annual
SouthSmall2003 <- prePrices[347+18,]$annual
SouthMed2003   <- prePrices[383,]$annual
SouthLarge2003 <- prePrices[401,]$annual


 SouthTiny2001 <- mean( prePrices[345,]$q1 +
                        prePrices[345,]$q2 +
                        prePrices[345,]$q3 +
                        prePrices[345,]$q4 )

 SouthTiny2002 <- mean(prePrices[346, ]$q1 +
                        prePrices[346, ]$q2 +
                        prePrices[346, ]$q3 +
                        prePrices[346, ]$q4 )

 SouthSmall2001 <- mean(prePrices[345 + 18, ]$q1 +
                         prePrices[345 + 18, ]$q2 +
                         prePrices[345 + 18, ]$q3 +
                         prePrices[345 + 18, ]$q4 )

 SouthSmall2002 <- mean(prePrices[345+19, ]$q1 +
                         prePrices[345+19, ]$q2 +
                         prePrices[345+19, ]$q3 +
                         prePrices[345+19, ]$q4 )

 SouthMed2001 <- mean(prePrices[345+36,]$q1 +
                       prePrices[345+36,]$q2 +
                       prePrices[345+36,]$q3 +
                       prePrices[345+36,]$q4 )

 SouthMed2002   <-  mean(prePrices[345+38,]$q1 +
                         prePrices[345+38,]$q2 +
                         prePrices[345+38,]$q3 +
                         prePrices[345+38,]$q4 )

 SouthLarge2001 <- mean(prePrices[399, ]$q1 +
                         prePrices[399, ]$q2 +
                         prePrices[399, ]$q3 +
                         prePrices[399, ]$q4 )

 SouthLarge2002 <- mean(prePrices[400, ]$q1 +
                         prePrices[400, ]$q2 +
                         prePrices[400, ]$q3 +
                         prePrices[400, ]$q4 )

#################    Sharon ################33

SharonTiny2003  <- prePrices[416,]$annual
SharonSmall2003 <- prePrices[434,]$annual
SharonMed2003   <- prePrices[452,]$annual
SharonLarge2003 <- prePrices[470,]$annual


 SharonTiny2001 <- mean(prePrices[414,]$q1 +
                        prePrices[414,]$q2 +
                        prePrices[414,]$q3 +
                        prePrices[414,]$q4 )

 SharonTiny2002 <- mean(prePrices[415, ]$q1 +
                        prePrices[415, ]$q2 +
                        prePrices[415, ]$q3 +
                        prePrices[415, ]$q4 )

 SharonSmall2001 <- mean(prePrices[432, ]$q1 +
                         prePrices[432, ]$q2 +
                         prePrices[432, ]$q3 +
                         prePrices[432, ]$q4 )

 SharonSmall2002 <- mean(prePrices[433, ]$q1 +
                         prePrices[433, ]$q2 +
                         prePrices[433, ]$q3 +
                         prePrices[433, ]$q4 )

 SharonMed2001 <- mean(prePrices[450 ,]$q1 +
                       prePrices[450 ,]$q2 +
                       prePrices[450 ,]$q3 +
                       prePrices[450 ,]$q4 )

 SharonMed2002   <- mean(prePrices[451,]$q1 +
                         prePrices[451,]$q2 +
                         prePrices[451,]$q3 +
                         prePrices[451,]$q4 )

 SharonLarge2001 <- mean(prePrices[450+18, ]$q1 +
                         prePrices[450+18, ]$q2 +
                         prePrices[450+18, ]$q3 +
                         prePrices[450+18, ]$q4 )

 SharonLarge2002 <- mean(prePrices[450+19, ]$q1 +
                         prePrices[450+19, ]$q2 +
                         prePrices[450+19, ]$q3 +
                         prePrices[450+19, ]$q4 )

#################    North ################33

NorthTiny2003  <- prePrices[485,]$annual
NorthSmall2003 <- prePrices[485 + 18,]$annual
NorthMed2003   <- prePrices[485 + 36,]$annual
NorthLarge2003 <- prePrices[485 + 54,]$annual


 NorthTiny2001 <-  mean(prePrices[483,]$q1 +
                        prePrices[483,]$q2 +
                        prePrices[483,]$q3 +
                        prePrices[483,]$q4 )

 NorthTiny2002 <-  mean(prePrices[483+1, ]$q1 +
                        prePrices[483+1, ]$q2 +
                        prePrices[483+1, ]$q3 +
                        prePrices[483+1, ]$q4 )

 NorthSmall2001 <-  mean(prePrices[483+18, ]$q1 +
                         prePrices[483+18, ]$q2 +
                         prePrices[483+18, ]$q3 +
                         prePrices[483+18, ]$q4 )

 NorthSmall2002 <-  mean(prePrices[483+19, ]$q1 +
                         prePrices[483+19, ]$q2 +
                         prePrices[483+19, ]$q3 +
                         prePrices[483+19, ]$q4 )

 NorthMed2001 <-  mean(prePrices[483 + 36,]$q1 +
                       prePrices[483 + 36,]$q2 +
                       prePrices[483 + 36,]$q3 +
                       prePrices[483 + 36,]$q4 )

 NorthMed2002   <-  mean(prePrices[483 + 38,]$q1 +
                         prePrices[483 + 38,]$q2 +
                         prePrices[483 + 38,]$q3 +
                         prePrices[483 + 38,]$q4 )

 NorthLarge2001 <-  mean(prePrices[483+36+18, ]$q1 +
                         prePrices[483+36+18, ]$q2 +
                         prePrices[483+36+18, ]$q3 +
                         prePrices[483+36+18, ]$q4 )

 NorthLarge2002 <-  mean(prePrices[483+36+19, ]$q1 +
                         prePrices[483+36+19, ]$q2 +
                         prePrices[483+36+19, ]$q3 +
                         prePrices[483+36+19, ]$q4 )

#################    QrayotHaifa ################33

QrayotHaifaTiny2003  <- prePrices[554,]$annual
QrayotHaifaSmall2003 <- prePrices[554 + 18,]$annual
QrayotHaifaMed2003   <- prePrices[554 + 36,]$annual
QrayotHaifaLarge2003 <- prePrices[554 + 54,]$annual

 QrayotHaifaTiny2001 <- mean(prePrices[552,]$q1 +
                        prePrices[552,]$q2 +
                        prePrices[552,]$q3 +
                        prePrices[552,]$q4 )

 QrayotHaifaTiny2002 <- mean(prePrices[553, ]$q1 +
                        prePrices[553, ]$q2 +
                        prePrices[553, ]$q3 +
                        prePrices[553, ]$q4 )

 QrayotHaifaSmall2001 <- mean(prePrices[553+18, ]$q1 +
                         prePrices[553+18, ]$q2 +
                         prePrices[553+18, ]$q3 +
                         prePrices[553+18, ]$q4 )

 QrayotHaifaSmall2002 <- mean(prePrices[553+19, ]$q1 +
                         prePrices[553+19, ]$q2 +
                         prePrices[553+19, ]$q3 +
                         prePrices[553+19, ]$q4 )

 QrayotHaifaMed2001 <- mean(prePrices[588,]$q1 +
                       prePrices[588,]$q2 +
                       prePrices[588,]$q3 +
                       prePrices[588,]$q4 )

 QrayotHaifaMed2002   <- mean(prePrices[589,]$q1 +
                         prePrices[589,]$q2 +
                         prePrices[589,]$q3 +
                         prePrices[589,]$q4 )

 QrayotHaifaLarge2001 <- NA

 QrayotHaifaLarge2002 <- NA

## add these years to the coulumns
fam2004g <-
     fam2004f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDanTiny2003,
regionOne == "Gush Dan" && size == "Small"  ~ GushDanSmall2003,
regionOne == "Gush Dan" && size == "Medium" ~ GushDanMed2003,
regionOne == "Gush Dan" && size == "Large"  ~ GushDanLarge2003,
regionOne == "HaDarom" && size == "Tiny"   ~ SouthTiny2003,
regionOne == "HaDarom" && size == "Small"  ~ SouthSmall2003,
regionOne == "HaDarom" && size == "Medium" ~ SouthMed2003,
regionOne == "HaDarom" && size == "Large"  ~ SouthLarge2003,
regionOne == "Haifa" && size == "Tiny"   ~ HaifaTiny2003,
regionOne == "Haifa" && size == "Small"  ~ HaifaSmall2003,
regionOne == "Haifa" && size == "Medium" ~ HaifaMed2003,
regionOne == "Haifa" && size == "Large"  ~ HaifaLarge2003,
regionOne == "HaMerkaz" && size == "Tiny"   ~ CenterTiny2003,
regionOne == "HaMerkaz" && size == "Small"  ~ CenterSmall2003,
regionOne == "HaMerkaz" && size == "Medium" ~ CenterMed2003,
regionOne == "HaMerkaz" && size == "Large"  ~ CenterLarge2003,
regionOne == "Jerusalem" && size == "Tiny"   ~ JeruTiny2003,
regionOne == "Jerusalem" && size == "Small"  ~ JeruSmall2003,
regionOne == "Jerusalem" && size == "Medium" ~ JeruMed2003,
regionOne == "Jerusalem" && size == "Large"  ~ JeruLarge2003,
regionOne == "Sharon" && size == "Tiny"   ~ SharonTiny2003,
regionOne == "Sharon" && size == "Small"  ~ SharonSmall2003,
regionOne == "Sharon" && size == "Medium" ~ SharonMed2003,
regionOne == "Sharon" && size == "Large"  ~ SharonLarge2003,
regionOne == "Tel Aviv" && size == "Tiny"   ~ TLVTiny2003,
regionOne == "Tel Aviv" && size == "Small"  ~ TLVSmall2003,
regionOne == "Tel Aviv" && size == "Medium" ~ TLVMed2003,
regionOne == "Tel Aviv" && size == "Large"  ~ TLVLarge2003,
                                                           TRUE ~ 999.99))

fam2004h <-
     fam2004g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDanTiny2002,
regionOne == "Gush Dan" && size == "Small"  ~ GushDanSmall2002,
regionOne == "Gush Dan" && size == "Medium" ~ GushDanMed2002,
regionOne == "Gush Dan" && size == "Large"  ~ GushDanLarge2002,
regionOne == "HaDarom" && size == "Tiny"   ~ SouthTiny2002,
regionOne == "HaDarom" && size == "Small"  ~ SouthSmall2002,
regionOne == "HaDarom" && size == "Medium" ~ SouthMed2002,
regionOne == "HaDarom" && size == "Large"  ~ SouthLarge2002,
regionOne == "Haifa" && size == "Tiny"   ~ HaifaTiny2002,
regionOne == "Haifa" && size == "Small"  ~ HaifaSmall2002,
regionOne == "Haifa" && size == "Medium" ~ HaifaMed2002,
regionOne == "Haifa" && size == "Large"  ~ HaifaLarge2002,
regionOne == "HaMerkaz" && size == "Tiny"   ~ CenterTiny2002,
regionOne == "HaMerkaz" && size == "Small"  ~ CenterSmall2002,
regionOne == "HaMerkaz" && size == "Medium" ~ CenterMed2002,
regionOne == "HaMerkaz" && size == "Large"  ~ CenterLarge2002,
regionOne == "Jerusalem" && size == "Tiny"   ~ JeruTiny2002,
regionOne == "Jerusalem" && size == "Small"  ~ JeruSmall2002,
regionOne == "Jerusalem" && size == "Medium" ~ JeruMed2002,
regionOne == "Jerusalem" && size == "Large"  ~ JeruLarge2002,
regionOne == "Sharon" && size == "Tiny"   ~ SharonTiny2002,
regionOne == "Sharon" && size == "Small"  ~ SharonSmall2002,
regionOne == "Sharon" && size == "Medium" ~ SharonMed2002,
regionOne == "Sharon" && size == "Large"  ~ SharonLarge2002,
regionOne == "Tel Aviv" && size == "Tiny"   ~ TLVTiny2002,
regionOne == "Tel Aviv" && size == "Small"  ~ TLVSmall2002,
regionOne == "Tel Aviv" && size == "Medium" ~ TLVMed2002,
regionOne == "Tel Aviv" && size == "Large"  ~ TLVLarge2002,
                                                           TRUE ~ 999.99))

fam2004g <<-
     fam2004h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDanTiny2001,
regionOne == "Gush Dan" && size == "Small"  ~ GushDanSmall2001,
regionOne == "Gush Dan" && size == "Medium" ~ GushDanMed2001,
regionOne == "Gush Dan" && size == "Large"  ~ GushDanLarge2001,
regionOne == "HaDarom"  && size == "Tiny"   ~ SouthTiny2001,
regionOne == "HaDarom"  && size == "Small"  ~ SouthSmall2001,
regionOne == "HaDarom"  && size == "Medium" ~ SouthMed2001,
regionOne == "HaDarom"  && size == "Large"  ~ SouthLarge2001,
regionOne == "Haifa"    && size == "Tiny"   ~ HaifaTiny2001,
regionOne == "Haifa"    && size == "Small"  ~ HaifaSmall2001,
regionOne == "Haifa"    && size == "Medium" ~ HaifaMed2001,
regionOne == "Haifa"    && size == "Large"  ~ HaifaLarge2001,
regionOne == "HaMerkaz" && size == "Tiny"   ~ CenterTiny2001,
regionOne == "HaMerkaz" && size == "Small"  ~ CenterSmall2001,
regionOne == "HaMerkaz" && size == "Medium" ~ CenterMed2001,
regionOne == "HaMerkaz" && size == "Large"  ~ CenterLarge2001,
regionOne == "Jerusalem" && size == "Tiny"   ~ JeruTiny2001,
regionOne == "Jerusalem" && size == "Small"  ~ JeruSmall2001,
regionOne == "Jerusalem" && size == "Medium" ~ JeruMed2001,
regionOne == "Jerusalem" && size == "Large"  ~ JeruLarge2001,
regionOne == "Sharon"    && size == "Tiny"   ~ SharonTiny2001,
regionOne == "Sharon"    && size == "Small"  ~ SharonSmall2001,
regionOne == "Sharon"    && size == "Medium" ~ SharonMed2001,
regionOne == "Sharon"    && size == "Large"  ~ SharonLarge2001,
regionOne == "Tel Aviv"  && size == "Tiny"   ~ TLVTiny2001,
regionOne == "Tel Aviv"  && size == "Small"  ~ TLVSmall2001,
regionOne == "Tel Aviv"  && size == "Medium" ~ TLVMed2001,
regionOne == "Tel Aviv"  && size == "Large"  ~ TLVLarge2001,
                                                           TRUE ~ 999.99))

### 2005
fam2005g  <- fam2005f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[3, 2],
regionOne == "HaDarom"  && size == "Tiny"   ~ South1.5[3, 2],
regionOne == "HaDarom"  && size == "Small"  ~ South2.5[3, 2],
regionOne == "HaDarom"  && size == "Medium" ~ South3.5[3, 2],
regionOne == "HaDarom"  && size == "Large"  ~ South4.5[3, 2],
regionOne == "Haifa"    && size == "Tiny"   ~ Haifa1.5[3, 2],
regionOne == "Haifa"    && size == "Small"  ~ Haifa2.5[3, 2],
regionOne == "Haifa"    && size == "Medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa"    && size == "Large"  ~ Haifa4.5[3, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[3, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[3, 2],
regionOne == "Sharon"    && size == "Tiny"   ~ Sharon1.5[3, 2],
regionOne == "Sharon"    && size == "Small"  ~ Sharon2.5[3, 2],
regionOne == "Sharon"    && size == "Medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon"    && size == "Large"  ~ Sharon4.5[3, 2],
regionOne == "Tel Aviv"  && size == "Tiny"   ~ tlv1.5[3, 2],
regionOne == "Tel Aviv"  && size == "Small"  ~ tlv2.5[3, 2],
regionOne == "Tel Aviv"  && size == "Medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv"  && size == "Large"  ~ tlv4.5[3, 2],
                                              TRUE ~ 999.99))

fam2005h <-
     fam2005g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDanTiny2003,
regionOne == "Gush Dan" && size == "Small"  ~ GushDanSmall2003,
regionOne == "Gush Dan" && size == "Medium" ~ GushDanMed2003,
regionOne == "Gush Dan" && size == "Large"  ~ GushDanLarge2003,
regionOne == "HaDarom" && size == "Tiny"   ~ SouthTiny2003,
regionOne == "HaDarom" && size == "Small"  ~ SouthSmall2003,
regionOne == "HaDarom" && size == "Medium" ~ SouthMed2003,
regionOne == "HaDarom" && size == "Large"  ~ SouthLarge2003,
regionOne == "Haifa" && size == "Tiny"   ~ HaifaTiny2003,
regionOne == "Haifa" && size == "Small"  ~ HaifaSmall2003,
regionOne == "Haifa" && size == "Medium" ~ HaifaMed2003,
regionOne == "Haifa" && size == "Large"  ~ HaifaLarge2003,
regionOne == "HaMerkaz" && size == "Tiny"   ~ CenterTiny2003,
regionOne == "HaMerkaz" && size == "Small"  ~ CenterSmall2003,
regionOne == "HaMerkaz" && size == "Medium" ~ CenterMed2003,
regionOne == "HaMerkaz" && size == "Large"  ~ CenterLarge2003,
regionOne == "Jerusalem" && size == "Tiny"   ~ JeruTiny2003,
regionOne == "Jerusalem" && size == "Small"  ~ JeruSmall2003,
regionOne == "Jerusalem" && size == "Medium" ~ JeruMed2003,
regionOne == "Jerusalem" && size == "Large"  ~ JeruLarge2003,
regionOne == "Sharon" && size == "Tiny"   ~ SharonTiny2003,
regionOne == "Sharon" && size == "Small"  ~ SharonSmall2003,
regionOne == "Sharon" && size == "Medium" ~ SharonMed2003,
regionOne == "Sharon" && size == "Large"  ~ SharonLarge2003,
regionOne == "Tel Aviv" && size == "Tiny"   ~ TLVTiny2003,
regionOne == "Tel Aviv" && size == "Small"  ~ TLVSmall2003,
regionOne == "Tel Aviv" && size == "Medium" ~ TLVMed2003,
regionOne == "Tel Aviv" && size == "Large"  ~ TLVLarge2003,
                                                           TRUE ~ 999.99))

fam2005g <<-
     fam2005h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDanTiny2002,
regionOne == "Gush Dan" && size == "Small"  ~ GushDanSmall2002,
regionOne == "Gush Dan" && size == "Medium" ~ GushDanMed2002,
regionOne == "Gush Dan" && size == "Large"  ~ GushDanLarge2002,
regionOne == "HaDarom" && size == "Tiny"   ~ SouthTiny2002,
regionOne == "HaDarom" && size == "Small"  ~ SouthSmall2002,
regionOne == "HaDarom" && size == "Medium" ~ SouthMed2002,
regionOne == "HaDarom" && size == "Large"  ~ SouthLarge2002,
regionOne == "Haifa" && size == "Tiny"   ~ HaifaTiny2002,
regionOne == "Haifa" && size == "Small"  ~ HaifaSmall2002,
regionOne == "Haifa" && size == "Medium" ~ HaifaMed2002,
regionOne == "Haifa" && size == "Large"  ~ HaifaLarge2002,
regionOne == "HaMerkaz" && size == "Tiny"   ~ CenterTiny2002,
regionOne == "HaMerkaz" && size == "Small"  ~ CenterSmall2002,
regionOne == "HaMerkaz" && size == "Medium" ~ CenterMed2002,
regionOne == "HaMerkaz" && size == "Large"  ~ CenterLarge2002,
regionOne == "Jerusalem" && size == "Tiny"   ~ JeruTiny2002,
regionOne == "Jerusalem" && size == "Small"  ~ JeruSmall2002,
regionOne == "Jerusalem" && size == "Medium" ~ JeruMed2002,
regionOne == "Jerusalem" && size == "Large"  ~ JeruLarge2002,
regionOne == "Sharon" && size == "Tiny"   ~ SharonTiny2002,
regionOne == "Sharon" && size == "Small"  ~ SharonSmall2002,
regionOne == "Sharon" && size == "Medium" ~ SharonMed2002,
regionOne == "Sharon" && size == "Large"  ~ SharonLarge2002,
regionOne == "Tel Aviv" && size == "Tiny"   ~ TLVTiny2002,
regionOne == "Tel Aviv" && size == "Small"  ~ TLVSmall2002,
regionOne == "Tel Aviv" && size == "Medium" ~ TLVMed2002,
regionOne == "Tel Aviv" && size == "Large"  ~ TLVLarge2002,
                                                           TRUE ~ 999.99))

### 2006
fam2006g <-
     fam2006f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[7, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[7, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[7, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[7, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[7, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[7, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[7, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[7, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[7, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[7, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[7, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[7, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[7, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[7, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[7, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[7, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[7, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[7, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[7, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[7, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[7, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[7, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[7, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[7, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[7, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[7, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[7, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[7, 2],
                                                           TRUE ~ 999.99))

fam2006h <-
     fam2006g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[3, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[3, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[3, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[3, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[3, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[3, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[3, 2],
                                                           TRUE ~ 999.99))

fam2006g <<-
     fam2006h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDanTiny2003,
regionOne == "Gush Dan" && size == "Small"  ~ GushDanSmall2003,
regionOne == "Gush Dan" && size == "Medium" ~ GushDanMed2003,
regionOne == "Gush Dan" && size == "Large"  ~ GushDanLarge2003,
regionOne == "HaDarom" && size == "Tiny"   ~ SouthTiny2003,
regionOne == "HaDarom" && size == "Small"  ~ SouthSmall2003,
regionOne == "HaDarom" && size == "Medium" ~ SouthMed2003,
regionOne == "HaDarom" && size == "Large"  ~ SouthLarge2003,
regionOne == "Haifa" && size == "Tiny"   ~ HaifaTiny2003,
regionOne == "Haifa" && size == "Small"  ~ HaifaSmall2003,
regionOne == "Haifa" && size == "Medium" ~ HaifaMed2003,
regionOne == "Haifa" && size == "Large"  ~ HaifaLarge2003,
regionOne == "HaMerkaz" && size == "Tiny"   ~ CenterTiny2003,
regionOne == "HaMerkaz" && size == "Small"  ~ CenterSmall2003,
regionOne == "HaMerkaz" && size == "Medium" ~ CenterMed2003,
regionOne == "HaMerkaz" && size == "Large"  ~ CenterLarge2003,
regionOne == "Jerusalem" && size == "Tiny"   ~ JeruTiny2003,
regionOne == "Jerusalem" && size == "Small"  ~ JeruSmall2003,
regionOne == "Jerusalem" && size == "Medium" ~ JeruMed2003,
regionOne == "Jerusalem" && size == "Large"  ~ JeruLarge2003,
regionOne == "Sharon" && size == "Tiny"   ~ SharonTiny2003,
regionOne == "Sharon" && size == "Small"  ~ SharonSmall2003,
regionOne == "Sharon" && size == "Medium" ~ SharonMed2003,
regionOne == "Sharon" && size == "Large"  ~ SharonLarge2003,
regionOne == "Tel Aviv" && size == "Tiny"   ~ TLVTiny2003,
regionOne == "Tel Aviv" && size == "Small"  ~ TLVSmall2003,
regionOne == "Tel Aviv" && size == "Medium" ~ TLVMed2003,
regionOne == "Tel Aviv" && size == "Large"  ~ TLVLarge2003,
                                                           TRUE ~ 999.99))

### 2007
fam2007g <-
     fam2007f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[11, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[11, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[11, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[11, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[11, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[11, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[11, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[11, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[11, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[11, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[11, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[11, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[11, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[11, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[11, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[11, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[11, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[11, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[11, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[11, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[11, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[11, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[11, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[11, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[11, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[11, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[11, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[11, 2],
                                                           TRUE ~ 999.99))

fam2007h <-
     fam2007g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[7, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[7, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[7, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[7, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[7, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[7, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[7, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[7, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[7, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[7, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[7, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[7, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[7, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[7, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[7, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[7, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[7, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[7, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[7, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[7, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[7, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[7, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[7, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[7, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[7, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[7, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[7, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[7, 2],
                                                           TRUE ~ 999.99))

fam2007g <<-
     fam2007h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[3, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[3, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[3, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[3, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[3, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[3, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[3, 2],
                                                           TRUE ~ 999.99))

### 2008
fam2008g <-
     fam2008f %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[11, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[11, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[11, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[11, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[11, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[11, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[11, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[11, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[11, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[11, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[11, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[11, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[11, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[11, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[11, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[11, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[11, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[11, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[11, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[11, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[11, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[11, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[11, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[11, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[11, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[11, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[11, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[11, 2],
                                                           TRUE ~ 999.99))

fam2008h <-
     fam2008g %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[7, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[7, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[7, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[7, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[7, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[7, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[7, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[7, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[7, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[7, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[7, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[7, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[7, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[7, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[7, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[7, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[7, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[7, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[7, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[7, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[7, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[7, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[7, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[7, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[7, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[7, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[7, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[7, 2],
                                                           TRUE ~ 999.99))



fam2008g <<-
     fam2008h %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[15, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[15, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[15, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[15, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[15, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[15, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[15, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[15, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[15, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[15, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[15, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[15, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[15, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[15, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[15, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[15, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[15, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[15, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[15, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[15, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[15, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[15, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[15, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[15, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[15, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[15, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[15, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[15, 2],
                                                           TRUE ~ 999.99))


### 2009
fam2009g <-
     fam2009f %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[11, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[11, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[11, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[11, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[11, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[11, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[11, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[11, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[11, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[11, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[11, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[11, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[11, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[11, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[11, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[11, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[11, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[11, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[11, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[11, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[11, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[11, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[11, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[11, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[11, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[11, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[11, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[11, 2],
                                                           TRUE ~ 999.99))

fam2009h <-
     fam2009g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[15, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[15, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[15, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[15, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[15, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[15, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[15, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[15, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[15, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[15, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[15, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[15, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[15, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[15, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[15, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[15, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[15, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[15, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[15, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[15, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[15, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[15, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[15, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[15, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[15, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[15, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[15, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[15, 2],
                                                           TRUE ~ 999.99))



fam2009g <<-
     fam2009h %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[19, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[19, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[19, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[19, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[19, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[19, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[19, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[19, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[19, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[19, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[19, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[19, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[19, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[19, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[19, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[19, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[19, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[19, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[19, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[19, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[19, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[19, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[19, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[19, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[19, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[19, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[19, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[19, 2],
                                                           TRUE ~ 999.99))



### 2010
fam2010g <-
     fam2010f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[19, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[19, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[19, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[19, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[19, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[19, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[19, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[19, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[19, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[19, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[19, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[19, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[19, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[19, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[19, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[19, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[19, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[19, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[19, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[19, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[19, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[19, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[19, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[19, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[19, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[19, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[19, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[19, 2],
                                                           TRUE ~ 999.99))

fam2010h <-
     fam2010g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[19, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[19, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[19, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[19, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[19, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[19, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[19, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[19, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[19, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[19, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[19, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[19, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[19, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[19, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[19, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[19, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[19, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[19, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[19, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[19, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[19, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[19, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[19, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[19, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[19, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[19, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[19, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[19, 2],
                                                           TRUE ~ 999.99))



fam2010g <<-
     fam2010h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[15, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[15, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[15, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[15, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[15, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[15, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[15, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[15, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[15, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[15, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[15, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[15, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[15, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[15, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[15, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[15, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[15, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[15, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[15, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[15, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[15, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[15, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[15, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[15, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[15, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[15, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[15, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[15, 2],
                                                           TRUE ~ 999.99))



### 2011

fam2011g <-
     fam2011f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[27, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[27, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[27, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[27, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[27, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[27, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[27, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[27, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[27, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[27, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[27, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[27, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[27, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[27, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[27, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[27, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[27, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[27, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[27, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[27, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[27, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[27, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[27, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[27, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[27, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[27, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[27, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[27, 2],
                                                           TRUE ~ 999.99))

fam2011h <-
     fam2011g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[23, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[23, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[23, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[23, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[23, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[23, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[23, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[23, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[23, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[23, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[23, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[23, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[23, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[23, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[23, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[23, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[23, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[23, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[23, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[23, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[23, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[23, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[23, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[23, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[23, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[23, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[23, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[23, 2],
                                                           TRUE ~ 999.99))



fam2011g <<-
     fam2011h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[19, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[19, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[19, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[19, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[19, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[19, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[19, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[19, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[19, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[19, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[19, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[19, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[19, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[19, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[19, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[19, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[19, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[19, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[19, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[19, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[19, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[19, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[19, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[19, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[19, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[19, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[19, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[19, 2],
                                                           TRUE ~ 999.99))
### 2012
fam2012g <-
     fam2012f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[31, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[31, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[31, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[31, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[31, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[31, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[31, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[31, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[31, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[31, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[31, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[31, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[31, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[31, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[31, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[31, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[31, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[31, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[31, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[31, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[31, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[31, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[31, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[31, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[31, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[31, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[31, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[31, 2],
                                                           TRUE ~ 999.99))

fam2012h <-
     fam2012g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[27, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[27, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[27, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[27, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[27, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[27, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[27, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[27, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[27, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[27, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[27, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[27, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[27, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[27, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[27, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[27, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[27, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[27, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[27, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[27, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[27, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[27, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[27, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[27, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[27, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[27, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[27, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[27, 2],
                                                           TRUE ~ 999.99))



fam2012g <<-
     fam2012h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[23, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[23, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[23, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[23, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[23, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[23, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[23, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[23, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[23, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[23, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[23, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[23, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[23, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[23, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[23, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[23, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[23, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[23, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[23, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[23, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[23, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[23, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[23, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[23, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[23, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[23, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[23, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[23, 2],
                                                           TRUE ~ 999.99))


### 2013
fam2013g <-
     fam2013f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[35, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[35, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[35, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[35, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[35, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[35, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[35, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[35, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[35, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[35, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[35, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[35, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[35, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[35, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[35, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[35, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[35, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[35, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[35, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[35, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[35, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[35, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[35, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[35, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[35, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[35, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[35, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[35, 2],
                                                           TRUE ~ 999.99))

fam2013h <-
     fam2013g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[31, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[31, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[31, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[31, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[31, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[31, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[31, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[31, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[31, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[31, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[31, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[31, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[31, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[31, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[31, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[31, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[31, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[31, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[31, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[31, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[31, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[31, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[31, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[31, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[31, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[31, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[31, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[31, 2],
                                                           TRUE ~ 999.99))

fam2013g <<-
     fam2013h %>%
     mutate(housePtminus3 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[27, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[27, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[27, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[27, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[27, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[27, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[27, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[27, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[27, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[27, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[27, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[27, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[27, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[27, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[27, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[27, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[27, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[27, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[27, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[27, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[27, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[27, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[27, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[27, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[27, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[27, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[27, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[27, 2],
                                                           TRUE ~ 999.99))
### 2014
fam2014g <-
     fam2014f %>%
     mutate(housePtminus1 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[39, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[39, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[39, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[39, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[39, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[39, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[39, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[39, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[39, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[39, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[39, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[39, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[39, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[39, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[39, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[39, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[39, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[39, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[39, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[39, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[39, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[39, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[39, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[39, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[39, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[39, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[39, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[39, 2],
                                                           TRUE ~ 999.99))
fam2014h <-
     fam2014g %>%
     mutate(housePtminus2 = case_when(
regionOne == "Gush Dan" && size == "Tiny"   ~ GushDan1.5[35, 2],
regionOne == "Gush Dan" && size == "Small"  ~ GushDan2.5[35, 2],
regionOne == "Gush Dan" && size == "Medium" ~ GushDan3.5[35, 2],
regionOne == "Gush Dan" && size == "Large"  ~ GushDan4.5[35, 2],
regionOne == "HaDarom" && size == "Tiny"   ~ South1.5[35, 2],
regionOne == "HaDarom" && size == "Small"  ~ South2.5[35, 2],
regionOne == "HaDarom" && size == "Medium" ~ South3.5[35, 2],
regionOne == "HaDarom" && size == "Large"  ~ South4.5[35, 2],
regionOne == "Haifa" && size == "Tiny"   ~ Haifa1.5[35, 2],
regionOne == "Haifa" && size == "Small"  ~ Haifa2.5[35, 2],
regionOne == "Haifa" && size == "Medium" ~ Haifa3.5[35, 2],
regionOne == "Haifa" && size == "Large"  ~ Haifa4.5[35, 2],
regionOne == "HaMerkaz" && size == "Tiny"   ~ centerJeruPeri1.5[35, 2],
regionOne == "HaMerkaz" && size == "Small"  ~ centerJeruPeri2.5[35, 2],
regionOne == "HaMerkaz" && size == "Medium" ~ centerJeruPeri3.5[35, 2],
regionOne == "HaMerkaz" && size == "Large"  ~ centerJeruPeri4.5[35, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[35, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[35, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[35, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[35, 2],
regionOne == "Sharon" && size == "Tiny"   ~ Sharon1.5[35, 2],
regionOne == "Sharon" && size == "Small"  ~ Sharon2.5[35, 2],
regionOne == "Sharon" && size == "Medium" ~ Sharon3.5[35, 2],
regionOne == "Sharon" && size == "Large"  ~ Sharon4.5[35, 2],
regionOne == "Tel Aviv" && size == "Tiny"   ~ tlv1.5[35, 2],
regionOne == "Tel Aviv" && size == "Small"  ~ tlv2.5[35, 2],
regionOne == "Tel Aviv" && size == "Medium" ~ tlv3.5[35, 2],
regionOne == "Tel Aviv" && size == "Large"  ~ tlv4.5[35, 2],
                                                           TRUE ~ 999.99))

fam2014g <<-
     fam2014h %>%
     mutate(housePtminus3 = case_when(   ## 31 is 2013.5 or q2 or mid-year price
regionOne == "Gush Dan" && size == "Tiny"    ~ GushDan1.5[31, 2],
regionOne == "Gush Dan" && size == "Small"   ~ GushDan2.5[31, 2],
regionOne == "Gush Dan" && size == "Medium"  ~ GushDan3.5[31, 2],
regionOne == "Gush Dan" && size == "Large"   ~ GushDan4.5[31, 2],
regionOne == "HaDarom" && size == "Tiny"     ~ South1.5[31, 2],
regionOne == "HaDarom" && size == "Small"    ~ South2.5[31, 2],
regionOne == "HaDarom" && size == "Medium"   ~ South3.5[31, 2],
regionOne == "HaDarom" && size == "Large"    ~ South4.5[31, 2],
regionOne == "Haifa" && size == "Tiny"       ~ Haifa1.5[31, 2],
regionOne == "Haifa" && size == "Small"      ~ Haifa2.5[31, 2],
regionOne == "Haifa" && size == "Medium"     ~ Haifa3.5[31, 2],
regionOne == "Haifa" && size == "Large"      ~ Haifa4.5[31, 2],
regionOne == "HaMerkaz" && size == "Tiny"    ~ centerJeruPeri1.5[31, 2],
regionOne == "HaMerkaz" && size == "Small"   ~ centerJeruPeri2.5[31, 2],
regionOne == "HaMerkaz" && size == "Medium"  ~ centerJeruPeri3.5[31, 2],
regionOne == "HaMerkaz" && size == "Large"   ~ centerJeruPeri4.5[31, 2],
regionOne == "Jerusalem" && size == "Tiny"   ~ Jerusalem1.5[31, 2],
regionOne == "Jerusalem" && size == "Small"  ~ Jerusalem2.5[31, 2],
regionOne == "Jerusalem" && size == "Medium" ~ Jerusalem3.5[31, 2],
regionOne == "Jerusalem" && size == "Large"  ~ Jerusalem4.5[31, 2],
regionOne == "Sharon" && size == "Tiny"      ~ Sharon1.5[31, 2],
regionOne == "Sharon" && size == "Small"     ~ Sharon2.5[31, 2],
regionOne == "Sharon" && size == "Medium"    ~ Sharon3.5[31, 2],
regionOne == "Sharon" && size == "Large"     ~ Sharon4.5[31, 2],
regionOne == "Tel Aviv" && size == "Tiny"    ~ tlv1.5[31, 2],
regionOne == "Tel Aviv" && size == "Small"   ~ tlv2.5[31, 2],
regionOne == "Tel Aviv" && size == "Medium"  ~ tlv3.5[31, 2],
regionOne == "Tel Aviv" && size == "Large"   ~ tlv4.5[31, 2],
                                                           TRUE ~ 999.99))
return("added cols for Simple Moving Average (SMA).")

    } else if (arg == "whshock") {

## calculate housing wealth shock
      ## 2004
      fam2004g$diffPriorHP  <<-(fam2004g$housePtminus1[1] - fam2004g$housePtminus2[1])/ fam2004g$housePtminus2[1]
      fam2004g$diffNowHP    <<-(fam2004g$houseP           - fam2004g$housePtminus1[1])/ fam2004g$housePtminus1[1]
      fam2004g$Wshock_H     <<- fam2004g$diffNow - fam2004g$diffPrior

      ## 2005
      fam2005g$diffPriorHP  <<-(fam2005g$housePtminus1[1] - fam2005g$housePtminus2[1])/ fam2005g$housePtminus2[1]
      fam2005g$diffNowHP    <<-(fam2005g$houseP           - fam2005g$housePtminus1[1])/ fam2005g$housePtminus1[1]
      fam2005g$Wshock_H     <<- fam2005g$diffNow - fam2005g$diffPrior

      ## 2006
      fam2006g$diffPriorHP  <<-(fam2006g$housePtminus1[1] - fam2006g$housePtminus2[1])/ fam2006g$housePtminus2[1]
      fam2006g$diffNowHP    <<-(fam2006g$houseP           - fam2006g$housePtminus1[1])/ fam2006g$housePtminus1[1]
      fam2006g$Wshock_H     <<- fam2006g$diffNow - fam2006g$diffPrior

      ## 2007
      fam2007g$diffPriorHP  <<-(fam2007g$housePtminus1[1] - fam2007g$housePtminus2[1])/ fam2007g$housePtminus2[1]
      fam2007g$diffNowHP    <<-(fam2007g$houseP           - fam2007g$housePtminus1[1])/ fam2007g$housePtminus1[1]
      fam2007g$Wshock_H     <<- fam2007g$diffNow - fam2007g$diffPrior

      ## 2008
      fam2008g$diffPriorHP  <<-(fam2008g$housePtminus1[1] - fam2008g$housePtminus2[1])/ fam2008g$housePtminus2[1]
      fam2008g$diffNowHP    <<-(fam2008g$houseP           - fam2008g$housePtminus1[1])/ fam2008g$housePtminus1[1]
      fam2008g$Wshock_H     <<- fam2008g$diffNow - fam2008g$diffPrior

      ## 2009
      fam2009g$diffPriorHP  <<-(fam2009g$housePtminus1[1] - fam2009g$housePtminus2[1])/ fam2009g$housePtminus2[1]
      fam2009g$diffNowHP    <<-(fam2009g$houseP           - fam2009g$housePtminus1[1])/ fam2009g$housePtminus1[1]
      fam2009g$Wshock_H     <<- fam2009g$diffNow - fam2009g$diffPrior

      ## 2010
      fam2010g$diffPriorHP  <<-(fam2010g$housePtminus1[1] - fam2010g$housePtminus2[1])/ fam2010g$housePtminus2[1]
      fam2010g$diffNowHP    <<-(fam2010g$houseP           - fam2010g$housePtminus1[1])/ fam2010g$housePtminus1[1]
      fam2010g$Wshock_H     <<- fam2010g$diffNow - fam2010g$diffPrior

      ## 2011
      fam2011g$diffPriorHP  <<-(fam2011g$housePtminus1[1] - fam2011g$housePtminus2[1])/ fam2011g$housePtminus2[1]
      fam2011g$diffNowHP    <<-(fam2011g$houseP           - fam2011g$housePtminus1[1])/ fam2011g$housePtminus1[1]
      fam2011g$Wshock_H     <<- fam2011g$diffNow - fam2011g$diffPrior

      ## 2012
      fam2012g$diffPriorHP  <<-(fam2012g$housePtminus1[1] - fam2012g$housePtminus2[1])/ fam2012g$housePtminus2[1]
      fam2012g$diffNowHP    <<-(fam2012g$houseP           - fam2012g$housePtminus1[1])/ fam2012g$housePtminus1[1]
      fam2012g$Wshock_H     <<- fam2012g$diffNow - fam2012g$diffPrior

      ## 2013
      fam2013g$diffPriorHP  <<-(fam2013g$housePtminus1[1] - fam2013g$housePtminus2[1])/ fam2013g$housePtminus2[1]
      fam2013g$diffNowHP    <<-(fam2013g$houseP           - fam2013g$housePtminus1[1])/ fam2013g$housePtminus1[1]
      fam2013g$Wshock_H     <<- fam2013g$diffNow - fam2013g$diffPrior

      ## 2014
      fam2014g$diffPriorHP  <<-(fam2014g$housePtminus1[1] - fam2014g$housePtminus2[1])/ fam2014g$housePtminus2[1]
      fam2014g$diffNowHP    <<-(fam2014g$houseP           - fam2014g$housePtminus1[1])/ fam2014g$housePtminus1[1]
      fam2014g$Wshock_H     <<- fam2014g$diffNow - fam2014g$diffPrior


return("Calculated yearly housing wealth shock.")
    } else {
        print("oarysdlaors9 -- not yet implemented.")
    }
}

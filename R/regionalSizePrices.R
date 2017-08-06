#' Add houseP column based on fam:(yr, size, region) v. 3.4
#'
#' This function adds $houseP and $housePTwo columns.
#' @param frame The frame: data/owners/renters
#' @family mutate
#' @keywords price, regional, case
#' @export regionalSizePrices
#' @examples
#' regionalSizePrices()
#' regionalSizePrices("famYYYY")
#' regionalSizePrices("data")
#' regionalSizePrices("owners")
#' regionalSizePrices("renters")
regionalSizePrices <- function(frame = TRUE) {
         if (frame == "data") {
Data6 <<-
     Data5 %>%
     mutate(houseP = case_when(
regionOne == "Jerusalem" && size5 == "tiny" && year == year[1] ~ Jerusalem1.5[3],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[2] ~ Jerusalem1.5[7],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[3] ~ Jerusalem1.5[11],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[4] ~ Jerusalem1.5[15],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[5] ~ Jerusalem1.5[19],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[6] ~ Jerusalem1.5[23],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[7] ~ Jerusalem1.5[27],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[8] ~ Jerusalem1.5[31],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[9] ~ Jerusalem1.5[35],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[10]~ Jerusalem1.5[39],
regionOne == "Jerusalem" && size5 == "tiny" && year == year[11]~ Jerusalem1.5[43],
##################
regionOne == "Jerusalem" && size5 == "small" && year == year[1] ~ Jerusalem2.5[3],
regionOne == "Jerusalem" && size5 == "small" && year == year[2] ~ Jerusalem2.5[7],
regionOne == "Jerusalem" && size5 == "small" && year == year[3] ~ Jerusalem2.5[11],
regionOne == "Jerusalem" && size5 == "small" && year == year[4] ~ Jerusalem2.5[15],
regionOne == "Jerusalem" && size5 == "small" && year == year[5] ~ Jerusalem2.5[19],
regionOne == "Jerusalem" && size5 == "small" && year == year[6] ~ Jerusalem2.5[23],
regionOne == "Jerusalem" && size5 == "small" && year == year[7] ~ Jerusalem2.5[27],
regionOne == "Jerusalem" && size5 == "small" && year == year[8] ~ Jerusalem2.5[31],
regionOne == "Jerusalem" && size5 == "small" && year == year[9] ~ Jerusalem2.5[35],
regionOne == "Jerusalem" && size5 == "small" && year == year[10]~ Jerusalem2.5[39],
regionOne == "Jerusalem" && size5 == "small" && year == year[11]~ Jerusalem2.5[43],
##################
regionOne == "Jerusalem" && size5 == "medium" && year == year[1] ~ Jerusalem3.5[3],
regionOne == "Jerusalem" && size5 == "medium" && year == year[2] ~ Jerusalem3.5[7],
regionOne == "Jerusalem" && size5 == "medium" && year == year[3] ~ Jerusalem3.5[11],
regionOne == "Jerusalem" && size5 == "medium" && year == year[4] ~ Jerusalem3.5[15],
regionOne == "Jerusalem" && size5 == "medium" && year == year[5] ~ Jerusalem3.5[19],
regionOne == "Jerusalem" && size5 == "medium" && year == year[6] ~ Jerusalem3.5[23],
regionOne == "Jerusalem" && size5 == "medium" && year == year[7] ~ Jerusalem3.5[27],
regionOne == "Jerusalem" && size5 == "medium" && year == year[8] ~ Jerusalem3.5[31],
regionOne == "Jerusalem" && size5 == "medium" && year == year[9] ~ Jerusalem3.5[35],
regionOne == "Jerusalem" && size5 == "medium" && year == year[10]~ Jerusalem3.5[39],
regionOne == "Jerusalem" && size5 == "medium" && year == year[11]~ Jerusalem3.5[43],
##################

regionOne == "Jerusalem" && size5 == "large" && year == year[1] ~ Jerusalem4.5[3],
regionOne == "Jerusalem" && size5 == "large" && year == year[2] ~ Jerusalem4.5[7],
regionOne == "Jerusalem" && size5 == "large" && year == year[3] ~ Jerusalem4.5[11],
regionOne == "Jerusalem" && size5 == "large" && year == year[4] ~ Jerusalem4.5[15],
regionOne == "Jerusalem" && size5 == "large" && year == year[5] ~ Jerusalem4.5[19],
regionOne == "Jerusalem" && size5 == "large" && year == year[6] ~ Jerusalem4.5[23],
regionOne == "Jerusalem" && size5 == "large" && year == year[7] ~ Jerusalem4.5[27],
regionOne == "Jerusalem" && size5 == "large" && year == year[8] ~ Jerusalem4.5[31],
regionOne == "Jerusalem" && size5 == "large" && year == year[9] ~ Jerusalem4.5[35],
regionOne == "Jerusalem" && size5 == "large" && year == year[10]~ Jerusalem4.5[39],
regionOne == "Jerusalem" && size5 == "large" && year == year[11]~ Jerusalem4.5[43],

##################
##################

regionOne == "Gush Dan" && size5 == "tiny" && year == year[1] ~ GushDan1.5[3],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[2] ~ GushDan1.5[7],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[3] ~ GushDan1.5[11],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[4] ~ GushDan1.5[15],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[5] ~ GushDan1.5[19],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[6] ~ GushDan1.5[23],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[7] ~ GushDan1.5[27],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[8] ~ GushDan1.5[31],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[9] ~ GushDan1.5[35],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[10]~ GushDan1.5[39],
regionOne == "Gush Dan" && size5 == "tiny" && year == year[11]~ GushDan1.5[43],
##################

regionOne == "Gush Dan" && size5 == "small" && year == year[1] ~ GushDan2.5[3],
regionOne == "Gush Dan" && size5 == "small" && year == year[2] ~ GushDan2.5[7],
regionOne == "Gush Dan" && size5 == "small" && year == year[3] ~ GushDan2.5[11],
regionOne == "Gush Dan" && size5 == "small" && year == year[4] ~ GushDan2.5[15],
regionOne == "Gush Dan" && size5 == "small" && year == year[5] ~ GushDan2.5[19],
regionOne == "Gush Dan" && size5 == "small" && year == year[6] ~ GushDan2.5[23],
regionOne == "Gush Dan" && size5 == "small" && year == year[7] ~ GushDan2.5[27],
regionOne == "Gush Dan" && size5 == "small" && year == year[8] ~ GushDan2.5[31],
regionOne == "Gush Dan" && size5 == "small" && year == year[9] ~ GushDan2.5[35],
regionOne == "Gush Dan" && size5 == "small" && year == year[10]~ GushDan2.5[39],
regionOne == "Gush Dan" && size5 == "small" && year == year[11]~ GushDan2.5[43],
##################


regionOne == "Gush Dan" && size5 == "medium" && year == year[1] ~ GushDan3.5[3],
regionOne == "Gush Dan" && size5 == "medium" && year == year[2] ~ GushDan3.5[7],
regionOne == "Gush Dan" && size5 == "medium" && year == year[3] ~ GushDan3.5[11],
regionOne == "Gush Dan" && size5 == "medium" && year == year[4] ~ GushDan3.5[15],
regionOne == "Gush Dan" && size5 == "medium" && year == year[5] ~ GushDan3.5[19],
regionOne == "Gush Dan" && size5 == "medium" && year == year[6] ~ GushDan3.5[23],
regionOne == "Gush Dan" && size5 == "medium" && year == year[7] ~ GushDan3.5[27],
regionOne == "Gush Dan" && size5 == "medium" && year == year[8] ~ GushDan3.5[31],
regionOne == "Gush Dan" && size5 == "medium" && year == year[9] ~ GushDan3.5[35],
regionOne == "Gush Dan" && size5 == "medium" && year == year[10]~ GushDan3.5[39],
regionOne == "Gush Dan" && size5 == "medium" && year == year[11]~ GushDan3.5[43],
##################

regionOne == "Gush Dan" && size5 == "large" && year == year[1] ~ GushDan4.5[3],
regionOne == "Gush Dan" && size5 == "large" && year == year[2] ~ GushDan4.5[7],
regionOne == "Gush Dan" && size5 == "large" && year == year[3] ~ GushDan4.5[11],
regionOne == "Gush Dan" && size5 == "large" && year == year[4] ~ GushDan4.5[15],
regionOne == "Gush Dan" && size5 == "large" && year == year[5] ~ GushDan4.5[19],
regionOne == "Gush Dan" && size5 == "large" && year == year[6] ~ GushDan4.5[23],
regionOne == "Gush Dan" && size5 == "large" && year == year[7] ~ GushDan4.5[27],
regionOne == "Gush Dan" && size5 == "large" && year == year[8] ~ GushDan4.5[31],
regionOne == "Gush Dan" && size5 == "large" && year == year[9] ~ GushDan4.5[35],
regionOne == "Gush Dan" && size5 == "large" && year == year[10]~ GushDan4.5[39],
regionOne == "Gush Dan" && size5 == "large" && year == year[11]~ GushDan4.5[43],

##################
##################


regionOne == "HaDarom" && size5 == "tiny" && year == year[1] ~ South1.5[3],
regionOne == "HaDarom" && size5 == "tiny" && year == year[2] ~ South1.5[7],
regionOne == "HaDarom" && size5 == "tiny" && year == year[3] ~ South1.5[11],
regionOne == "HaDarom" && size5 == "tiny" && year == year[4] ~ South1.5[15],
regionOne == "HaDarom" && size5 == "tiny" && year == year[5] ~ South1.5[19],
regionOne == "HaDarom" && size5 == "tiny" && year == year[6] ~ South1.5[23],
regionOne == "HaDarom" && size5 == "tiny" && year == year[7] ~ South1.5[27],
regionOne == "HaDarom" && size5 == "tiny" && year == year[8] ~ South1.5[31],
regionOne == "HaDarom" && size5 == "tiny" && year == year[9] ~ South1.5[35],
regionOne == "HaDarom" && size5 == "tiny" && year == year[10]~ South1.5[39],
regionOne == "HaDarom" && size5 == "tiny" && year == year[11]~ South1.5[43],
##################

regionOne == "HaDarom" && size5 == "small" && year == year[1] ~ South2.5[3],
regionOne == "HaDarom" && size5 == "small" && year == year[2] ~ South2.5[7],
regionOne == "HaDarom" && size5 == "small" && year == year[3] ~ South2.5[11],
regionOne == "HaDarom" && size5 == "small" && year == year[4] ~ South2.5[15],
regionOne == "HaDarom" && size5 == "small" && year == year[5] ~ South2.5[19],
regionOne == "HaDarom" && size5 == "small" && year == year[6] ~ South2.5[23],
regionOne == "HaDarom" && size5 == "small" && year == year[7] ~ South2.5[27],
regionOne == "HaDarom" && size5 == "small" && year == year[8] ~ South2.5[31],
regionOne == "HaDarom" && size5 == "small" && year == year[9] ~ South2.5[35],
regionOne == "HaDarom" && size5 == "small" && year == year[10]~ South2.5[39],
regionOne == "HaDarom" && size5 == "small" && year == year[11]~ South2.5[43],
##################


regionOne == "HaDarom" && size5 == "medium" && year == year[1] ~ South3.5[3],
regionOne == "HaDarom" && size5 == "medium" && year == year[2] ~ South3.5[7],
regionOne == "HaDarom" && size5 == "medium" && year == year[3] ~ South3.5[11],
regionOne == "HaDarom" && size5 == "medium" && year == year[4] ~ South3.5[15],
regionOne == "HaDarom" && size5 == "medium" && year == year[5] ~ South3.5[19],
regionOne == "HaDarom" && size5 == "medium" && year == year[6] ~ South3.5[23],
regionOne == "HaDarom" && size5 == "medium" && year == year[7] ~ South3.5[27],
regionOne == "HaDarom" && size5 == "medium" && year == year[8] ~ South3.5[31],
regionOne == "HaDarom" && size5 == "medium" && year == year[9] ~ South3.5[35],
regionOne == "HaDarom" && size5 == "medium" && year == year[10]~ South3.5[39],
regionOne == "HaDarom" && size5 == "medium" && year == year[11]~ South3.5[43],
##################

regionOne == "HaDarom" && size5 == "large" && year == year[1] ~ South4.5[3],
regionOne == "HaDarom" && size5 == "large" && year == year[2] ~ South4.5[7],
regionOne == "HaDarom" && size5 == "large" && year == year[3] ~ South4.5[11],
regionOne == "HaDarom" && size5 == "large" && year == year[4] ~ South4.5[15],
regionOne == "HaDarom" && size5 == "large" && year == year[5] ~ South4.5[19],
regionOne == "HaDarom" && size5 == "large" && year == year[6] ~ South4.5[23],
regionOne == "HaDarom" && size5 == "large" && year == year[7] ~ South4.5[27],
regionOne == "HaDarom" && size5 == "large" && year == year[8] ~ South4.5[31],
regionOne == "HaDarom" && size5 == "large" && year == year[9] ~ South4.5[35],
regionOne == "HaDarom" && size5 == "large" && year == year[10]~ South4.5[39],
regionOne == "HaDarom" && size5 == "large" && year == year[11]~ South4.5[43],

##################
##################


regionOne == "Haifa" && size5 == "tiny" && year == year[1] ~ Haifa1.5[3],
regionOne == "Haifa" && size5 == "tiny" && year == year[2] ~ Haifa1.5[7],
regionOne == "Haifa" && size5 == "tiny" && year == year[3] ~ Haifa1.5[11],
regionOne == "Haifa" && size5 == "tiny" && year == year[4] ~ Haifa1.5[15],
regionOne == "Haifa" && size5 == "tiny" && year == year[5] ~ Haifa1.5[19],
regionOne == "Haifa" && size5 == "tiny" && year == year[6] ~ Haifa1.5[23],
regionOne == "Haifa" && size5 == "tiny" && year == year[7] ~ Haifa1.5[27],
regionOne == "Haifa" && size5 == "tiny" && year == year[8] ~ Haifa1.5[31],
regionOne == "Haifa" && size5 == "tiny" && year == year[9] ~ Haifa1.5[35],
regionOne == "Haifa" && size5 == "tiny" && year == year[10]~ Haifa1.5[39],
regionOne == "Haifa" && size5 == "tiny" && year == year[11]~ Haifa1.5[43],
##################

regionOne == "Haifa" && size5 == "small" && year == year[1] ~ Haifa2.5[3],
regionOne == "Haifa" && size5 == "small" && year == year[2] ~ Haifa2.5[7],
regionOne == "Haifa" && size5 == "small" && year == year[3] ~ Haifa2.5[11],
regionOne == "Haifa" && size5 == "small" && year == year[4] ~ Haifa2.5[15],
regionOne == "Haifa" && size5 == "small" && year == year[5] ~ Haifa2.5[19],
regionOne == "Haifa" && size5 == "small" && year == year[6] ~ Haifa2.5[23],
regionOne == "Haifa" && size5 == "small" && year == year[7] ~ Haifa2.5[27],
regionOne == "Haifa" && size5 == "small" && year == year[8] ~ Haifa2.5[31],
regionOne == "Haifa" && size5 == "small" && year == year[9] ~ Haifa2.5[35],
regionOne == "Haifa" && size5 == "small" && year == year[10]~ Haifa2.5[39],
regionOne == "Haifa" && size5 == "small" && year == year[11]~ Haifa2.5[43],
##################


regionOne == "Haifa" && size5 == "medium" && year == year[1] ~ Haifa3.5[3],
regionOne == "Haifa" && size5 == "medium" && year == year[2] ~ Haifa3.5[7],
regionOne == "Haifa" && size5 == "medium" && year == year[3] ~ Haifa3.5[11],
regionOne == "Haifa" && size5 == "medium" && year == year[4] ~ Haifa3.5[15],
regionOne == "Haifa" && size5 == "medium" && year == year[5] ~ Haifa3.5[19],
regionOne == "Haifa" && size5 == "medium" && year == year[6] ~ Haifa3.5[23],
regionOne == "Haifa" && size5 == "medium" && year == year[7] ~ Haifa3.5[27],
regionOne == "Haifa" && size5 == "medium" && year == year[8] ~ Haifa3.5[31],
regionOne == "Haifa" && size5 == "medium" && year == year[9] ~ Haifa3.5[35],
regionOne == "Haifa" && size5 == "medium" && year == year[10]~ Haifa3.5[39],
regionOne == "Haifa" && size5 == "medium" && year == year[11]~ Haifa3.5[43],
##################

regionOne == "Haifa" && size5 == "large" && year == year[1] ~ Haifa4.5[3],
regionOne == "Haifa" && size5 == "large" && year == year[2] ~ Haifa4.5[7],
regionOne == "Haifa" && size5 == "large" && year == year[3] ~ Haifa4.5[11],
regionOne == "Haifa" && size5 == "large" && year == year[4] ~ Haifa4.5[15],
regionOne == "Haifa" && size5 == "large" && year == year[5] ~ Haifa4.5[19],
regionOne == "Haifa" && size5 == "large" && year == year[6] ~ Haifa4.5[23],
regionOne == "Haifa" && size5 == "large" && year == year[7] ~ Haifa4.5[27],
regionOne == "Haifa" && size5 == "large" && year == year[8] ~ Haifa4.5[31],
regionOne == "Haifa" && size5 == "large" && year == year[9] ~ Haifa4.5[35],
regionOne == "Haifa" && size5 == "large" && year == year[10]~ Haifa4.5[39],
regionOne == "Haifa" && size5 == "large" && year == year[11]~ Haifa4.5[43],

##################
##################


regionOne == "HaMerkaz" && size5 == "tiny" && year == year[1] ~ centerJeruPeri1.5[3],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[2] ~ centerJeruPeri1.5[7],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[3] ~ centerJeruPeri1.5[11],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[4] ~ centerJeruPeri1.5[15],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[5] ~ centerJeruPeri1.5[19],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[6] ~ centerJeruPeri1.5[23],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[7] ~ centerJeruPeri1.5[27],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[8] ~ centerJeruPeri1.5[31],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[9] ~ centerJeruPeri1.5[35],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[10]~ centerJeruPeri1.5[39],
regionOne == "HaMerkaz" && size5 == "tiny" && year == year[11]~ centerJeruPeri1.5[43],
##################

regionOne == "HaMerkaz" && size5 == "small" && year == year[1] ~ centerJeruPeri2.5[3],
regionOne == "HaMerkaz" && size5 == "small" && year == year[2] ~ centerJeruPeri2.5[7],
regionOne == "HaMerkaz" && size5 == "small" && year == year[3] ~ centerJeruPeri2.5[11],
regionOne == "HaMerkaz" && size5 == "small" && year == year[4] ~ centerJeruPeri2.5[15],
regionOne == "HaMerkaz" && size5 == "small" && year == year[5] ~ centerJeruPeri2.5[19],
regionOne == "HaMerkaz" && size5 == "small" && year == year[6] ~ centerJeruPeri2.5[23],
regionOne == "HaMerkaz" && size5 == "small" && year == year[7] ~ centerJeruPeri2.5[27],
regionOne == "HaMerkaz" && size5 == "small" && year == year[8] ~ centerJeruPeri2.5[31],
regionOne == "HaMerkaz" && size5 == "small" && year == year[9] ~ centerJeruPeri2.5[35],
regionOne == "HaMerkaz" && size5 == "small" && year == year[10]~ centerJeruPeri2.5[39],
regionOne == "HaMerkaz" && size5 == "small" && year == year[11]~ centerJeruPeri2.5[43],
##################


regionOne == "HaMerkaz" && size5 == "medium" && year == year[1] ~ centerJeruPeri3.5[3],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[2] ~ centerJeruPeri3.5[7],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[3] ~ centerJeruPeri3.5[11],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[4] ~ centerJeruPeri3.5[15],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[5] ~ centerJeruPeri3.5[19],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[6] ~ centerJeruPeri3.5[23],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[7] ~ centerJeruPeri3.5[27],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[8] ~ centerJeruPeri3.5[31],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[9] ~ centerJeruPeri3.5[35],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[10]~ centerJeruPeri3.5[39],
regionOne == "HaMerkaz" && size5 == "medium" && year == year[11]~ centerJeruPeri3.5[43],
##################

regionOne == "HaMerkaz" && size5 == "large" && year == year[1] ~ centerJeruPeri4.5[3],
regionOne == "HaMerkaz" && size5 == "large" && year == year[2] ~ centerJeruPeri4.5[7],
regionOne == "HaMerkaz" && size5 == "large" && year == year[3] ~ centerJeruPeri4.5[11],
regionOne == "HaMerkaz" && size5 == "large" && year == year[4] ~ centerJeruPeri4.5[15],
regionOne == "HaMerkaz" && size5 == "large" && year == year[5] ~ centerJeruPeri4.5[19],
regionOne == "HaMerkaz" && size5 == "large" && year == year[6] ~ centerJeruPeri4.5[23],
regionOne == "HaMerkaz" && size5 == "large" && year == year[7] ~ centerJeruPeri4.5[27],
regionOne == "HaMerkaz" && size5 == "large" && year == year[8] ~ centerJeruPeri4.5[31],
regionOne == "HaMerkaz" && size5 == "large" && year == year[9] ~ centerJeruPeri4.5[35],
regionOne == "HaMerkaz" && size5 == "large" && year == year[10]~ centerJeruPeri4.5[39],
regionOne == "HaMerkaz" && size5 == "large" && year == year[11]~ centerJeruPeri4.5[43],

##################
##################

regionOne == "HaZafon" && size5 == "tiny" && year == year[1] ~ North1.5[3],
regionOne == "HaZafon" && size5 == "tiny" && year == year[2] ~ North1.5[7],
regionOne == "HaZafon" && size5 == "tiny" && year == year[3] ~ North1.5[11],
regionOne == "HaZafon" && size5 == "tiny" && year == year[4] ~ North1.5[15],
regionOne == "HaZafon" && size5 == "tiny" && year == year[5] ~ North1.5[19],
regionOne == "HaZafon" && size5 == "tiny" && year == year[6] ~ North1.5[23],
regionOne == "HaZafon" && size5 == "tiny" && year == year[7] ~ North1.5[27],
regionOne == "HaZafon" && size5 == "tiny" && year == year[8] ~ North1.5[31],
regionOne == "HaZafon" && size5 == "tiny" && year == year[9] ~ North1.5[35],
regionOne == "HaZafon" && size5 == "tiny" && year == year[10]~ North1.5[39],
regionOne == "HaZafon" && size5 == "tiny" && year == year[11]~ North1.5[43],
##################

regionOne == "HaZafon" && size5 == "small" && year == year[1] ~ North2.5[3],
regionOne == "HaZafon" && size5 == "small" && year == year[2] ~ North2.5[7],
regionOne == "HaZafon" && size5 == "small" && year == year[3] ~ North2.5[11],
regionOne == "HaZafon" && size5 == "small" && year == year[4] ~ North2.5[15],
regionOne == "HaZafon" && size5 == "small" && year == year[5] ~ North2.5[19],
regionOne == "HaZafon" && size5 == "small" && year == year[6] ~ North2.5[23],
regionOne == "HaZafon" && size5 == "small" && year == year[7] ~ North2.5[27],
regionOne == "HaZafon" && size5 == "small" && year == year[8] ~ North2.5[31],
regionOne == "HaZafon" && size5 == "small" && year == year[9] ~ North2.5[35],
regionOne == "HaZafon" && size5 == "small" && year == year[10]~ North2.5[39],
regionOne == "HaZafon" && size5 == "small" && year == year[11]~ North2.5[43],
##################


regionOne == "HaZafon" && size5 == "medium" && year == year[1] ~ North3.5[3],
regionOne == "HaZafon" && size5 == "medium" && year == year[2] ~ North3.5[7],
regionOne == "HaZafon" && size5 == "medium" && year == year[3] ~ North3.5[11],
regionOne == "HaZafon" && size5 == "medium" && year == year[4] ~ North3.5[15],
regionOne == "HaZafon" && size5 == "medium" && year == year[5] ~ North3.5[19],
regionOne == "HaZafon" && size5 == "medium" && year == year[6] ~ North3.5[23],
regionOne == "HaZafon" && size5 == "medium" && year == year[7] ~ North3.5[27],
regionOne == "HaZafon" && size5 == "medium" && year == year[8] ~ North3.5[31],
regionOne == "HaZafon" && size5 == "medium" && year == year[9] ~ North3.5[35],
regionOne == "HaZafon" && size5 == "medium" && year == year[10]~ North3.5[39],
regionOne == "HaZafon" && size5 == "medium" && year == year[11]~ North3.5[43],
##################

regionOne == "HaZafon" && size5 == "large" && year == year[1] ~ North4.5[3],
regionOne == "HaZafon" && size5 == "large" && year == year[2] ~ North4.5[7],
regionOne == "HaZafon" && size5 == "large" && year == year[3] ~ North4.5[11],
regionOne == "HaZafon" && size5 == "large" && year == year[4] ~ North4.5[15],
regionOne == "HaZafon" && size5 == "large" && year == year[5] ~ North4.5[19],
regionOne == "HaZafon" && size5 == "large" && year == year[6] ~ North4.5[23],
regionOne == "HaZafon" && size5 == "large" && year == year[7] ~ North4.5[27],
regionOne == "HaZafon" && size5 == "large" && year == year[8] ~ North4.5[31],
regionOne == "HaZafon" && size5 == "large" && year == year[9] ~ North4.5[35],
regionOne == "HaZafon" && size5 == "large" && year == year[10]~ North4.5[39],
regionOne == "HaZafon" && size5 == "large" && year == year[11]~ North4.5[43],

##################
##################
regionOne == "Sharon" && size5 == "tiny" && year == year[1] ~ Sharon1.5[3],
regionOne == "Sharon" && size5 == "tiny" && year == year[2] ~ Sharon1.5[7],
regionOne == "Sharon" && size5 == "tiny" && year == year[3] ~ Sharon1.5[11],
regionOne == "Sharon" && size5 == "tiny" && year == year[4] ~ Sharon1.5[15],
regionOne == "Sharon" && size5 == "tiny" && year == year[5] ~ Sharon1.5[19],
regionOne == "Sharon" && size5 == "tiny" && year == year[6] ~ Sharon1.5[23],
regionOne == "Sharon" && size5 == "tiny" && year == year[7] ~ Sharon1.5[27],
regionOne == "Sharon" && size5 == "tiny" && year == year[8] ~ Sharon1.5[31],
regionOne == "Sharon" && size5 == "tiny" && year == year[9] ~ Sharon1.5[35],
regionOne == "Sharon" && size5 == "tiny" && year == year[10]~ Sharon1.5[39],
regionOne == "Sharon" && size5 == "tiny" && year == year[11]~ Sharon1.5[43],
##################

regionOne == "Sharon" && size5 == "small" && year == year[1] ~ Sharon2.5[3],
regionOne == "Sharon" && size5 == "small" && year == year[2] ~ Sharon2.5[7],
regionOne == "Sharon" && size5 == "small" && year == year[3] ~ Sharon2.5[11],
regionOne == "Sharon" && size5 == "small" && year == year[4] ~ Sharon2.5[15],
regionOne == "Sharon" && size5 == "small" && year == year[5] ~ Sharon2.5[19],
regionOne == "Sharon" && size5 == "small" && year == year[6] ~ Sharon2.5[23],
regionOne == "Sharon" && size5 == "small" && year == year[7] ~ Sharon2.5[27],
regionOne == "Sharon" && size5 == "small" && year == year[8] ~ Sharon2.5[31],
regionOne == "Sharon" && size5 == "small" && year == year[9] ~ Sharon2.5[35],
regionOne == "Sharon" && size5 == "small" && year == year[10]~ Sharon2.5[39],
regionOne == "Sharon" && size5 == "small" && year == year[11]~ Sharon2.5[43],
##################


regionOne == "Sharon" && size5 == "medium" && year == year[1] ~ Sharon3.5[3],
regionOne == "Sharon" && size5 == "medium" && year == year[2] ~ Sharon3.5[7],
regionOne == "Sharon" && size5 == "medium" && year == year[3] ~ Sharon3.5[11],
regionOne == "Sharon" && size5 == "medium" && year == year[4] ~ Sharon3.5[15],
regionOne == "Sharon" && size5 == "medium" && year == year[5] ~ Sharon3.5[19],
regionOne == "Sharon" && size5 == "medium" && year == year[6] ~ Sharon3.5[23],
regionOne == "Sharon" && size5 == "medium" && year == year[7] ~ Sharon3.5[27],
regionOne == "Sharon" && size5 == "medium" && year == year[8] ~ Sharon3.5[31],
regionOne == "Sharon" && size5 == "medium" && year == year[9] ~ Sharon3.5[35],
regionOne == "Sharon" && size5 == "medium" && year == year[10]~ Sharon3.5[39],
regionOne == "Sharon" && size5 == "medium" && year == year[11]~ Sharon3.5[43],
##################

regionOne == "Sharon" && size5 == "large" && year == year[1] ~ Sharon4.5[3],
regionOne == "Sharon" && size5 == "large" && year == year[2] ~ Sharon4.5[7],
regionOne == "Sharon" && size5 == "large" && year == year[3] ~ Sharon4.5[11],
regionOne == "Sharon" && size5 == "large" && year == year[4] ~ Sharon4.5[15],
regionOne == "Sharon" && size5 == "large" && year == year[5] ~ Sharon4.5[19],
regionOne == "Sharon" && size5 == "large" && year == year[6] ~ Sharon4.5[23],
regionOne == "Sharon" && size5 == "large" && year == year[7] ~ Sharon4.5[27],
regionOne == "Sharon" && size5 == "large" && year == year[8] ~ Sharon4.5[31],
regionOne == "Sharon" && size5 == "large" && year == year[9] ~ Sharon4.5[35],
regionOne == "Sharon" && size5 == "large" && year == year[10]~ Sharon4.5[39],
regionOne == "Sharon" && size5 == "large" && year == year[11]~ Sharon4.5[43],

##################
##################


regionOne == "Tel Aviv" && size5 == "tiny" && year == year[1] ~ tlv1.5[3],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[2] ~ tlv1.5[7],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[3] ~ tlv1.5[11],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[4] ~ tlv1.5[15],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[5] ~ tlv1.5[19],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[6] ~ tlv1.5[23],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[7] ~ tlv1.5[27],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[8] ~ tlv1.5[31],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[9] ~ tlv1.5[35],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[10]~ tlv1.5[39],
regionOne == "Tel Aviv" && size5 == "tiny" && year == year[11]~ tlv1.5[43],
##################

regionOne == "Tel Aviv" && size5 == "small" && year == year[1] ~ tlv2.5[3],
regionOne == "Tel Aviv" && size5 == "small" && year == year[2] ~ tlv2.5[7],
regionOne == "Tel Aviv" && size5 == "small" && year == year[3] ~ tlv2.5[11],
regionOne == "Tel Aviv" && size5 == "small" && year == year[4] ~ tlv2.5[15],
regionOne == "Tel Aviv" && size5 == "small" && year == year[5] ~ tlv2.5[19],
regionOne == "Tel Aviv" && size5 == "small" && year == year[6] ~ tlv2.5[23],
regionOne == "Tel Aviv" && size5 == "small" && year == year[7] ~ tlv2.5[27],
regionOne == "Tel Aviv" && size5 == "small" && year == year[8] ~ tlv2.5[31],
regionOne == "Tel Aviv" && size5 == "small" && year == year[9] ~ tlv2.5[35],
regionOne == "Tel Aviv" && size5 == "small" && year == year[10]~ tlv2.5[39],
regionOne == "Tel Aviv" && size5 == "small" && year == year[11]~ tlv2.5[43],
##################

regionOne == "Tel Aviv" && size5 == "medium" && year == year[1] ~ tlv3.5[3],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[2] ~ tlv3.5[7],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[3] ~ tlv3.5[11],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[4] ~ tlv3.5[15],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[5] ~ tlv3.5[19],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[6] ~ tlv3.5[23],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[7] ~ tlv3.5[27],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[8] ~ tlv3.5[31],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[9] ~ tlv3.5[35],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[10]~ tlv3.5[39],
regionOne == "Tel Aviv" && size5 == "medium" && year == year[11]~ tlv3.5[43],
##################

regionOne == "Tel Aviv" && size5 == "large" && year == year[1] ~ tlv4.5[3],
regionOne == "Tel Aviv" && size5 == "large" && year == year[2] ~ tlv4.5[7],
regionOne == "Tel Aviv" && size5 == "large" && year == year[3] ~ tlv4.5[11],
regionOne == "Tel Aviv" && size5 == "large" && year == year[4] ~ tlv4.5[15],
regionOne == "Tel Aviv" && size5 == "large" && year == year[5] ~ tlv4.5[19],
regionOne == "Tel Aviv" && size5 == "large" && year == year[6] ~ tlv4.5[23],
regionOne == "Tel Aviv" && size5 == "large" && year == year[7] ~ tlv4.5[27],
regionOne == "Tel Aviv" && size5 == "large" && year == year[8] ~ tlv4.5[31],
regionOne == "Tel Aviv" && size5 == "large" && year == year[9] ~ tlv4.5[35],
regionOne == "Tel Aviv" && size5 == "large" && year == year[10]~ tlv4.5[39],
regionOne == "Tel Aviv" && size5 == "large" && year == year[11]~ tlv4.5[43],
TRUE                                                           ~ 1899.34           ## catch undefined region
)
)
   return("Home prices added by city/size/year to each row.")
  } else if (frame == "famYYYY") {

fam2004 <- fam2004 %>%
  mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region
))

fam2005 <- fam2005 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2006 <- fam2006 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2007 <- fam2007 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],


TRUE                   ~ 1899.34           ## catch undefined region
))
fam2008 <- fam2008 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2009 <- fam2009 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2010 <- fam2010 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2011 <- fam2011 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2012 <- fam2012 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region
))
fam2013 <- fam2013 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],
TRUE                   ~ 1899.34           ## catch undefined region
))
fam2014 <- fam2014 %>%
   mutate(houseP = case_when(

regionOne == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionOne == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionOne == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionOne == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionOne == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionOne == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionOne == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionOne == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionOne == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionOne == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionOne == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionOne == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionOne == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionOne == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionOne == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionOne == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionOne == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionOne == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionOne == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionOne == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionOne == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],
TRUE                   ~ 1899.34           ## catch undefined region
))

    ## now add $housePTwo  based on regionTwo
fam2004 <- fam2004 %>%
  mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region
))

fam2005 <- fam2005 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2006 <- fam2006 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2007 <- fam2007 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],


TRUE                   ~ 1899.34           ## catch undefined region
))
fam2008 <- fam2008 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2009 <- fam2009 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2010 <- fam2010 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2011 <- fam2011 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region

))
fam2012 <- fam2012 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],

TRUE                   ~ 1899.34           ## catch undefined region
))
fam2013 <- fam2013 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],
TRUE                   ~ 1899.34           ## catch undefined region
))
fam2014 <- fam2014 %>%
   mutate(housePTwo = case_when(

regionTwo == "Jerusalem" && size == "tiny" ~ Jerusalem1.5[3, 2],
regionTwo == "Jerusalem" && size == "small" ~ Jerusalem2.5[3, 2],
regionTwo == "Jerusalem" && size == "medium" ~ Jerusalem3.5[3, 2],
regionTwo == "Jerusalem" && size == "large" ~ Jerusalem4.5[3, 2],

regionTwo == "Gush Dan" && size == "tiny" ~ GushDan1.5[3, 2],
regionTwo == "Gush Dan" && size == "small" ~ GushDan2.5[3, 2],
regionTwo == "Gush Dan" && size == "medium" ~ GushDan3.5[3, 2],
regionTwo == "Gush Dan" && size == "large" ~ GushDan4.5[3, 2],

regionTwo == "HaDarom" && size == "tiny" ~ South1.5[3, 2],
regionTwo == "HaDarom" && size == "small" ~ South2.5[3, 2],
regionTwo == "HaDarom" && size == "medium" ~ South3.5[3, 2],
regionTwo == "HaDarom" && size == "large" ~ South4.5[3, 2],

regionTwo == "Haifa" && size == "tiny" ~ Haifa1.5[3, 2],
regionTwo == "Haifa" && size == "small" ~ Haifa2.5[3, 2],
regionTwo == "Haifa" && size == "medium" ~ Haifa3.5[3, 2],
regionTwo == "Haifa" && size == "large" ~ Haifa4.5[3, 2],

regionTwo == "HaMerkaz" && size == "tiny" ~ centerJeruPeri1.5[3, 2],
regionTwo == "HaMerkaz" && size == "small" ~ centerJeruPeri2.5[3, 2],
regionTwo == "HaMerkaz" && size == "medium" ~ centerJeruPeri3.5[3, 2],
regionTwo == "HaMerkaz" && size == "large" ~ centerJeruPeri4.5[3, 2],

regionTwo == "HaZafon" && size == "tiny" ~ North1.5[3, 2],
regionTwo == "HaZafon" && size == "small" ~ North2.5[3, 2],
regionTwo == "HaZafon" && size == "medium" ~ North3.5[3, 2],
regionTwo == "HaZafon" && size == "large" ~ North4.5[3, 2],

regionTwo == "Sharon" && size == "tiny" ~ Sharon1.5[3, 2],
regionTwo == "Sharon" && size == "small" ~ Sharon2.5[3, 2],
regionTwo == "Sharon" && size == "medium" ~ Sharon3.5[3, 2],
regionTwo == "Sharon" && size == "large" ~ Sharon4.5[3, 2],

regionTwo == "Tel Aviv" && size == "tiny" ~ tlv1.5[3, 2],
regionTwo == "Tel Aviv" && size == "small" ~ tlv2.5[3, 2],
regionTwo == "Tel Aviv" && size == "medium" ~ tlv3.5[3, 2],
regionTwo == "Tel Aviv" && size == "large" ~ tlv4.5[3, 2],
TRUE                   ~ 1899.34           ## catch undefined region
))


return("Added $houseP and $housePTwo columns to famYYYY.")
  } else if (frame == "renters") {
    ## add column
    ## set initial value
    Rent5$houseP = as.factor("foo")
Rent7 <<-
     Rent5 %>%
     mutate(houseP == case_when(

regionOne == "Jerusalem" && size == "tiny" && year == year[1] ~ Jerusalem1.5[3, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[2] ~ Jerusalem1.5[7, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[3] ~ Jerusalem1.5[11, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[4] ~ Jerusalem1.5[15, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[5] ~ Jerusalem1.5[19, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[6] ~ Jerusalem1.5[23, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[7] ~ Jerusalem1.5[27, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[8] ~ Jerusalem1.5[31, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[9] ~ Jerusalem1.5[35, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[10]~ Jerusalem1.5[39, 2],
regionOne == "Jerusalem" && size == "tiny" && year == year[11]~ Jerusalem1.5[43, 2],
##################

regionOne == "Jerusalem" && size == "small" && year == year[1] ~ Jerusalem2.5[3, 2],
regionOne == "Jerusalem" && size == "small" && year == year[2] ~ Jerusalem2.5[7, 2],
regionOne == "Jerusalem" && size == "small" && year == year[3] ~ Jerusalem2.5[11, 2],
regionOne == "Jerusalem" && size == "small" && year == year[4] ~ Jerusalem2.5[15, 2],
regionOne == "Jerusalem" && size == "small" && year == year[5] ~ Jerusalem2.5[19, 2],
regionOne == "Jerusalem" && size == "small" && year == year[6] ~ Jerusalem2.5[23, 2],
regionOne == "Jerusalem" && size == "small" && year == year[7] ~ Jerusalem2.5[27, 2],
regionOne == "Jerusalem" && size == "small" && year == year[8] ~ Jerusalem2.5[31, 2],
regionOne == "Jerusalem" && size == "small" && year == year[9] ~ Jerusalem2.5[35, 2],
regionOne == "Jerusalem" && size == "small" && year == year[10]~ Jerusalem2.5[39, 2],
regionOne == "Jerusalem" && size == "small" && year == year[11]~ Jerusalem2.5[43, 2],
##################


regionOne == "Jerusalem" && size == "medium" && year == year[1] ~ Jerusalem3.5[3, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[2] ~ Jerusalem3.5[7, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[3] ~ Jerusalem3.5[11, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[4] ~ Jerusalem3.5[15, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[5] ~ Jerusalem3.5[19, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[6] ~ Jerusalem3.5[23, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[7] ~ Jerusalem3.5[27, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[8] ~ Jerusalem3.5[31, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[9] ~ Jerusalem3.5[35, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[10]~ Jerusalem3.5[39, 2],
regionOne == "Jerusalem" && size == "medium" && year == year[11]~ Jerusalem3.5[43, 2],
##################

regionOne == "Jerusalem" && size == "large" && year == year[1] ~ Jerusalem4.5[3, 2],
regionOne == "Jerusalem" && size == "large" && year == year[2] ~ Jerusalem4.5[7, 2],
regionOne == "Jerusalem" && size == "large" && year == year[3] ~ Jerusalem4.5[11, 2],
regionOne == "Jerusalem" && size == "large" && year == year[4] ~ Jerusalem4.5[15, 2],
regionOne == "Jerusalem" && size == "large" && year == year[5] ~ Jerusalem4.5[19, 2],
regionOne == "Jerusalem" && size == "large" && year == year[6] ~ Jerusalem4.5[23, 2],
regionOne == "Jerusalem" && size == "large" && year == year[7] ~ Jerusalem4.5[27, 2],
regionOne == "Jerusalem" && size == "large" && year == year[8] ~ Jerusalem4.5[31, 2],
regionOne == "Jerusalem" && size == "large" && year == year[9] ~ Jerusalem4.5[35, 2],
regionOne == "Jerusalem" && size == "large" && year == year[10]~ Jerusalem4.5[39, 2],
regionOne == "Jerusalem" && size == "large" && year == year[11]~ Jerusalem4.5[43, 2],

##################
##################

regionOne == "Gush Dan" && size == "tiny" && year == year[1] ~ GushDan1.5[3, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[2] ~ GushDan1.5[7, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[3] ~ GushDan1.5[11, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[4] ~ GushDan1.5[15, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[5] ~ GushDan1.5[19, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[6] ~ GushDan1.5[23, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[7] ~ GushDan1.5[27, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[8] ~ GushDan1.5[31, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[9] ~ GushDan1.5[35, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[10]~ GushDan1.5[39, 2],
regionOne == "Gush Dan" && size == "tiny" && year == year[11]~ GushDan1.5[43, 2],
##################

regionOne == "Gush Dan" && size == "small" && year == year[1] ~ GushDan2.5[3, 2],
regionOne == "Gush Dan" && size == "small" && year == year[2] ~ GushDan2.5[7, 2],
regionOne == "Gush Dan" && size == "small" && year == year[3] ~ GushDan2.5[11, 2],
regionOne == "Gush Dan" && size == "small" && year == year[4] ~ GushDan2.5[15, 2],
regionOne == "Gush Dan" && size == "small" && year == year[5] ~ GushDan2.5[19, 2],
regionOne == "Gush Dan" && size == "small" && year == year[6] ~ GushDan2.5[23, 2],
regionOne == "Gush Dan" && size == "small" && year == year[7] ~ GushDan2.5[27, 2],
regionOne == "Gush Dan" && size == "small" && year == year[8] ~ GushDan2.5[31, 2],
regionOne == "Gush Dan" && size == "small" && year == year[9] ~ GushDan2.5[35, 2],
regionOne == "Gush Dan" && size == "small" && year == year[10]~ GushDan2.5[39, 2],
regionOne == "Gush Dan" && size == "small" && year == year[11]~ GushDan2.5[43, 2],
##################


regionOne == "Gush Dan" && size == "medium" && year == year[1] ~ GushDan3.5[3, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[2] ~ GushDan3.5[7, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[3] ~ GushDan3.5[11, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[4] ~ GushDan3.5[15, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[5] ~ GushDan3.5[19, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[6] ~ GushDan3.5[23, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[7] ~ GushDan3.5[27, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[8] ~ GushDan3.5[31, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[9] ~ GushDan3.5[35, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[10]~ GushDan3.5[39, 2],
regionOne == "Gush Dan" && size == "medium" && year == year[11]~ GushDan3.5[43, 2],
##################

regionOne == "Gush Dan" && size == "large" && year == year[1] ~ GushDan4.5[3, 2],
regionOne == "Gush Dan" && size == "large" && year == year[2] ~ GushDan4.5[7, 2],
regionOne == "Gush Dan" && size == "large" && year == year[3] ~ GushDan4.5[11, 2],
regionOne == "Gush Dan" && size == "large" && year == year[4] ~ GushDan4.5[15, 2],
regionOne == "Gush Dan" && size == "large" && year == year[5] ~ GushDan4.5[19, 2],
regionOne == "Gush Dan" && size == "large" && year == year[6] ~ GushDan4.5[23, 2],
regionOne == "Gush Dan" && size == "large" && year == year[7] ~ GushDan4.5[27, 2],
regionOne == "Gush Dan" && size == "large" && year == year[8] ~ GushDan4.5[31, 2],
regionOne == "Gush Dan" && size == "large" && year == year[9] ~ GushDan4.5[35, 2],
regionOne == "Gush Dan" && size == "large" && year == year[10]~ GushDan4.5[39, 2],
regionOne == "Gush Dan" && size == "large" && year == year[11]~ GushDan4.5[43, 2],

##################
##################


regionOne == "HaDarom" && size == "tiny" && year == year[1] ~ South1.5[3, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[2] ~ South1.5[7, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[3] ~ South1.5[11, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[4] ~ South1.5[15, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[5] ~ South1.5[19, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[6] ~ South1.5[23, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[7] ~ South1.5[27, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[8] ~ South1.5[31, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[9] ~ South1.5[35, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[10]~ South1.5[39, 2],
regionOne == "HaDarom" && size == "tiny" && year == year[11]~ South1.5[43, 2],
##################

regionOne == "HaDarom" && size == "small" && year == year[1] ~ South2.5[3, 2],
regionOne == "HaDarom" && size == "small" && year == year[2] ~ South2.5[7, 2],
regionOne == "HaDarom" && size == "small" && year == year[3] ~ South2.5[11, 2],
regionOne == "HaDarom" && size == "small" && year == year[4] ~ South2.5[15, 2],
regionOne == "HaDarom" && size == "small" && year == year[5] ~ South2.5[19, 2],
regionOne == "HaDarom" && size == "small" && year == year[6] ~ South2.5[23, 2],
regionOne == "HaDarom" && size == "small" && year == year[7] ~ South2.5[27, 2],
regionOne == "HaDarom" && size == "small" && year == year[8] ~ South2.5[31, 2],
regionOne == "HaDarom" && size == "small" && year == year[9] ~ South2.5[35, 2],
regionOne == "HaDarom" && size == "small" && year == year[10]~ South2.5[39, 2],
regionOne == "HaDarom" && size == "small" && year == year[11]~ South2.5[43, 2],
##################


regionOne == "HaDarom" && size == "medium" && year == year[1] ~ South3.5[3, 2],
regionOne == "HaDarom" && size == "medium" && year == year[2] ~ South3.5[7, 2],
regionOne == "HaDarom" && size == "medium" && year == year[3] ~ South3.5[11, 2],
regionOne == "HaDarom" && size == "medium" && year == year[4] ~ South3.5[15, 2],
regionOne == "HaDarom" && size == "medium" && year == year[5] ~ South3.5[19, 2],
regionOne == "HaDarom" && size == "medium" && year == year[6] ~ South3.5[23, 2],
regionOne == "HaDarom" && size == "medium" && year == year[7] ~ South3.5[27, 2],
regionOne == "HaDarom" && size == "medium" && year == year[8] ~ South3.5[31, 2],
regionOne == "HaDarom" && size == "medium" && year == year[9] ~ South3.5[35, 2],
regionOne == "HaDarom" && size == "medium" && year == year[10]~ South3.5[39, 2],
regionOne == "HaDarom" && size == "medium" && year == year[11]~ South3.5[43, 2],
##################

regionOne == "HaDarom" && size == "large" && year == year[1] ~ South4.5[3, 2],
regionOne == "HaDarom" && size == "large" && year == year[2] ~ South4.5[7, 2],
regionOne == "HaDarom" && size == "large" && year == year[3] ~ South4.5[11, 2],
regionOne == "HaDarom" && size == "large" && year == year[4] ~ South4.5[15, 2],
regionOne == "HaDarom" && size == "large" && year == year[5] ~ South4.5[19, 2],
regionOne == "HaDarom" && size == "large" && year == year[6] ~ South4.5[23, 2],
regionOne == "HaDarom" && size == "large" && year == year[7] ~ South4.5[27, 2],
regionOne == "HaDarom" && size == "large" && year == year[8] ~ South4.5[31, 2],
regionOne == "HaDarom" && size == "large" && year == year[9] ~ South4.5[35, 2],
regionOne == "HaDarom" && size == "large" && year == year[10]~ South4.5[39, 2],
regionOne == "HaDarom" && size == "large" && year == year[11]~ South4.5[43, 2],

##################
##################


regionOne == "Haifa" && size == "tiny" && year == year[1] ~ Haifa1.5[3, 2],
regionOne == "Haifa" && size == "tiny" && year == year[2] ~ Haifa1.5[7, 2],
regionOne == "Haifa" && size == "tiny" && year == year[3] ~ Haifa1.5[11, 2],
regionOne == "Haifa" && size == "tiny" && year == year[4] ~ Haifa1.5[15, 2],
regionOne == "Haifa" && size == "tiny" && year == year[5] ~ Haifa1.5[19, 2],
regionOne == "Haifa" && size == "tiny" && year == year[6] ~ Haifa1.5[23, 2],
regionOne == "Haifa" && size == "tiny" && year == year[7] ~ Haifa1.5[27, 2],
regionOne == "Haifa" && size == "tiny" && year == year[8] ~ Haifa1.5[31, 2],
regionOne == "Haifa" && size == "tiny" && year == year[9] ~ Haifa1.5[35, 2],
regionOne == "Haifa" && size == "tiny" && year == year[10]~ Haifa1.5[39, 2],
regionOne == "Haifa" && size == "tiny" && year == year[11]~ Haifa1.5[43, 2],
##################

regionOne == "Haifa" && size == "small" && year == year[1] ~ Haifa2.5[3, 2],
regionOne == "Haifa" && size == "small" && year == year[2] ~ Haifa2.5[7, 2],
regionOne == "Haifa" && size == "small" && year == year[3] ~ Haifa2.5[11, 2],
regionOne == "Haifa" && size == "small" && year == year[4] ~ Haifa2.5[15, 2],
regionOne == "Haifa" && size == "small" && year == year[5] ~ Haifa2.5[19, 2],
regionOne == "Haifa" && size == "small" && year == year[6] ~ Haifa2.5[23, 2],
regionOne == "Haifa" && size == "small" && year == year[7] ~ Haifa2.5[27, 2],
regionOne == "Haifa" && size == "small" && year == year[8] ~ Haifa2.5[31, 2],
regionOne == "Haifa" && size == "small" && year == year[9] ~ Haifa2.5[35, 2],
regionOne == "Haifa" && size == "small" && year == year[10]~ Haifa2.5[39, 2],
regionOne == "Haifa" && size == "small" && year == year[11]~ Haifa2.5[43, 2],
##################


regionOne == "Haifa" && size == "medium" && year == year[1] ~ Haifa3.5[3],
regionOne == "Haifa" && size == "medium" && year == year[2] ~ Haifa3.5[7],
regionOne == "Haifa" && size == "medium" && year == year[3] ~ Haifa3.5[11],
regionOne == "Haifa" && size == "medium" && year == year[4] ~ Haifa3.5[15],
regionOne == "Haifa" && size == "medium" && year == year[5] ~ Haifa3.5[19],
regionOne == "Haifa" && size == "medium" && year == year[6] ~ Haifa3.5[23],
regionOne == "Haifa" && size == "medium" && year == year[7] ~ Haifa3.5[27],
regionOne == "Haifa" && size == "medium" && year == year[8] ~ Haifa3.5[31],
regionOne == "Haifa" && size == "medium" && year == year[9] ~ Haifa3.5[35],
regionOne == "Haifa" && size == "medium" && year == year[10]~ Haifa3.5[39],
regionOne == "Haifa" && size == "medium" && year == year[11]~ Haifa3.5[43],
##################

regionOne == "Haifa" && size == "large" && year == year[1] ~ Haifa4.5[3],
regionOne == "Haifa" && size == "large" && year == year[2] ~ Haifa4.5[7],
regionOne == "Haifa" && size == "large" && year == year[3] ~ Haifa4.5[11],
regionOne == "Haifa" && size == "large" && year == year[4] ~ Haifa4.5[15],
regionOne == "Haifa" && size == "large" && year == year[5] ~ Haifa4.5[19],
regionOne == "Haifa" && size == "large" && year == year[6] ~ Haifa4.5[23],
regionOne == "Haifa" && size == "large" && year == year[7] ~ Haifa4.5[27],
regionOne == "Haifa" && size == "large" && year == year[8] ~ Haifa4.5[31],
regionOne == "Haifa" && size == "large" && year == year[9] ~ Haifa4.5[35],
regionOne == "Haifa" && size == "large" && year == year[10]~ Haifa4.5[39],
regionOne == "Haifa" && size == "large" && year == year[11]~ Haifa4.5[43],

##################
##################


regionOne == "HaMerkaz" && size == "tiny" && year == year[1] ~ centerJeruPeri1.5[3],
regionOne == "HaMerkaz" && size == "tiny" && year == year[2] ~ centerJeruPeri1.5[7],
regionOne == "HaMerkaz" && size == "tiny" && year == year[3] ~ centerJeruPeri1.5[11],
regionOne == "HaMerkaz" && size == "tiny" && year == year[4] ~ centerJeruPeri1.5[15],
regionOne == "HaMerkaz" && size == "tiny" && year == year[5] ~ centerJeruPeri1.5[19],
regionOne == "HaMerkaz" && size == "tiny" && year == year[6] ~ centerJeruPeri1.5[23],
regionOne == "HaMerkaz" && size == "tiny" && year == year[7] ~ centerJeruPeri1.5[27],
regionOne == "HaMerkaz" && size == "tiny" && year == year[8] ~ centerJeruPeri1.5[31],
regionOne == "HaMerkaz" && size == "tiny" && year == year[9] ~ centerJeruPeri1.5[35],
regionOne == "HaMerkaz" && size == "tiny" && year == year[10]~ centerJeruPeri1.5[39],
regionOne == "HaMerkaz" && size == "tiny" && year == year[11]~ centerJeruPeri1.5[43],
##################

regionOne == "HaMerkaz" && size == "small" && year == year[1] ~ centerJeruPeri2.5[3],
regionOne == "HaMerkaz" && size == "small" && year == year[2] ~ centerJeruPeri2.5[7],
regionOne == "HaMerkaz" && size == "small" && year == year[3] ~ centerJeruPeri2.5[11],
regionOne == "HaMerkaz" && size == "small" && year == year[4] ~ centerJeruPeri2.5[15],
regionOne == "HaMerkaz" && size == "small" && year == year[5] ~ centerJeruPeri2.5[19],
regionOne == "HaMerkaz" && size == "small" && year == year[6] ~ centerJeruPeri2.5[23],
regionOne == "HaMerkaz" && size == "small" && year == year[7] ~ centerJeruPeri2.5[27],
regionOne == "HaMerkaz" && size == "small" && year == year[8] ~ centerJeruPeri2.5[31],
regionOne == "HaMerkaz" && size == "small" && year == year[9] ~ centerJeruPeri2.5[35],
regionOne == "HaMerkaz" && size == "small" && year == year[10]~ centerJeruPeri2.5[39],
regionOne == "HaMerkaz" && size == "small" && year == year[11]~ centerJeruPeri2.5[43],
##################


regionOne == "HaMerkaz" && size == "medium" && year == year[1] ~ centerJeruPeri3.5[3],
regionOne == "HaMerkaz" && size == "medium" && year == year[2] ~ centerJeruPeri3.5[7],
regionOne == "HaMerkaz" && size == "medium" && year == year[3] ~ centerJeruPeri3.5[11],
regionOne == "HaMerkaz" && size == "medium" && year == year[4] ~ centerJeruPeri3.5[15],
regionOne == "HaMerkaz" && size == "medium" && year == year[5] ~ centerJeruPeri3.5[19],
regionOne == "HaMerkaz" && size == "medium" && year == year[6] ~ centerJeruPeri3.5[23],
regionOne == "HaMerkaz" && size == "medium" && year == year[7] ~ centerJeruPeri3.5[27],
regionOne == "HaMerkaz" && size == "medium" && year == year[8] ~ centerJeruPeri3.5[31],
regionOne == "HaMerkaz" && size == "medium" && year == year[9] ~ centerJeruPeri3.5[35],
regionOne == "HaMerkaz" && size == "medium" && year == year[10]~ centerJeruPeri3.5[39],
regionOne == "HaMerkaz" && size == "medium" && year == year[11]~ centerJeruPeri3.5[43],
##################

regionOne == "HaMerkaz" && size == "large" && year == year[1] ~ centerJeruPeri4.5[3],
regionOne == "HaMerkaz" && size == "large" && year == year[2] ~ centerJeruPeri4.5[7],
regionOne == "HaMerkaz" && size == "large" && year == year[3] ~ centerJeruPeri4.5[11],
regionOne == "HaMerkaz" && size == "large" && year == year[4] ~ centerJeruPeri4.5[15],
regionOne == "HaMerkaz" && size == "large" && year == year[5] ~ centerJeruPeri4.5[19],
regionOne == "HaMerkaz" && size == "large" && year == year[6] ~ centerJeruPeri4.5[23],
regionOne == "HaMerkaz" && size == "large" && year == year[7] ~ centerJeruPeri4.5[27],
regionOne == "HaMerkaz" && size == "large" && year == year[8] ~ centerJeruPeri4.5[31],
regionOne == "HaMerkaz" && size == "large" && year == year[9] ~ centerJeruPeri4.5[35],
regionOne == "HaMerkaz" && size == "large" && year == year[10]~ centerJeruPeri4.5[39],
regionOne == "HaMerkaz" && size == "large" && year == year[11]~ centerJeruPeri4.5[43],

##################
##################

regionOne == "HaZafon" && size == "tiny" && year == year[1] ~ North1.5[3],
regionOne == "HaZafon" && size == "tiny" && year == year[2] ~ North1.5[7],
regionOne == "HaZafon" && size == "tiny" && year == year[3] ~ North1.5[11],
regionOne == "HaZafon" && size == "tiny" && year == year[4] ~ North1.5[15],
regionOne == "HaZafon" && size == "tiny" && year == year[5] ~ North1.5[19],
regionOne == "HaZafon" && size == "tiny" && year == year[6] ~ North1.5[23],
regionOne == "HaZafon" && size == "tiny" && year == year[7] ~ North1.5[27],
regionOne == "HaZafon" && size == "tiny" && year == year[8] ~ North1.5[31],
regionOne == "HaZafon" && size == "tiny" && year == year[9] ~ North1.5[35],
regionOne == "HaZafon" && size == "tiny" && year == year[10]~ North1.5[39],
regionOne == "HaZafon" && size == "tiny" && year == year[11]~ North1.5[43],
##################

regionOne == "HaZafon" && size == "small" && year == year[1] ~ North2.5[3],
regionOne == "HaZafon" && size == "small" && year == year[2] ~ North2.5[7],
regionOne == "HaZafon" && size == "small" && year == year[3] ~ North2.5[11],
regionOne == "HaZafon" && size == "small" && year == year[4] ~ North2.5[15],
regionOne == "HaZafon" && size == "small" && year == year[5] ~ North2.5[19],
regionOne == "HaZafon" && size == "small" && year == year[6] ~ North2.5[23],
regionOne == "HaZafon" && size == "small" && year == year[7] ~ North2.5[27],
regionOne == "HaZafon" && size == "small" && year == year[8] ~ North2.5[31],
regionOne == "HaZafon" && size == "small" && year == year[9] ~ North2.5[35],
regionOne == "HaZafon" && size == "small" && year == year[10]~ North2.5[39],
regionOne == "HaZafon" && size == "small" && year == year[11]~ North2.5[43],
##################


regionOne == "HaZafon" && size == "medium" && year == year[1] ~ North3.5[3],
regionOne == "HaZafon" && size == "medium" && year == year[2] ~ North3.5[7],
regionOne == "HaZafon" && size == "medium" && year == year[3] ~ North3.5[11],
regionOne == "HaZafon" && size == "medium" && year == year[4] ~ North3.5[15],
regionOne == "HaZafon" && size == "medium" && year == year[5] ~ North3.5[19],
regionOne == "HaZafon" && size == "medium" && year == year[6] ~ North3.5[23],
regionOne == "HaZafon" && size == "medium" && year == year[7] ~ North3.5[27],
regionOne == "HaZafon" && size == "medium" && year == year[8] ~ North3.5[31],
regionOne == "HaZafon" && size == "medium" && year == year[9] ~ North3.5[35],
regionOne == "HaZafon" && size == "medium" && year == year[10]~ North3.5[39],
regionOne == "HaZafon" && size == "medium" && year == year[11]~ North3.5[43],
##################

regionOne == "HaZafon" && size == "large" && year == year[1] ~ North4.5[3],
regionOne == "HaZafon" && size == "large" && year == year[2] ~ North4.5[7],
regionOne == "HaZafon" && size == "large" && year == year[3] ~ North4.5[11],
regionOne == "HaZafon" && size == "large" && year == year[4] ~ North4.5[15],
regionOne == "HaZafon" && size == "large" && year == year[5] ~ North4.5[19],
regionOne == "HaZafon" && size == "large" && year == year[6] ~ North4.5[23],
regionOne == "HaZafon" && size == "large" && year == year[7] ~ North4.5[27],
regionOne == "HaZafon" && size == "large" && year == year[8] ~ North4.5[31],
regionOne == "HaZafon" && size == "large" && year == year[9] ~ North4.5[35],
regionOne == "HaZafon" && size == "large" && year == year[10]~ North4.5[39],
regionOne == "HaZafon" && size == "large" && year == year[11]~ North4.5[43],

##################
##################
regionOne == "Sharon" && size == "tiny" && year == year[1] ~ Sharon1.5[3],
regionOne == "Sharon" && size == "tiny" && year == year[2] ~ Sharon1.5[7],
regionOne == "Sharon" && size == "tiny" && year == year[3] ~ Sharon1.5[11],
regionOne == "Sharon" && size == "tiny" && year == year[4] ~ Sharon1.5[15],
regionOne == "Sharon" && size == "tiny" && year == year[5] ~ Sharon1.5[19],
regionOne == "Sharon" && size == "tiny" && year == year[6] ~ Sharon1.5[23],
regionOne == "Sharon" && size == "tiny" && year == year[7] ~ Sharon1.5[27],
regionOne == "Sharon" && size == "tiny" && year == year[8] ~ Sharon1.5[31],
regionOne == "Sharon" && size == "tiny" && year == year[9] ~ Sharon1.5[35],
regionOne == "Sharon" && size == "tiny" && year == year[10]~ Sharon1.5[39],
regionOne == "Sharon" && size == "tiny" && year == year[11]~ Sharon1.5[43],
##################

regionOne == "Sharon" && size == "small" && year == year[1] ~ Sharon2.5[3],
regionOne == "Sharon" && size == "small" && year == year[2] ~ Sharon2.5[7],
regionOne == "Sharon" && size == "small" && year == year[3] ~ Sharon2.5[11],
regionOne == "Sharon" && size == "small" && year == year[4] ~ Sharon2.5[15],
regionOne == "Sharon" && size == "small" && year == year[5] ~ Sharon2.5[19],
regionOne == "Sharon" && size == "small" && year == year[6] ~ Sharon2.5[23],
regionOne == "Sharon" && size == "small" && year == year[7] ~ Sharon2.5[27],
regionOne == "Sharon" && size == "small" && year == year[8] ~ Sharon2.5[31],
regionOne == "Sharon" && size == "small" && year == year[9] ~ Sharon2.5[35],
regionOne == "Sharon" && size == "small" && year == year[10]~ Sharon2.5[39],
regionOne == "Sharon" && size == "small" && year == year[11]~ Sharon2.5[43],
##################


regionOne == "Sharon" && size == "medium" && year == year[1] ~ Sharon3.5[3],
regionOne == "Sharon" && size == "medium" && year == year[2] ~ Sharon3.5[7],
regionOne == "Sharon" && size == "medium" && year == year[3] ~ Sharon3.5[11],
regionOne == "Sharon" && size == "medium" && year == year[4] ~ Sharon3.5[15],
regionOne == "Sharon" && size == "medium" && year == year[5] ~ Sharon3.5[19],
regionOne == "Sharon" && size == "medium" && year == year[6] ~ Sharon3.5[23],
regionOne == "Sharon" && size == "medium" && year == year[7] ~ Sharon3.5[27],
regionOne == "Sharon" && size == "medium" && year == year[8] ~ Sharon3.5[31],
regionOne == "Sharon" && size == "medium" && year == year[9] ~ Sharon3.5[35],
regionOne == "Sharon" && size == "medium" && year == year[10]~ Sharon3.5[39],
regionOne == "Sharon" && size == "medium" && year == year[11]~ Sharon3.5[43],
##################

regionOne == "Sharon" && size == "large" && year == year[1] ~ Sharon4.5[3],
regionOne == "Sharon" && size == "large" && year == year[2] ~ Sharon4.5[7],
regionOne == "Sharon" && size == "large" && year == year[3] ~ Sharon4.5[11],
regionOne == "Sharon" && size == "large" && year == year[4] ~ Sharon4.5[15],
regionOne == "Sharon" && size == "large" && year == year[5] ~ Sharon4.5[19],
regionOne == "Sharon" && size == "large" && year == year[6] ~ Sharon4.5[23],
regionOne == "Sharon" && size == "large" && year == year[7] ~ Sharon4.5[27],
regionOne == "Sharon" && size == "large" && year == year[8] ~ Sharon4.5[31],
regionOne == "Sharon" && size == "large" && year == year[9] ~ Sharon4.5[35],
regionOne == "Sharon" && size == "large" && year == year[10]~ Sharon4.5[39],
regionOne == "Sharon" && size == "large" && year == year[11]~ Sharon4.5[43],

##################
##################


regionOne == "Tel Aviv" && size == "tiny" && year == year[1] ~ tlv1.5[3],
regionOne == "Tel Aviv" && size == "tiny" && year == year[2] ~ tlv1.5[7],
regionOne == "Tel Aviv" && size == "tiny" && year == year[3] ~ tlv1.5[11],
regionOne == "Tel Aviv" && size == "tiny" && year == year[4] ~ tlv1.5[15],
regionOne == "Tel Aviv" && size == "tiny" && year == year[5] ~ tlv1.5[19],
regionOne == "Tel Aviv" && size == "tiny" && year == year[6] ~ tlv1.5[23],
regionOne == "Tel Aviv" && size == "tiny" && year == year[7] ~ tlv1.5[27],
regionOne == "Tel Aviv" && size == "tiny" && year == year[8] ~ tlv1.5[31],
regionOne == "Tel Aviv" && size == "tiny" && year == year[9] ~ tlv1.5[35],
regionOne == "Tel Aviv" && size == "tiny" && year == year[10]~ tlv1.5[39],
regionOne == "Tel Aviv" && size == "tiny" && year == year[11]~ tlv1.5[43],
##################

regionOne == "Tel Aviv" && size == "small" && year == year[1] ~ tlv2.5[3],
regionOne == "Tel Aviv" && size == "small" && year == year[2] ~ tlv2.5[7],
regionOne == "Tel Aviv" && size == "small" && year == year[3] ~ tlv2.5[11],
regionOne == "Tel Aviv" && size == "small" && year == year[4] ~ tlv2.5[15],
regionOne == "Tel Aviv" && size == "small" && year == year[5] ~ tlv2.5[19],
regionOne == "Tel Aviv" && size == "small" && year == year[6] ~ tlv2.5[23],
regionOne == "Tel Aviv" && size == "small" && year == year[7] ~ tlv2.5[27],
regionOne == "Tel Aviv" && size == "small" && year == year[8] ~ tlv2.5[31],
regionOne == "Tel Aviv" && size == "small" && year == year[9] ~ tlv2.5[35],
regionOne == "Tel Aviv" && size == "small" && year == year[10]~ tlv2.5[39],
regionOne == "Tel Aviv" && size == "small" && year == year[11]~ tlv2.5[43],
##################

regionOne == "Tel Aviv" && size == "medium" && year == year[1] ~ tlv3.5[3],
regionOne == "Tel Aviv" && size == "medium" && year == year[2] ~ tlv3.5[7],
regionOne == "Tel Aviv" && size == "medium" && year == year[3] ~ tlv3.5[11],
regionOne == "Tel Aviv" && size == "medium" && year == year[4] ~ tlv3.5[15],
regionOne == "Tel Aviv" && size == "medium" && year == year[5] ~ tlv3.5[19],
regionOne == "Tel Aviv" && size == "medium" && year == year[6] ~ tlv3.5[23],
regionOne == "Tel Aviv" && size == "medium" && year == year[7] ~ tlv3.5[27],
regionOne == "Tel Aviv" && size == "medium" && year == year[8] ~ tlv3.5[31],
regionOne == "Tel Aviv" && size == "medium" && year == year[9] ~ tlv3.5[35],
regionOne == "Tel Aviv" && size == "medium" && year == year[10]~ tlv3.5[39],
regionOne == "Tel Aviv" && size == "medium" && year == year[11]~ tlv3.5[43],
##################

regionOne == "Tel Aviv" && size == "large" && year == year[1] ~ tlv4.5[3],
regionOne == "Tel Aviv" && size == "large" && year == year[2] ~ tlv4.5[7],
regionOne == "Tel Aviv" && size == "large" && year == year[3] ~ tlv4.5[11],
regionOne == "Tel Aviv" && size == "large" && year == year[4] ~ tlv4.5[15],
regionOne == "Tel Aviv" && size == "large" && year == year[5] ~ tlv4.5[19],
regionOne == "Tel Aviv" && size == "large" && year == year[6] ~ tlv4.5[23],
regionOne == "Tel Aviv" && size == "large" && year == year[7] ~ tlv4.5[27],
regionOne == "Tel Aviv" && size == "large" && year == year[8] ~ tlv4.5[31],
regionOne == "Tel Aviv" && size == "large" && year == year[9] ~ tlv4.5[35],
regionOne == "Tel Aviv" && size == "large" && year == year[10]~ tlv4.5[39],
regionOne == "Tel Aviv" && size == "large" && year == year[11]~ tlv4.5[43],
TRUE                                                           ~ 99          # catch undefined Region
))
    return("Home prices added by city/size/year to each renter.")
  } else if (frame == "owners") {
  ## add column
    ### set initial value
    Own5$houseP = as.factor("foo")
  ## set $houseP based on yr/region/rooms

Own6 <<-
     Own5 %>%
     mutate(houseP == case_when(

regionOne == "Jerusalem" && size == "tiny" && year == year[1] ~ Jerusalem1.5[3],
regionOne == "Jerusalem" && size == "tiny" && year == year[2] ~ Jerusalem1.5[7],
regionOne == "Jerusalem" && size == "tiny" && year == year[3] ~ Jerusalem1.5[11],
regionOne == "Jerusalem" && size == "tiny" && year == year[4] ~ Jerusalem1.5[15],
regionOne == "Jerusalem" && size == "tiny" && year == year[5] ~ Jerusalem1.5[19],
regionOne == "Jerusalem" && size == "tiny" && year == year[6] ~ Jerusalem1.5[23],
regionOne == "Jerusalem" && size == "tiny" && year == year[7] ~ Jerusalem1.5[27],
regionOne == "Jerusalem" && size == "tiny" && year == year[8] ~ Jerusalem1.5[31],
regionOne == "Jerusalem" && size == "tiny" && year == year[9] ~ Jerusalem1.5[35],
regionOne == "Jerusalem" && size == "tiny" && year == year[10]~ Jerusalem1.5[39],
regionOne == "Jerusalem" && size == "tiny" && year == year[11]~ Jerusalem1.5[43],
##################

regionOne == "Jerusalem" && size == "small" && year == year[1] ~ Jerusalem2.5[3],
regionOne == "Jerusalem" && size == "small" && year == year[2] ~ Jerusalem2.5[7],
regionOne == "Jerusalem" && size == "small" && year == year[3] ~ Jerusalem2.5[11],
regionOne == "Jerusalem" && size == "small" && year == year[4] ~ Jerusalem2.5[15],
regionOne == "Jerusalem" && size == "small" && year == year[5] ~ Jerusalem2.5[19],
regionOne == "Jerusalem" && size == "small" && year == year[6] ~ Jerusalem2.5[23],
regionOne == "Jerusalem" && size == "small" && year == year[7] ~ Jerusalem2.5[27],
regionOne == "Jerusalem" && size == "small" && year == year[8] ~ Jerusalem2.5[31],
regionOne == "Jerusalem" && size == "small" && year == year[9] ~ Jerusalem2.5[35],
regionOne == "Jerusalem" && size == "small" && year == year[10]~ Jerusalem2.5[39],
regionOne == "Jerusalem" && size == "small" && year == year[11]~ Jerusalem2.5[43],
##################


regionOne == "Jerusalem" && size == "medium" && year == year[1] ~ Jerusalem3.5[3],
regionOne == "Jerusalem" && size == "medium" && year == year[2] ~ Jerusalem3.5[7],
regionOne == "Jerusalem" && size == "medium" && year == year[3] ~ Jerusalem3.5[11],
regionOne == "Jerusalem" && size == "medium" && year == year[4] ~ Jerusalem3.5[15],
regionOne == "Jerusalem" && size == "medium" && year == year[5] ~ Jerusalem3.5[19],
regionOne == "Jerusalem" && size == "medium" && year == year[6] ~ Jerusalem3.5[23],
regionOne == "Jerusalem" && size == "medium" && year == year[7] ~ Jerusalem3.5[27],
regionOne == "Jerusalem" && size == "medium" && year == year[8] ~ Jerusalem3.5[31],
regionOne == "Jerusalem" && size == "medium" && year == year[9] ~ Jerusalem3.5[35],
regionOne == "Jerusalem" && size == "medium" && year == year[10]~ Jerusalem3.5[39],
regionOne == "Jerusalem" && size == "medium" && year == year[11]~ Jerusalem3.5[43],
##################

regionOne == "Jerusalem" && size == "large" && year == year[1] ~ Jerusalem4.5[3],
regionOne == "Jerusalem" && size == "large" && year == year[2] ~ Jerusalem4.5[7],
regionOne == "Jerusalem" && size == "large" && year == year[3] ~ Jerusalem4.5[11],
regionOne == "Jerusalem" && size == "large" && year == year[4] ~ Jerusalem4.5[15],
regionOne == "Jerusalem" && size == "large" && year == year[5] ~ Jerusalem4.5[19],
regionOne == "Jerusalem" && size == "large" && year == year[6] ~ Jerusalem4.5[23],
regionOne == "Jerusalem" && size == "large" && year == year[7] ~ Jerusalem4.5[27],
regionOne == "Jerusalem" && size == "large" && year == year[8] ~ Jerusalem4.5[31],
regionOne == "Jerusalem" && size == "large" && year == year[9] ~ Jerusalem4.5[35],
regionOne == "Jerusalem" && size == "large" && year == year[10]~ Jerusalem4.5[39],
regionOne == "Jerusalem" && size == "large" && year == year[11]~ Jerusalem4.5[43],

##################
##################

regionOne == "Gush Dan" && size == "tiny" && year == year[1] ~ GushDan1.5[3],
regionOne == "Gush Dan" && size == "tiny" && year == year[2] ~ GushDan1.5[7],
regionOne == "Gush Dan" && size == "tiny" && year == year[3] ~ GushDan1.5[11],
regionOne == "Gush Dan" && size == "tiny" && year == year[4] ~ GushDan1.5[15],
regionOne == "Gush Dan" && size == "tiny" && year == year[5] ~ GushDan1.5[19],
regionOne == "Gush Dan" && size == "tiny" && year == year[6] ~ GushDan1.5[23],
regionOne == "Gush Dan" && size == "tiny" && year == year[7] ~ GushDan1.5[27],
regionOne == "Gush Dan" && size == "tiny" && year == year[8] ~ GushDan1.5[31],
regionOne == "Gush Dan" && size == "tiny" && year == year[9] ~ GushDan1.5[35],
regionOne == "Gush Dan" && size == "tiny" && year == year[10]~ GushDan1.5[39],
regionOne == "Gush Dan" && size == "tiny" && year == year[11]~ GushDan1.5[43],
##################

regionOne == "Gush Dan" && size == "small" && year == year[1] ~ GushDan2.5[3],
regionOne == "Gush Dan" && size == "small" && year == year[2] ~ GushDan2.5[7],
regionOne == "Gush Dan" && size == "small" && year == year[3] ~ GushDan2.5[11],
regionOne == "Gush Dan" && size == "small" && year == year[4] ~ GushDan2.5[15],
regionOne == "Gush Dan" && size == "small" && year == year[5] ~ GushDan2.5[19],
regionOne == "Gush Dan" && size == "small" && year == year[6] ~ GushDan2.5[23],
regionOne == "Gush Dan" && size == "small" && year == year[7] ~ GushDan2.5[27],
regionOne == "Gush Dan" && size == "small" && year == year[8] ~ GushDan2.5[31],
regionOne == "Gush Dan" && size == "small" && year == year[9] ~ GushDan2.5[35],
regionOne == "Gush Dan" && size == "small" && year == year[10]~ GushDan2.5[39],
regionOne == "Gush Dan" && size == "small" && year == year[11]~ GushDan2.5[43],
##################


regionOne == "Gush Dan" && size == "medium" && year == year[1] ~ GushDan3.5[3],
regionOne == "Gush Dan" && size == "medium" && year == year[2] ~ GushDan3.5[7],
regionOne == "Gush Dan" && size == "medium" && year == year[3] ~ GushDan3.5[11],
regionOne == "Gush Dan" && size == "medium" && year == year[4] ~ GushDan3.5[15],
regionOne == "Gush Dan" && size == "medium" && year == year[5] ~ GushDan3.5[19],
regionOne == "Gush Dan" && size == "medium" && year == year[6] ~ GushDan3.5[23],
regionOne == "Gush Dan" && size == "medium" && year == year[7] ~ GushDan3.5[27],
regionOne == "Gush Dan" && size == "medium" && year == year[8] ~ GushDan3.5[31],
regionOne == "Gush Dan" && size == "medium" && year == year[9] ~ GushDan3.5[35],
regionOne == "Gush Dan" && size == "medium" && year == year[10]~ GushDan3.5[39],
regionOne == "Gush Dan" && size == "medium" && year == year[11]~ GushDan3.5[43],
##################

regionOne == "Gush Dan" && size == "large" && year == year[1] ~ GushDan4.5[3],
regionOne == "Gush Dan" && size == "large" && year == year[2] ~ GushDan4.5[7],
regionOne == "Gush Dan" && size == "large" && year == year[3] ~ GushDan4.5[11],
regionOne == "Gush Dan" && size == "large" && year == year[4] ~ GushDan4.5[15],
regionOne == "Gush Dan" && size == "large" && year == year[5] ~ GushDan4.5[19],
regionOne == "Gush Dan" && size == "large" && year == year[6] ~ GushDan4.5[23],
regionOne == "Gush Dan" && size == "large" && year == year[7] ~ GushDan4.5[27],
regionOne == "Gush Dan" && size == "large" && year == year[8] ~ GushDan4.5[31],
regionOne == "Gush Dan" && size == "large" && year == year[9] ~ GushDan4.5[35],
regionOne == "Gush Dan" && size == "large" && year == year[10]~ GushDan4.5[39],
regionOne == "Gush Dan" && size == "large" && year == year[11]~ GushDan4.5[43],

##################
##################


regionOne == "HaDarom" && size == "tiny" && year == year[1] ~ South1.5[3],
regionOne == "HaDarom" && size == "tiny" && year == year[2] ~ South1.5[7],
regionOne == "HaDarom" && size == "tiny" && year == year[3] ~ South1.5[11],
regionOne == "HaDarom" && size == "tiny" && year == year[4] ~ South1.5[15],
regionOne == "HaDarom" && size == "tiny" && year == year[5] ~ South1.5[19],
regionOne == "HaDarom" && size == "tiny" && year == year[6] ~ South1.5[23],
regionOne == "HaDarom" && size == "tiny" && year == year[7] ~ South1.5[27],
regionOne == "HaDarom" && size == "tiny" && year == year[8] ~ South1.5[31],
regionOne == "HaDarom" && size == "tiny" && year == year[9] ~ South1.5[35],
regionOne == "HaDarom" && size == "tiny" && year == year[10]~ South1.5[39],
regionOne == "HaDarom" && size == "tiny" && year == year[11]~ South1.5[43],
##################

regionOne == "HaDarom" && size == "small" && year == year[1] ~ South2.5[3],
regionOne == "HaDarom" && size == "small" && year == year[2] ~ South2.5[7],
regionOne == "HaDarom" && size == "small" && year == year[3] ~ South2.5[11],
regionOne == "HaDarom" && size == "small" && year == year[4] ~ South2.5[15],
regionOne == "HaDarom" && size == "small" && year == year[5] ~ South2.5[19],
regionOne == "HaDarom" && size == "small" && year == year[6] ~ South2.5[23],
regionOne == "HaDarom" && size == "small" && year == year[7] ~ South2.5[27],
regionOne == "HaDarom" && size == "small" && year == year[8] ~ South2.5[31],
regionOne == "HaDarom" && size == "small" && year == year[9] ~ South2.5[35],
regionOne == "HaDarom" && size == "small" && year == year[10]~ South2.5[39],
regionOne == "HaDarom" && size == "small" && year == year[11]~ South2.5[43],
##################


regionOne == "HaDarom" && size == "medium" && year == year[1] ~ South3.5[3],
regionOne == "HaDarom" && size == "medium" && year == year[2] ~ South3.5[7],
regionOne == "HaDarom" && size == "medium" && year == year[3] ~ South3.5[11],
regionOne == "HaDarom" && size == "medium" && year == year[4] ~ South3.5[15],
regionOne == "HaDarom" && size == "medium" && year == year[5] ~ South3.5[19],
regionOne == "HaDarom" && size == "medium" && year == year[6] ~ South3.5[23],
regionOne == "HaDarom" && size == "medium" && year == year[7] ~ South3.5[27],
regionOne == "HaDarom" && size == "medium" && year == year[8] ~ South3.5[31],
regionOne == "HaDarom" && size == "medium" && year == year[9] ~ South3.5[35],
regionOne == "HaDarom" && size == "medium" && year == year[10]~ South3.5[39],
regionOne == "HaDarom" && size == "medium" && year == year[11]~ South3.5[43],
##################

regionOne == "HaDarom" && size == "large" && year == year[1] ~ South4.5[3],
regionOne == "HaDarom" && size == "large" && year == year[2] ~ South4.5[7],
regionOne == "HaDarom" && size == "large" && year == year[3] ~ South4.5[11],
regionOne == "HaDarom" && size == "large" && year == year[4] ~ South4.5[15],
regionOne == "HaDarom" && size == "large" && year == year[5] ~ South4.5[19],
regionOne == "HaDarom" && size == "large" && year == year[6] ~ South4.5[23],
regionOne == "HaDarom" && size == "large" && year == year[7] ~ South4.5[27],
regionOne == "HaDarom" && size == "large" && year == year[8] ~ South4.5[31],
regionOne == "HaDarom" && size == "large" && year == year[9] ~ South4.5[35],
regionOne == "HaDarom" && size == "large" && year == year[10]~ South4.5[39],
regionOne == "HaDarom" && size == "large" && year == year[11]~ South4.5[43],

##################
##################


regionOne == "Haifa" && size == "tiny" && year == year[1] ~ Haifa1.5[3],
regionOne == "Haifa" && size == "tiny" && year == year[2] ~ Haifa1.5[7],
regionOne == "Haifa" && size == "tiny" && year == year[3] ~ Haifa1.5[11],
regionOne == "Haifa" && size == "tiny" && year == year[4] ~ Haifa1.5[15],
regionOne == "Haifa" && size == "tiny" && year == year[5] ~ Haifa1.5[19],
regionOne == "Haifa" && size == "tiny" && year == year[6] ~ Haifa1.5[23],
regionOne == "Haifa" && size == "tiny" && year == year[7] ~ Haifa1.5[27],
regionOne == "Haifa" && size == "tiny" && year == year[8] ~ Haifa1.5[31],
regionOne == "Haifa" && size == "tiny" && year == year[9] ~ Haifa1.5[35],
regionOne == "Haifa" && size == "tiny" && year == year[10]~ Haifa1.5[39],
regionOne == "Haifa" && size == "tiny" && year == year[11]~ Haifa1.5[43],
##################

regionOne == "Haifa" && size == "small" && year == year[1] ~ Haifa2.5[3],
regionOne == "Haifa" && size == "small" && year == year[2] ~ Haifa2.5[7],
regionOne == "Haifa" && size == "small" && year == year[3] ~ Haifa2.5[11],
regionOne == "Haifa" && size == "small" && year == year[4] ~ Haifa2.5[15],
regionOne == "Haifa" && size == "small" && year == year[5] ~ Haifa2.5[19],
regionOne == "Haifa" && size == "small" && year == year[6] ~ Haifa2.5[23],
regionOne == "Haifa" && size == "small" && year == year[7] ~ Haifa2.5[27],
regionOne == "Haifa" && size == "small" && year == year[8] ~ Haifa2.5[31],
regionOne == "Haifa" && size == "small" && year == year[9] ~ Haifa2.5[35],
regionOne == "Haifa" && size == "small" && year == year[10]~ Haifa2.5[39],
regionOne == "Haifa" && size == "small" && year == year[11]~ Haifa2.5[43],
##################


regionOne == "Haifa" && size == "medium" && year == year[1] ~ Haifa3.5[3],
regionOne == "Haifa" && size == "medium" && year == year[2] ~ Haifa3.5[7],
regionOne == "Haifa" && size == "medium" && year == year[3] ~ Haifa3.5[11],
regionOne == "Haifa" && size == "medium" && year == year[4] ~ Haifa3.5[15],
regionOne == "Haifa" && size == "medium" && year == year[5] ~ Haifa3.5[19],
regionOne == "Haifa" && size == "medium" && year == year[6] ~ Haifa3.5[23],
regionOne == "Haifa" && size == "medium" && year == year[7] ~ Haifa3.5[27],
regionOne == "Haifa" && size == "medium" && year == year[8] ~ Haifa3.5[31],
regionOne == "Haifa" && size == "medium" && year == year[9] ~ Haifa3.5[35],
regionOne == "Haifa" && size == "medium" && year == year[10]~ Haifa3.5[39],
regionOne == "Haifa" && size == "medium" && year == year[11]~ Haifa3.5[43],
##################

regionOne == "Haifa" && size == "large" && year == year[1] ~ Haifa4.5[3],
regionOne == "Haifa" && size == "large" && year == year[2] ~ Haifa4.5[7],
regionOne == "Haifa" && size == "large" && year == year[3] ~ Haifa4.5[11],
regionOne == "Haifa" && size == "large" && year == year[4] ~ Haifa4.5[15],
regionOne == "Haifa" && size == "large" && year == year[5] ~ Haifa4.5[19],
regionOne == "Haifa" && size == "large" && year == year[6] ~ Haifa4.5[23],
regionOne == "Haifa" && size == "large" && year == year[7] ~ Haifa4.5[27],
regionOne == "Haifa" && size == "large" && year == year[8] ~ Haifa4.5[31],
regionOne == "Haifa" && size == "large" && year == year[9] ~ Haifa4.5[35],
regionOne == "Haifa" && size == "large" && year == year[10]~ Haifa4.5[39],
regionOne == "Haifa" && size == "large" && year == year[11]~ Haifa4.5[43],

##################
##################


regionOne == "HaMerkaz" && size == "tiny" && year == year[1] ~ centerJeruPeri1.5[3],
regionOne == "HaMerkaz" && size == "tiny" && year == year[2] ~ centerJeruPeri1.5[7],
regionOne == "HaMerkaz" && size == "tiny" && year == year[3] ~ centerJeruPeri1.5[11],
regionOne == "HaMerkaz" && size == "tiny" && year == year[4] ~ centerJeruPeri1.5[15],
regionOne == "HaMerkaz" && size == "tiny" && year == year[5] ~ centerJeruPeri1.5[19],
regionOne == "HaMerkaz" && size == "tiny" && year == year[6] ~ centerJeruPeri1.5[23],
regionOne == "HaMerkaz" && size == "tiny" && year == year[7] ~ centerJeruPeri1.5[27],
regionOne == "HaMerkaz" && size == "tiny" && year == year[8] ~ centerJeruPeri1.5[31],
regionOne == "HaMerkaz" && size == "tiny" && year == year[9] ~ centerJeruPeri1.5[35],
regionOne == "HaMerkaz" && size == "tiny" && year == year[10]~ centerJeruPeri1.5[39],
regionOne == "HaMerkaz" && size == "tiny" && year == year[11]~ centerJeruPeri1.5[43],
##################

regionOne == "HaMerkaz" && size == "small" && year == year[1] ~ centerJeruPeri2.5[3],
regionOne == "HaMerkaz" && size == "small" && year == year[2] ~ centerJeruPeri2.5[7],
regionOne == "HaMerkaz" && size == "small" && year == year[3] ~ centerJeruPeri2.5[11],
regionOne == "HaMerkaz" && size == "small" && year == year[4] ~ centerJeruPeri2.5[15],
regionOne == "HaMerkaz" && size == "small" && year == year[5] ~ centerJeruPeri2.5[19],
regionOne == "HaMerkaz" && size == "small" && year == year[6] ~ centerJeruPeri2.5[23],
regionOne == "HaMerkaz" && size == "small" && year == year[7] ~ centerJeruPeri2.5[27],
regionOne == "HaMerkaz" && size == "small" && year == year[8] ~ centerJeruPeri2.5[31],
regionOne == "HaMerkaz" && size == "small" && year == year[9] ~ centerJeruPeri2.5[35],
regionOne == "HaMerkaz" && size == "small" && year == year[10]~ centerJeruPeri2.5[39],
regionOne == "HaMerkaz" && size == "small" && year == year[11]~ centerJeruPeri2.5[43],
##################


regionOne == "HaMerkaz" && size == "medium" && year == year[1] ~ centerJeruPeri3.5[3],
regionOne == "HaMerkaz" && size == "medium" && year == year[2] ~ centerJeruPeri3.5[7],
regionOne == "HaMerkaz" && size == "medium" && year == year[3] ~ centerJeruPeri3.5[11],
regionOne == "HaMerkaz" && size == "medium" && year == year[4] ~ centerJeruPeri3.5[15],
regionOne == "HaMerkaz" && size == "medium" && year == year[5] ~ centerJeruPeri3.5[19],
regionOne == "HaMerkaz" && size == "medium" && year == year[6] ~ centerJeruPeri3.5[23],
regionOne == "HaMerkaz" && size == "medium" && year == year[7] ~ centerJeruPeri3.5[27],
regionOne == "HaMerkaz" && size == "medium" && year == year[8] ~ centerJeruPeri3.5[31],
regionOne == "HaMerkaz" && size == "medium" && year == year[9] ~ centerJeruPeri3.5[35],
regionOne == "HaMerkaz" && size == "medium" && year == year[10]~ centerJeruPeri3.5[39],
regionOne == "HaMerkaz" && size == "medium" && year == year[11]~ centerJeruPeri3.5[43],
##################

regionOne == "HaMerkaz" && size == "large" && year == year[1] ~ centerJeruPeri4.5[3],
regionOne == "HaMerkaz" && size == "large" && year == year[2] ~ centerJeruPeri4.5[7],
regionOne == "HaMerkaz" && size == "large" && year == year[3] ~ centerJeruPeri4.5[11],
regionOne == "HaMerkaz" && size == "large" && year == year[4] ~ centerJeruPeri4.5[15],
regionOne == "HaMerkaz" && size == "large" && year == year[5] ~ centerJeruPeri4.5[19],
regionOne == "HaMerkaz" && size == "large" && year == year[6] ~ centerJeruPeri4.5[23],
regionOne == "HaMerkaz" && size == "large" && year == year[7] ~ centerJeruPeri4.5[27],
regionOne == "HaMerkaz" && size == "large" && year == year[8] ~ centerJeruPeri4.5[31],
regionOne == "HaMerkaz" && size == "large" && year == year[9] ~ centerJeruPeri4.5[35],
regionOne == "HaMerkaz" && size == "large" && year == year[10]~ centerJeruPeri4.5[39],
regionOne == "HaMerkaz" && size == "large" && year == year[11]~ centerJeruPeri4.5[43],

##################
##################

regionOne == "HaZafon" && size == "tiny" && year == year[1] ~ North1.5[3],
regionOne == "HaZafon" && size == "tiny" && year == year[2] ~ North1.5[7],
regionOne == "HaZafon" && size == "tiny" && year == year[3] ~ North1.5[11],
regionOne == "HaZafon" && size == "tiny" && year == year[4] ~ North1.5[15],
regionOne == "HaZafon" && size == "tiny" && year == year[5] ~ North1.5[19],
regionOne == "HaZafon" && size == "tiny" && year == year[6] ~ North1.5[23],
regionOne == "HaZafon" && size == "tiny" && year == year[7] ~ North1.5[27],
regionOne == "HaZafon" && size == "tiny" && year == year[8] ~ North1.5[31],
regionOne == "HaZafon" && size == "tiny" && year == year[9] ~ North1.5[35],
regionOne == "HaZafon" && size == "tiny" && year == year[10]~ North1.5[39],
regionOne == "HaZafon" && size == "tiny" && year == year[11]~ North1.5[43],
##################

regionOne == "HaZafon" && size == "small" && year == year[1] ~ North2.5[3],
regionOne == "HaZafon" && size == "small" && year == year[2] ~ North2.5[7],
regionOne == "HaZafon" && size == "small" && year == year[3] ~ North2.5[11],
regionOne == "HaZafon" && size == "small" && year == year[4] ~ North2.5[15],
regionOne == "HaZafon" && size == "small" && year == year[5] ~ North2.5[19],
regionOne == "HaZafon" && size == "small" && year == year[6] ~ North2.5[23],
regionOne == "HaZafon" && size == "small" && year == year[7] ~ North2.5[27],
regionOne == "HaZafon" && size == "small" && year == year[8] ~ North2.5[31],
regionOne == "HaZafon" && size == "small" && year == year[9] ~ North2.5[35],
regionOne == "HaZafon" && size == "small" && year == year[10]~ North2.5[39],
regionOne == "HaZafon" && size == "small" && year == year[11]~ North2.5[43],
##################


regionOne == "HaZafon" && size == "medium" && year == year[1] ~ North3.5[3],
regionOne == "HaZafon" && size == "medium" && year == year[2] ~ North3.5[7],
regionOne == "HaZafon" && size == "medium" && year == year[3] ~ North3.5[11],
regionOne == "HaZafon" && size == "medium" && year == year[4] ~ North3.5[15],
regionOne == "HaZafon" && size == "medium" && year == year[5] ~ North3.5[19],
regionOne == "HaZafon" && size == "medium" && year == year[6] ~ North3.5[23],
regionOne == "HaZafon" && size == "medium" && year == year[7] ~ North3.5[27],
regionOne == "HaZafon" && size == "medium" && year == year[8] ~ North3.5[31],
regionOne == "HaZafon" && size == "medium" && year == year[9] ~ North3.5[35],
regionOne == "HaZafon" && size == "medium" && year == year[10]~ North3.5[39],
regionOne == "HaZafon" && size == "medium" && year == year[11]~ North3.5[43],
##################

regionOne == "HaZafon" && size == "large" && year == year[1] ~ North4.5[3],
regionOne == "HaZafon" && size == "large" && year == year[2] ~ North4.5[7],
regionOne == "HaZafon" && size == "large" && year == year[3] ~ North4.5[11],
regionOne == "HaZafon" && size == "large" && year == year[4] ~ North4.5[15],
regionOne == "HaZafon" && size == "large" && year == year[5] ~ North4.5[19],
regionOne == "HaZafon" && size == "large" && year == year[6] ~ North4.5[23],
regionOne == "HaZafon" && size == "large" && year == year[7] ~ North4.5[27],
regionOne == "HaZafon" && size == "large" && year == year[8] ~ North4.5[31],
regionOne == "HaZafon" && size == "large" && year == year[9] ~ North4.5[35],
regionOne == "HaZafon" && size == "large" && year == year[10]~ North4.5[39],
regionOne == "HaZafon" && size == "large" && year == year[11]~ North4.5[43],

##################
##################
regionOne == "Sharon" && size == "tiny" && year == year[1] ~ Sharon1.5[3],
regionOne == "Sharon" && size == "tiny" && year == year[2] ~ Sharon1.5[7],
regionOne == "Sharon" && size == "tiny" && year == year[3] ~ Sharon1.5[11],
regionOne == "Sharon" && size == "tiny" && year == year[4] ~ Sharon1.5[15],
regionOne == "Sharon" && size == "tiny" && year == year[5] ~ Sharon1.5[19],
regionOne == "Sharon" && size == "tiny" && year == year[6] ~ Sharon1.5[23],
regionOne == "Sharon" && size == "tiny" && year == year[7] ~ Sharon1.5[27],
regionOne == "Sharon" && size == "tiny" && year == year[8] ~ Sharon1.5[31],
regionOne == "Sharon" && size == "tiny" && year == year[9] ~ Sharon1.5[35],
regionOne == "Sharon" && size == "tiny" && year == year[10]~ Sharon1.5[39],
regionOne == "Sharon" && size == "tiny" && year == year[11]~ Sharon1.5[43],
##################

regionOne == "Sharon" && size == "small" && year == year[1] ~ Sharon2.5[3],
regionOne == "Sharon" && size == "small" && year == year[2] ~ Sharon2.5[7],
regionOne == "Sharon" && size == "small" && year == year[3] ~ Sharon2.5[11],
regionOne == "Sharon" && size == "small" && year == year[4] ~ Sharon2.5[15],
regionOne == "Sharon" && size == "small" && year == year[5] ~ Sharon2.5[19],
regionOne == "Sharon" && size == "small" && year == year[6] ~ Sharon2.5[23],
regionOne == "Sharon" && size == "small" && year == year[7] ~ Sharon2.5[27],
regionOne == "Sharon" && size == "small" && year == year[8] ~ Sharon2.5[31],
regionOne == "Sharon" && size == "small" && year == year[9] ~ Sharon2.5[35],
regionOne == "Sharon" && size == "small" && year == year[10]~ Sharon2.5[39],
regionOne == "Sharon" && size == "small" && year == year[11]~ Sharon2.5[43],
##################


regionOne == "Sharon" && size == "medium" && year == year[1] ~ Sharon3.5[3],
regionOne == "Sharon" && size == "medium" && year == year[2] ~ Sharon3.5[7],
regionOne == "Sharon" && size == "medium" && year == year[3] ~ Sharon3.5[11],
regionOne == "Sharon" && size == "medium" && year == year[4] ~ Sharon3.5[15],
regionOne == "Sharon" && size == "medium" && year == year[5] ~ Sharon3.5[19],
regionOne == "Sharon" && size == "medium" && year == year[6] ~ Sharon3.5[23],
regionOne == "Sharon" && size == "medium" && year == year[7] ~ Sharon3.5[27],
regionOne == "Sharon" && size == "medium" && year == year[8] ~ Sharon3.5[31],
regionOne == "Sharon" && size == "medium" && year == year[9] ~ Sharon3.5[35],
regionOne == "Sharon" && size == "medium" && year == year[10]~ Sharon3.5[39],
regionOne == "Sharon" && size == "medium" && year == year[11]~ Sharon3.5[43],
##################

regionOne == "Sharon" && size == "large" && year == year[1] ~ Sharon4.5[3],
regionOne == "Sharon" && size == "large" && year == year[2] ~ Sharon4.5[7],
regionOne == "Sharon" && size == "large" && year == year[3] ~ Sharon4.5[11],
regionOne == "Sharon" && size == "large" && year == year[4] ~ Sharon4.5[15],
regionOne == "Sharon" && size == "large" && year == year[5] ~ Sharon4.5[19],
regionOne == "Sharon" && size == "large" && year == year[6] ~ Sharon4.5[23],
regionOne == "Sharon" && size == "large" && year == year[7] ~ Sharon4.5[27],
regionOne == "Sharon" && size == "large" && year == year[8] ~ Sharon4.5[31],
regionOne == "Sharon" && size == "large" && year == year[9] ~ Sharon4.5[35],
regionOne == "Sharon" && size == "large" && year == year[10]~ Sharon4.5[39],
regionOne == "Sharon" && size == "large" && year == year[11]~ Sharon4.5[43],

##################
##################


regionOne == "Tel Aviv" && size == "tiny" && year == year[1] ~ tlv1.5[3],
regionOne == "Tel Aviv" && size == "tiny" && year == year[2] ~ tlv1.5[7],
regionOne == "Tel Aviv" && size == "tiny" && year == year[3] ~ tlv1.5[11],
regionOne == "Tel Aviv" && size == "tiny" && year == year[4] ~ tlv1.5[15],
regionOne == "Tel Aviv" && size == "tiny" && year == year[5] ~ tlv1.5[19],
regionOne == "Tel Aviv" && size == "tiny" && year == year[6] ~ tlv1.5[23],
regionOne == "Tel Aviv" && size == "tiny" && year == year[7] ~ tlv1.5[27],
regionOne == "Tel Aviv" && size == "tiny" && year == year[8] ~ tlv1.5[31],
regionOne == "Tel Aviv" && size == "tiny" && year == year[9] ~ tlv1.5[35],
regionOne == "Tel Aviv" && size == "tiny" && year == year[10]~ tlv1.5[39],
regionOne == "Tel Aviv" && size == "tiny" && year == year[11]~ tlv1.5[43],
##################

regionOne == "Tel Aviv" && size == "small" && year == year[1] ~ tlv2.5[3],
regionOne == "Tel Aviv" && size == "small" && year == year[2] ~ tlv2.5[7],
regionOne == "Tel Aviv" && size == "small" && year == year[3] ~ tlv2.5[11],
regionOne == "Tel Aviv" && size == "small" && year == year[4] ~ tlv2.5[15],
regionOne == "Tel Aviv" && size == "small" && year == year[5] ~ tlv2.5[19],
regionOne == "Tel Aviv" && size == "small" && year == year[6] ~ tlv2.5[23],
regionOne == "Tel Aviv" && size == "small" && year == year[7] ~ tlv2.5[27],
regionOne == "Tel Aviv" && size == "small" && year == year[8] ~ tlv2.5[31],
regionOne == "Tel Aviv" && size == "small" && year == year[9] ~ tlv2.5[35],
regionOne == "Tel Aviv" && size == "small" && year == year[10]~ tlv2.5[39],
regionOne == "Tel Aviv" && size == "small" && year == year[11]~ tlv2.5[43],
##################

regionOne == "Tel Aviv" && size == "medium" && year == year[1] ~ tlv3.5[3],
regionOne == "Tel Aviv" && size == "medium" && year == year[2] ~ tlv3.5[7],
regionOne == "Tel Aviv" && size == "medium" && year == year[3] ~ tlv3.5[11],
regionOne == "Tel Aviv" && size == "medium" && year == year[4] ~ tlv3.5[15],
regionOne == "Tel Aviv" && size == "medium" && year == year[5] ~ tlv3.5[19],
regionOne == "Tel Aviv" && size == "medium" && year == year[6] ~ tlv3.5[23],
regionOne == "Tel Aviv" && size == "medium" && year == year[7] ~ tlv3.5[27],
regionOne == "Tel Aviv" && size == "medium" && year == year[8] ~ tlv3.5[31],
regionOne == "Tel Aviv" && size == "medium" && year == year[9] ~ tlv3.5[35],
regionOne == "Tel Aviv" && size == "medium" && year == year[10]~ tlv3.5[39],
regionOne == "Tel Aviv" && size == "medium" && year == year[11]~ tlv3.5[43],
##################

regionOne == "Tel Aviv" && size == "large" && year == year[1] ~ tlv4.5[3],
regionOne == "Tel Aviv" && size == "large" && year == year[2] ~ tlv4.5[7],
regionOne == "Tel Aviv" && size == "large" && year == year[3] ~ tlv4.5[11],
regionOne == "Tel Aviv" && size == "large" && year == year[4] ~ tlv4.5[15],
regionOne == "Tel Aviv" && size == "large" && year == year[5] ~ tlv4.5[19],
regionOne == "Tel Aviv" && size == "large" && year == year[6] ~ tlv4.5[23],
regionOne == "Tel Aviv" && size == "large" && year == year[7] ~ tlv4.5[27],
regionOne == "Tel Aviv" && size == "large" && year == year[8] ~ tlv4.5[31],
regionOne == "Tel Aviv" && size == "large" && year == year[9] ~ tlv4.5[35],
regionOne == "Tel Aviv" && size == "large" && year == year[10]~ tlv4.5[39],
regionOne == "Tel Aviv" && size == "large" && year == year[11]~ tlv4.5[43],
))
    return("Home prices added by city/size/year to each renter.")
  } else {
    return("error: 345a,  not yet implemented.")
  }
}

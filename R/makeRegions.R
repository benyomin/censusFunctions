#' Add a region column based on $CODELOC (city) in the original data.
#' v. 3.4 - makeRegions("famYYYY") needs <<- to namespace
#' This function adds columns $regionOne and $regionTwo
#' regionOne puts 13 cities in Gush Dan.
#' regionTwo puts  2 cities in Gush Dan.
#' @param arg The arg which you will perform the function on.
#' @family manipulate
#' @keywords city, region
#' @export makeRegions
#' @examples
#' makeRegions("data")
#' makeRegions()
#' makeRegions("renters")
#' makeRegions("famYYYY")
#' makeRegions("sizesYYYY")
#' makeRegions("owners")
#' makeRegions("writeout")
#' makeRegions("writeout5")
#' makeRegions("dropSmallTowns")
#' makeRegions("fixSizes")
#' makeRegions("load")
makeRegions <- function(arg = TRUE) {
        if (arg == "data") {

Data3 <- mergedData2 %>%
                          mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))

Data3$regionOne <- Data3$regionOne %>% as.factor

Data4 <<- Data3 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))

Data4$regionTwo <<- Data4$regionTwo %>% as.factor

return("Region columns added to data.")
 } else if (arg == "renters") {

Rent3 <- mergedRenters2 %>%
                          mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))

Rent3$regionOne <- Rent3$regionOne %>% as.factor

Rent4 <<- Rent3 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))

Rent4$regionTwo <<- Rent4$regionTwo %>% as.factor

return("Added region columns to renters.")
 } else if (arg == "famYYYY") {

fam2004 <- fam2004 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2005 <- fam2005 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2006 <- fam2006 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2007 <- fam2007 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2008 <- fam2008 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2009 <- fam2009 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2010 <- fam2010 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2011 <- fam2011 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2012 <- fam2012 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2013 <- fam2013 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2014 <- fam2014 %>%
plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))

#### add regionOne

fam2004 <- fam2004 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2005 <- fam2005 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2006 <- fam2006 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2007 <- fam2007 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2008 <- fam2008 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2009 <- fam2009 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2010 <- fam2010 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2011 <- fam2011 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2012 <- fam2012 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


fam2013 <- fam2013 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))



fam2014 <- fam2014 %>%
               mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


##   should be a factor
fam2004$regionTwo <<- fam2004$regionTwo %>% as.factor
fam2005$regionTwo <<- fam2005$regionTwo %>% as.factor
fam2006$regionTwo <<- fam2006$regionTwo %>% as.factor
fam2007$regionTwo <<- fam2007$regionTwo %>% as.factor
fam2008$regionTwo <<- fam2008$regionTwo %>% as.factor
fam2009$regionTwo <<- fam2009$regionTwo %>% as.factor
fam2010$regionTwo <<- fam2010$regionTwo %>% as.factor
fam2011$regionTwo <<- fam2011$regionTwo %>% as.factor
fam2012$regionTwo <<- fam2012$regionTwo %>% as.factor
fam2013$regionTwo <<- fam2013$regionTwo %>% as.factor
fam2014$regionTwo <<- fam2014$regionTwo %>% as.factor

##
fam2004$regionOne <<- fam2004$regionOne %>% as.factor
fam2005$regionOne <<- fam2005$regionOne %>% as.factor
fam2006$regionOne <<- fam2006$regionOne %>% as.factor
fam2007$regionOne <<- fam2007$regionOne %>% as.factor
fam2008$regionOne <<- fam2008$regionOne %>% as.factor
fam2009$regionOne <<- fam2009$regionOne %>% as.factor
fam2010$regionOne <<- fam2010$regionOne %>% as.factor
fam2011$regionOne <<- fam2011$regionOne %>% as.factor
fam2012$regionOne <<- fam2012$regionOne %>% as.factor
fam2013$regionOne <<- fam2013$regionOne %>% as.factor
fam2014$regionOne <<- fam2014$regionOne %>% as.factor

return("Added region columns to famYYYY.")
 } else if (arg == "owners") {


Own3 <- mergedOwners2 %>%
                          mutate(regionOne = case_when(
               CODELOC == "Jerusalem"      ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"    ~ 'Jerusalem',
               CODELOC == "Ashdod"         ~ 'HaDarom',
               CODELOC == "Ashkelon"       ~ 'HaDarom',
               CODELOC == "Beer Sheva"     ~ 'HaDarom',
               CODELOC == "Lod"            ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Nahariyya"      ~ 'HaZafon',
               CODELOC == "Haifa"          ~ 'Haifa',
               CODELOC == "Herzliyya"      ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo" ~ 'Tel Aviv',
               CODELOC == "Holon"          ~ 'Gush Dan',
               CODELOC == "Netanya"        ~ 'Gush Dan',
               CODELOC == "Rehovot"        ~ 'Gush Dan',
               CODELOC == "Ramla"          ~ 'Gush Dan',
               CODELOC == "Raannana"       ~ 'Gush Dan',
               CODELOC == "Bene Beraq"     ~ 'Gush Dan',
               CODELOC == "Bat Yam"        ~ 'Gush Dan',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'Gush Dan',
               CODELOC == "Rishon LeZiyon" ~ 'Gush Dan',
               CODELOC == "Ramat Gan"      ~ 'Gush Dan',
               CODELOC == "Modi'in"        ~ 'Gush Dan',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))

Own3$regionOne <- Own3$regionOne %>% as.factor

Own4 <<- Own3 %>%
                   plyr::mutate(regionTwo = case_when(
               CODELOC == "Jerusalem"     ~ 'Jerusalem',
               CODELOC == "Bet Shemesh"   ~ 'Jerusalem',
               CODELOC == "Ashdod"        ~ 'HaDarom',
               CODELOC == "Ashkelon"      ~ 'HaDarom',
               CODELOC == "Beer Sheva"    ~ 'HaDarom',
               CODELOC == "Lod"           ~ 'HaMerkaz',
               CODELOC == "Kefar Sava"    ~ 'HaMerkaz',
               CODELOC == "Nahariyya"     ~ 'HaZafon',
               CODELOC == "Haifa"         ~ 'Haifa',
               CODELOC == "Herzliyya"     ~ 'Tel Aviv',
               CODELOC == "Tel Aviv-Yaffo"~ 'Tel Aviv',
               CODELOC == "Holon"         ~ 'Gush Dan',
               CODELOC == "Netanya"       ~ 'HaMerkaz',
               CODELOC == "Rehovot"       ~ 'HaMerkaz',
               CODELOC == "Ramla"         ~ 'HaMerkaz',
               CODELOC == "Raannana"       ~ 'HaMerkaz',
               CODELOC == "Bene Beraq"     ~ 'Tel Aviv',
               CODELOC == "Bat Yam"        ~ 'Tel Aviv',
               CODELOC == "Kefar Sava"     ~ 'Gush Dan',
               CODELOC == "Petah Tiqwa"    ~ 'HaMerkaz',
               CODELOC == "Rishon LeZiyon" ~ 'HaMerkaz',
               CODELOC == "Ramat Gan"      ~ 'Tel Aviv',
               CODELOC == "Modi'in"        ~ 'HaMerkaz',
               CODELOC == "Hadera"         ~ 'Sharon',
               CODELOC == "Qirat Ata"      ~ 'Qrayot Haifa',
               CODELOC == "Qiryat Atta"    ~ 'Qrayot Haifa',
               TRUE                        ~  'Too Small - No Region'))


Own4$regionTwo <<- Own4$regionTwo %>% as.factor
return("Added region columns to owners")
 } else if (arg == "dropSmallTowns") {
keep <- c(  "Ashdod"  ,       "Jerusalem",      "Haifa",          "Tel Aviv-Yafo", "Bene Beraq",    "Bat Yam"     ,"Herzliyya"   ,  "Hadera"      , "Holon"     ,    "Kefar Sava"  ,  "Lod"           ,"Ashqelon"     , "Nettanya"  ,    "Petah Tiqwa" ,  "Rishon LeZiyon","Rehovot"      , "Ramla"     ,    "Ramat Gan"   ,  "Raannana"      ,"Beer Sheva"   , "Modi'in"   ,    "Bet Shemesh" ,  "Tel Aviv-Yaffo","Ashkelon"     , "Netanya"   ,    "Nahariyya"   ,  "Giv'atayim"    ,"Qiryat Atta"  , "Qiryat Ata")

Data4[]   <<- lapply(Data4, unclass)
Rent4[]   <<- lapply(Rent4, unclass)
 Own4[]   <<- lapply(Own4, unclass)
## 1.    Drop Small Towns that we don't know which region they are in.
Rent4 <- Rent4[!Rent4$CODELOC == "Less than 100,000 inhabitants", ]
Rent4 <<- droplevels(Rent4[!Rent4$CODELOC == "Less than 50,000 inhabitants", ])

Own4 <- Own4[!Own4$CODELOC == "Less than 100,000 inhabitants", ]
Own4 <<- droplevels(Own4[!Own4$CODELOC == "Less than 50,000 inhabitants", ])


Data4 <- Data4[!Data4$CODELOC == "Less than 100,000 inhabitants", ]
Data4 <<- droplevels(Data4[!Data4$CODELOC == "Less than 50,000 inhabitants", ])

return("droped locations of uncertain region")
 } else if (arg == "sizesYYYY") {

fam2004 <- fam2004 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))

fam2005 <- fam2005 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2006 <- fam2006 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2007 <- fam2007 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2008 <- fam2008 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2009 <- fam2009 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


   fam2010 <- fam2010 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2011 <- fam2011 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2012 <- fam2012 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2013 <- fam2013 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


fam2014 <- fam2014 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


return("Added $size column to famYYYY before making familiesList")

 } else if (arg == "fixSizes") {

Data4 <- subset(Data4, select = -size)
Rent4 <- subset(Rent4, select = -size)
Own4 <-  subset(Own4,  select = -size)

Data5 <<- Data4 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))

Rent5 <<- Rent4 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))


Own5 <<- Own4 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4                ~ 'large'
                          ))

return("Only 4 sizes, not 5.")
 } else if (arg == "writeout") {
saveRDS(Data4, "../savedData/Data4.rds")
saveRDS(Rent4, "../savedData/Rent4.rds")
saveRDS(Own4, "../savedData/Own4.rds")
return("Saved renters, owners, all (with region column) to disk.")
 } else if (arg == "writeout5") {
saveRDS(Data5, "../savedData/Data5.rds")
saveRDS(Rent5, "../savedData/Rent5.rds")
saveRDS(Own5,  "../savedData/Own5.rds")
return("Saved renters, owners, all (with region column) to disk.")

 } else if (arg == "load") {
## Data4 needs to be re-written, error reading from connection

Data4 <<- readRDS("../savedData/Data4.rds")
Rent4 <<- readRDS("../savedData/Rent4.rds")
Own4  <<- readRDS("../savedData/Own4.rds")
return("loaded data including region columns")
  } else {
return("those args not yet implemented.")
  }
}

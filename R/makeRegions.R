#' Add a region column based on $CODELOC (city) in the original data.
#' v. 3.65
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
#' makeRegions("dropSmallV2")
#' makeRegions("renters")
#' makeRegions("famYYYY")
#' makeRegions("famYYYYa")
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
 } else if (arg == "famYYYYa") {

#### add regionOne

fam2004a <- fam2004a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2005a <- fam2005a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2006a <- fam2006a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2007a <- fam2007a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2008a <- fam2008a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2009a <- fam2009a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2010a <- fam2010a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2011a <- fam2011a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2012a <- fam2012a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2013a <- fam2013a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)



fam2014a <- fam2014a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)

   ## add regionTwo column
   ## send to namespace as famYYYYb
fam2004b <<- fam2004a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2005b <<- fam2005a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2006b <<- fam2006a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2007b <<- fam2007a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2008b <<- fam2008a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2009b <<- fam2009a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2010b <<- fam2010a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2011b <<- fam2011a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2012b <<- fam2012a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2013b <<- fam2013a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)


fam2014b <<- fam2014a %>%
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
               TRUE                        ~  'Too Small - No Region') %>% factor)
return("Added regionOne and regionTwo columns to famYYYYb.")

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

return("sucess - famYYYY old version")

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
 } else if (arg == "dropSmallV2") {
## this function()  drops regionOne == "Too Small - No Region"

   keepRegions <- c( "Gush Dan",  ## 1
                    "HaDarom",    ## 2
                    "Haifa",      ## 3
                    "HaMerkaz",   ## 4
                    "HaZafon",    ## 5
                    "Jerusalem",  ## 6
                    "Sharon",     ## 7
                    "Tel Aviv")   ## 8
   ##   "Too Small - No Region"   ## 9

     fam2004d <<- droplevels(dplyr::filter(fam2004c,   regionOne %in% keepRegions))
     fam2005d <<- droplevels(dplyr::filter(fam2005c,   regionOne %in% keepRegions))
     fam2006d <<- droplevels(dplyr::filter(fam2006c,   regionOne %in% keepRegions))
     fam2007d <<- droplevels(dplyr::filter(fam2007c,   regionOne %in% keepRegions))
     fam2008d <<- dplyr::filter(fam2008c,   regionOne %in% keepRegions)
     fam2009d <<- dplyr::filter(fam2009c,   regionOne %in% keepRegions)
     fam2010d <<- droplevels(dplyr::filter(fam2010c,   regionOne %in% keepRegions))
     fam2011d <<- dplyr::filter(fam2011c,   regionOne %in% keepRegions)
     fam2012d <<- droplevels(dplyr::filter(fam2012c,   regionOne %in% keepRegions))
     fam2013d <<- droplevels(dplyr::filter(fam2013c,   regionOne %in% keepRegions))
     fam2014d <<- droplevels(dplyr::filter(fam2014c,   regionOne %in% keepRegions))


fam2008d$regionOne <<- droplevels(fam2008d$regionOne)
fam2008d$regionTwo <<- droplevels(fam2008d$regionTwo)

fam2009d$regionOne <<- droplevels(fam2009d$regionOne)
fam2009d$regionTwo <<- droplevels(fam2009d$regionTwo)

fam2011d$regionOne <<- droplevels(fam2011d$regionOne)
fam2011d$regionTwo <<- droplevels(fam2011d$regionTwo)

return("dropped locations of uncertain regionality.")
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

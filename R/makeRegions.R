#' Add a region column based on $CODELOC (city) in the original data.
#' v. 0.35
#' This function adds columns $regionOne and $regionTwo
#' regionOne puts 13 cities in Gush Dan.
#' regionTwo puts  2 cities in Gush Dan.
#' @param arg The arg which you will perform the function on.
#' @family manipulate
#' @keywords city, region
#' @export makeRegions
#' @examples
#' makeRegions()
#' makeRegions("data")
#' makeRegions("renters")
#' makeRegions("owners")
#' makeRegions("writeout")
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
               TRUE                        ~  'Too Small - No Region'))

Rent4$regionTwo <<- Rent4$regionTwo %>% as.factor


return("Added region columns to renters.")
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
               TRUE                        ~  'Too Small - No Region'))

Own4$regionTwo <<- Own4$regionTwo %>% as.factor

return("Added region columns to owners")
 } else if (arg == "writeout") {
saveRDS(Data4, "../savedData/Data4.rds")
saveRDS(Rent4, "../savedData/Rent4.rds")
saveRDS(Own4, "../savedData/Own4.rds")
return("Saved renters, owners, all (with region column) to disk.")
 } else if (arg == "load") {
Data4 <- readRDS("../savedData/Data4.rds")
Rent4 <- readRDS("../savedData/Rent4.rds")
Own4  <- readRDS("../savedData/Own4.rds")
return("loaded data including region columns")
  } else {
return("those args not yet implemented.")
  }
}

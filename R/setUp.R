#' Secondary setup code, replaces setupV7
#' This function wraps setup code
#' @param arg Defaults to 4.
#' @family setup
#' @keywords setup
#' @export setUp
#' @examples
#' setUp()
#' setUp(1)
#' setUp(2)
#' setUp(3)
#' setUp(4)
setUp <- function(arg = 4) {
  if(arg ==1){
                maxReproducible(3)
                subsetCities()
                mergeFrames("saved")
return("merged data from all years to 1 dataframe")

  }else if(arg == 2){
## none of these vars are exported, why run?

        regionList <- c(
                         "Jerusalem",
                         "Tel Aviv",
                         "Haifa",
                          "GushDan",
                          "HaMerkaz",
                          "HaDarom",
                          "Sharon",
                          "HaZafon",
                          "Qrayot Haifa")

regionOrder <- c(
  regionList[1], # Jeru = Jeru
  regionList[6], # https://en.wikipedia.org/wiki/Ashdod
  regionList[3], # Haifa = Haifa
  regionList[2], #https://en.wikipedia.org/wiki/Herzliya
  regionList[4], #https://en.wikipedia.org/wiki/Holon
  regionList[5], # https://en.wikipedia.org/wiki/Lod
  regionList[7], #https://en.wikipedia.org/wiki/Hadera
                 ## Hadera - Sharon or Haifa ?
  regionList[6], # http://en.wikipedia.org/wiki/Ashkelon
  regionList[4], # http://en.wikipedia.org/wiki/Netanya
  regionList[4], # http://en.wikipedia.org/wiki/Rehovot
  regionList[4], # http://en.wikipedia.org/wiki/Ramla
  regionList[4], # http://en.wikipedia.org/wiki/Ra%27annana
  regionList[2], # http://en.wikipedia.org/wiki/Tel-Aviv-Yaffo
  regionList[4], # http://en.wikipedia.org/wiki/Beni_Brak
  regionList[4], # http://en.wikipedia.org/wiki/Bat_Yam
  regionList[4], # http://en.wikipedia.org/wiki/Kfar_Saba
  regionList[4], # http://en.wikipedia.org/wiki/Petah_Tikva
  regionList[4], # http://en.wikipedia.org/wiki/Rishon_LeZion
  regionList[4], # http://en.wikipedia.org/wiki/Ramat-Gan
  regionList[6], # http://en.wikipedia.org/wiki/Beer-Sheva
  regionList[4], # http://en.wikipedia.org/wiki/Modi%27in
  regionList[1], # http://en.wikipedia.org/wiki/Beit-Shemesh
  regionList[8], # http://en.wikipedia.org/wiki/Nahariyya
  regionList[9], # http://en.wikipedia.org/wiki/Kiryat_Ata
  regionList[4]  # http://en.wikipedia.org/wiki/Givatayim
                 ## Givatayim - Gush Dan or TLV ?
                 ## https://en.wikipedia.org/wiki/Gush_Dan
                )
regionOrderTwo <-c(

 regionList[1], # Jeru = Jeru
  regionList[6], # https://en.wikipedia.org/wiki/Ashdod
  regionList[3], # Haifa = Haifa
  regionList[2], #https://en.wikipedia.org/wiki/Herzliya
  regionList[4], #https://en.wikipedia.org/wiki/Holon
  regionList[5], # https://en.wikipedia.org/wiki/Lod
  regionList[7], #https://en.wikipedia.org/wiki/Hadera
                 ## Hadera - Sharon or Haifa ?
  regionList[6], # http://en.wikipedia.org/wiki/Ashkelon
  regionList[5], # http://en.wikipedia.org/wiki/Netanya
  regionList[5], # http://en.wikipedia.org/wiki/Rehovot
  regionList[5], # http://en.wikipedia.org/wiki/Ramla
  regionList[5],# http://en.wikipedia.org/wiki/Ra%27annana
  regionList[2],# http://en.wikipedia.org/wiki/Tel-Aviv-Yaffo
  regionList[2],# http://en.wikipedia.org/wiki/Beni_Brak
  regionList[2], # http://en.wikipedia.org/wiki/Bat_Yam
   regionList[5], # http://en.wikipedia.org/wiki/Kfar_Saba
   regionList[5], # http://en.wikipedia.org/wiki/Petah_Tikva
   regionList[5], # http://en.wikipedia.org/wiki/Rishon_LeZion
   regionList[2], # http://en.wikipedia.org/wiki/Ramat-Gan
   regionList[6], # http://en.wikipedia.org/wiki/Beer-Sheva
   regionList[5], # http://en.wikipedia.org/wiki/Modi%27in
   regionList[1], # http://en.wikipedia.org/wiki/Beit-Shemesh
   regionList[8], # http://en.wikipedia.org/wiki/Nahariyya
   regionList[9], # http://en.wikipedia.org/wiki/Kiryat_Ata
   regionList[4] # http://en.wikipedia.org/wiki/Givatayim
  ## Givatayim - Gush Dan or TLV ?
 ## GushDan is a conurbation (technical geographer speech) of areas in the
 ## TLV and Central Districts (District is the administrative term like state or county)
 ## that have grown together
 ## regionOrderTwo places cities into TLV or Center / regionOrder places them in Gush Dan
)
## count cities in each region
cities_in_region <- c(2,
                      3,
                      1,
                      2,
                      13,
                      1,
                      1,
                      3,
                      13,
                      13,
                      13,
                      13,
                      2,
                      13,
                      13,
                      13,
                      13,
                      13,
                      13,
                      3,
                      13,
                      2,
                      1,
                      1,
                      13)
return("listed cities in their regions")
  }else if(arg == 3){
return("not implemented 3")
  }else if(arg == 4){
       subsetCities()
    maxReproducible(6)
return("ran maxReproducible(r) and subsetCities() (needed for n=tables)")
  }else{
return("not implemented ...")
  }
}

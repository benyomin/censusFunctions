#' dlplethr v.0.3
#'
#' This function modifies a map of Israel, adding the Golan.
#' @keywords import, map, choropleth, choroplethr, region, GIS
#' @export
#' @examples
#' dlplethr("raw")
#' dlplethr("saved")

dlplethr <- function(source = "saved"){     #defaults to saved version

  ## set variables outside of the if/else structure
  options(HTTPUserAgent = "Mozilla/5.0 (Windows NT 6.1; WOW64)")
  ## use a specific commit rather than head
  admin1map_url <- "https://github.com/arilamstein/choroplethrAdmin1/raw/62d6168aa29ce3391c356b0a077a9e972ccf6201/data/admin1.map.rdata"

  if  (source == "raw"){
    #load("/Users/AbuDavid/scratch/R/choroplethrAdmin1/data/admin1.map.rdata")
  ## f <-   CFILE("../rawData/maps/admin1.map.rdata", mode = "wb")  ##creates a file in which to write downloaded data.
  ##        curlPerform(url = admin1map_url, verbose = TRUE, useragent = getOption("HTTPUserAgent"), 
  ##            writedata = f@ref)
  ##       RCurl::close(f)
    library("RCurl")
    arymap <<- getUrl(admin1map_url)
dlplethr("write")
  } else if (source == "saved") {
        if (file.exists("../rawData/maps/admin1.map.rdata")) {
          load("../rawData/maps/admin1.map.rdata")
        } else {
    return ("09l;-99n")
 }} else if (source == "write"){
    ## implement write out function
          save(arymap, file = "../rawData/maps/admin1.map.rdata")
  } else {
  return ("8al-99n")
  }
}

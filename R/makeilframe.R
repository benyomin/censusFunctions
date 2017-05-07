#' makeilframe v.0.4
#'
#' This function is a wrapper around dlplethr and modplethr.
#' @keywords import, map, choropleth, choroplethr, region, GIS
#' @export
#' @examples
#' makeilframe("raw")
#' makeilframe("saved")
#' makeilframe("alt")

makeilframe <- function(x = "saved" ) {
        if(x == "raw"  ){
          ## not-implemented - download of the raw
          ## file from github fails
          ## dlplethr("raw")
          dlplethr("saved")
          modplethr("raw")
          modplethr("write")
print("This may have overwritten the downloaded version of
the file used by AcademikUser.")
  }else if(x == "saved"){
          modplethr("saved")
  }else if(x == "alt"){
          dlplethr("saved")
          modplethr("raw")
          modplethr("writeAgain")
   return("try this way")
  }else{
        return("error oy9ral9s")
  }
}

#' makeilframe v.0.3
#'
#' This function is a wrapper around dlplethr and modplethr.
#' @keywords import, map, choropleth, choroplethr, region, GIS
#' @export
#' @examples
#' makeilframe("raw")
#' makeilframe("saved")

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
  }else{
        return("error oy9ral9s")
  }
}

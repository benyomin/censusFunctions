#' Loads the required packages for the
#' Housing Wealth Effect Analysis.
#' Provides for simple dependency management to aid in reproducibility.
#' v.March 12-2017 - remove gdata dependency, use other xlsx reader
#' v.Jan12-2017 - changes to tidyverse instead of tidyr
#' March 5 - rio for csv import of stock prices
#' @param load Defaults to TRUE.
#' @keywords packages, packageManagement, dependencies, library, require, requirements
#' @export
#' @examples
#' loadWealthEffectPackages()
loadWealthEffectPackages <- function(load = TRUE) {
    if (load == TRUE) {
        # install_if_needed('reshape2') mapping
        devtools::install_github('mtennekes/tmap')
library(tmap)
        # devtools::install_github('walkerke/tigris') library(tigris) library(tmapn)
        # other
library(reshape2)
        install_github("dkahle/ggmap")
library("ggmap")
        install_if_needed("rio")
library(rio)
        install_if_needed("roxygen2")
library(roxygen2)
        install_if_needed("foreign")
library(foreign)
        install_if_needed("rJava")
library(rJava)
        install_if_needed("xlsx")
library(xlsx)
        install_if_needed("RCurl")
library(RCurl)
        install_if_needed("zoo")
library(zoo)
        install_if_needed("dtplyr")
library(dtplyr)
        install_if_needed("plm")  ## linear models for panel data ## https://r-forge.r-project.org/projects/plm/
library(plm)
        install_if_needed("choroplethr")
install_github("choroplethrAdmin1", "arilamstein")
        devtools::install_github("r-spatial/mapview@develop")
library(mapview)
 #     install_if_needed("choroplethrAdmin1")
library(choroplethr)
library(choroplethrAdmin1)
        # install_if_needed('gdata') ## causes thesisdown to fail library(gdata)
        # install_if_needed('operators') library(operators) install_if_needed('grid')
        # library(grid)
        devtools::install_github("hadley/xml2")
        devtools::install_github("gaborcsardi/crayon")
        devtools::install_github("rsheets/linen")
        devtools::install_github("rsheets/rexcel")
library(rexcel)
        install_if_needed("bookdown")
library(bookdown)
        install_if_needed("scales")
library(scales)
        devtools::install_github("therneau/survival")  ## Hmisc needs updated version
        install_if_needed("Hmisc")
library(Hmisc)
        install_if_needed("memisc")
library(memisc)
    } else {
        print("Helper Packages not loaded by censusFunctions.")
    }
}

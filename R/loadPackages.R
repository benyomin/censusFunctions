#' Loads the required packages for the
#' Housing Wealth Effect Analysis.   v. 3.0
#' Provides for simple dependency management to aid in reproducibility.
#' v.1.2 - remove gdata dependency, use other xlsx reader
#' v.2.0 - changes to tidyverse instead of tidyr
#' v.2.1 - rio for csv import of stock prices
#' v.2.7 - plyr to dplyr, add magrittr
#' @param load Defaults to TRUE.
#' @keywords packages, packageManagement, dependencies, library, require, requirements
#' @export
#' @examples
#' loadWealthEffectPackages()
loadWealthEffectPackages <- function(load = TRUE) {
    if (load == TRUE) {
        # install_if_needed('reshape2') mapping
        # devtools::install_github('walkerke/tigris') library(tigris) library(tmapn)
        # other
        # install_if_needed("ggplot2")
        #library(ggplot2)
#      install.packages("tidyverse")
install.packages("jpeg")   ## required for ggmap

library("tidyverse")
#      devtools::install_github('mtennekes/tmap')
library(tmap)
#      install_github("dkahle/ggmap")
library(ggmap)
#      install_if_needed("RColorBrewer")
library(RColorBrewer)
#      install_if_needed("pander")        ## replacement for kable, easy captions
library(pander)
#      devtools::install_github("renkun-ken/pipeR")
library(pipeR)
#      install_if_needed("magrittr")
library(magrittr)
#      install_if_needed("ggthemes")
library(ggthemes)
library(rgeos)
#      install_if_needed("reshape")
library(reshape)
#      install_if_needed("reshape2")
library(reshape2)
#      install_if_needed("maps")
library(maps)
#      install_if_needed("rio")
library(rio)
#      install_if_needed("roxygen2")
library(roxygen2)
#      install_if_needed("foreign")
library(foreign)
#      install_if_needed("tidyr")
#      library(tidyr)
#       install_if_needed("rgdal")
library(rgdal)
#       install_if_needed("rgeos")
library(rgeos)
#       install_if_needed("rJava")
# library(rJava)
#       install_if_needed("xlsx")
# library(xlsx)
#       install_if_needed("RCurl")
library(RCurl)
#        install_if_needed("zoo")
library(zoo)                       # dates package, used to manipulate quarters
     ##   install_if_needed("dtplyr")
#        devtools::install_github("hadley/dtplyr")
library(dtplyr)
      ##  install_if_needed("dplyr")          ## count()
      ##  install from source to prevent a warning about the binary being
      ##  compiled with R 3.4.1 when I am using R 3.4
      ## library(dplyr)
#        install_if_needed("plm")  ## linear models for panel data ## https://r-forge.r-project.org/projects/plm/
library(plm)
#        install_if_needed("choroplethr")
#        install_github("choroplethrAdmin1", "arilamstein")
#        devtools::install_github("r-spatial/mapview@develop")
library(mapview)
library(choroplethr)
library(choroplethrAdmin1)
#       install_if_needed('operators') library(operators)
#       install_if_needed('proxy')
library(proxy)
#       install_if_needed('grid')
library(grid)
#        devtools::install_github("hadley/xml2")
#        devtools::install_github("gaborcsardi/crayon")
#        devtools::install_github("rsheets/linen")
#        devtools::install_github("rsheets/rexcel")
library(rexcel)
#        install_if_needed("readxl")
library(readxl)
#        install_if_needed("bookdown")
library(bookdown)
#        install_if_needed("scales")
library(scales)
#        devtools::install_github("therneau/survival")  ## Hmisc needs updated version
#        install_if_needed("Hmisc")
library(Hmisc)
#        install_if_needed("memisc")
library(memisc)
#        install_github("edzer/sp")
library(sp)
    } else {
        print("Helper Packages not loaded by censusFunctions.")
    }
}

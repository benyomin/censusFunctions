#' Loads the required packages for the
#' Housing Wealth Effect Analysis.   v. 3.2
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
#' loadWealthEffectPackages("library")
#' loadWealthEffectPackages("install")
loadWealthEffectPackages <- function(load = "library") {
    if (load == "install") {
        # install_if_needed('reshape2') mapping
        # devtools::install_github('walkerke/tigris') library(tigris) library(tmapn)
        # other
        # install_if_needed("ggplot2")
        #library(ggplot2)
#      install.packages("tidyverse")
#      install.packages("jpeg")   ## required for ggmap

#      devtools::install_github('mtennekes/tmap')
#      install_github("dkahle/ggmap")
#      install_if_needed("RColorBrewer")
#      install_if_needed("pander")        ## replacement for kable, easy captions
#      devtools::install_github("renkun-ken/pipeR")
#      install_if_needed("magrittr")
#      install_if_needed("ggthemes")
#      install_if_needed("reshape")
#      install_if_needed("reshape2")
#      install_if_needed("maps")
#      install_if_needed("rio")
#      install_if_needed("roxygen2")
#      install_if_needed("foreign")
#      install_if_needed("tidyr")
#      library(tidyr)
#       install_if_needed("rgdal")
#       install_if_needed("rgeos")
#       install_if_needed("rJava")
# library(rJava)
#       install_if_needed("xlsx")
# library(xlsx)
#       install_if_needed("RCurl")
#        install_if_needed("zoo")
     ##   install_if_needed("dtplyr")
#        devtools::install_github("hadley/dtplyr")
   ##  install_if_needed("dplyr")          ## count()
      ##  install from source to prevent a warning about the binary being
      ##  compiled with R 3.4.1 when I am using R 3.4
   ## library(dplyr)
   ##      install_if_needed("plm")  ## linear models for panel data
                                     ## https://r-forge.r-project.org/projects/plm/
#        install_if_needed("choroplethr")
#        install_github("choroplethrAdmin1", "arilamstein")
#        devtools::install_github("r-spatial/mapview@develop")
#       install_if_needed('operators') library(operators)
#       install_if_needed('proxy')
#       install_if_needed('grid')
#        devtools::install_github("hadley/xml2")
#        devtools::install_github("gaborcsardi/crayon")
#        devtools::install_github("rsheets/linen")
#        devtools::install_github("rsheets/rexcel")
#        install_if_needed("readxl")
#        install_if_needed("bookdown")
#        install_if_needed("scales")
#        devtools::install_github("therneau/survival")  ## Hmisc needs updated version
#        install_if_needed("Hmisc")
#        install_if_needed("memisc")
#        install_github("edzer/sp")
censusFunctions::install_if_needed("rgdal")
censusFunctions::install_if_needed("tidyverse")
censusFunctions::install_if_needed("roxygen2")
censusFunctions::install_if_needed("tmap")
censusFunctions::install_if_needed("foreign")
censusFunctions::install_if_needed('ggmap')
censusFunctions::install_if_needed('rgeos')
censusFunctions::install_if_needed('RColorBrewer')
censusFunctions::install_if_needed('zoo') # dates package
                                         ## used to manipulate quarters
censusFunctions::install_if_needed('RCurl')
censusFunctions::install_if_needed('pander')
censusFunctions::install_if_needed('dtplyr')
censusFunctions::install_if_needed('pipeR')
censusFunctions::install_if_needed('plm')
censusFunctions::install_if_needed('magrittr')
censusFunctions::install_if_needed('mapview')
censusFunctions::install_if_needed('ggthemes')
censusFunctions::install_if_needed('rgeos')
censusFunctions::install_if_needed('choroplethr')
censusFunctions::install_if_needed('reshape')
censusFunctions::install_if_needed('choroplethrAdmin1')
censusFunctions::install_if_needed('reshape2')
censusFunctions::install_if_needed('proxy')
censusFunctions::install_if_needed('maps')
censusFunctions::install_if_needed('grid')
censusFunctions::install_if_needed('rio')
censusFunctions::install_if_needed('rexcel')
censusFunctions::install_if_needed('readxl')
censusFunctions::install_if_needed('bookdown')
censusFunctions::install_if_needed('scales')
censusFunctions::install_if_needed('Hmisc')
censusFunctions::install_if_needed('memisc')
censusFunctions::install_if_needed('sp')
    } else if (load == "library") {
library(rgdal)
library(tidyverse)
library(roxygen2)
library(tmap)
library(foreign)
library(ggmap)
library(rgeos)
library(RColorBrewer)
library(zoo)                       # dates package, used to manipulate quarters
library(RCurl)
library(pander)
library(dtplyr)
library(pipeR)
library(plm)
library(magrittr)
library(mapview)
library(ggthemes)
library(rgeos)
library(choroplethr)
library(reshape)
library(choroplethrAdmin1)
library(reshape2)
library(proxy)
library(maps)
library(grid)
library(rio)
library(rexcel)
library(readxl)
library(bookdown)
library(scales)
library(Hmisc)
library(memisc)
library(sp)
    } else {
        print("Helper Packages not loaded by censusFunctions.")
    }
}

#' modplethr v.0.3
#'
#' This function modifies a map of Israel, adding the Golan.
#' @keywords import, map, choropleth, choroplethr, region, GIS
#' @export
#' @examples
#' modplethr("raw")
#' modplethr("saved")
#' modplethr("write")

modplethr <- function(args = "saved"){
## load libraries
  library(maptools)
  library(rgeos)
if(args == "raw"){
## mod Golan
states <- readOGR("../includes/ISR_adm/ISR_adm1.shp", "ISR_adm1")
Golan <- subset(states, NAME_1 == "Golan")
gpoly <- Golan@polygons    ## works
Golan@polygons[1] ## works
typeof(gpoly)
str(gpoly)
str(gpoly[[1]])

insidepoly <-gpoly[[1]]
insidepoly@Polygons[1]   ## useful, closer
deepinside <- insidepoly@Polygons[[1]]
str(deepinside)
deepwhy    <- deepinside@coords[,1]  ## see docs, is y lat or long?
deepex     <- deepinside@coords[,2]
golan_lat  <- deepwhy
golan_long <- deepex

golanframe <- as.data.frame(cbind(long = deepwhy, lat = deepex))
#golanframe
## create columns in same order as choroplethrAdmin1 package
golanframe$admin  <- "israel"
golanframe$region <- "golan"
golanframe$group <- 1884.1  #probably overlaps with Italy
alist <- seq(1, 166, 1)     #166 is number of lat, long pairs in golan
numberofjeru <- 272989
blist <- alist + numberofjeru
blist
golanframe$order <- blist
golanframe$hole  <- FALSE
golanframe$piece <- 1
golanframe$id    <- 1878

capitals <- readOGR("../includes/ISR_adm_edited/ISR_adm1.shp", "ISR_adm1")
levels(capitals$NAME_1)

qrayot <- subset(capitals, NAME_1 == "QrayotHaifa")
sharon <- subset(capitals, NAME_1 == "Sharon")

qrayotpoly <- qrayot@polygons
sharonpoly <- sharon@polygons

insideqrayot <- qrayotpoly[[1]]
insidesharon <- sharonpoly[[1]]

deepinsideqrayot <- insideqrayot@Polygons[[1]]
deepinsidesharon <- insidesharon@Polygons[[1]]

qrayotlong <- deepinsideqrayot@coords[, 1]
qrayotlat  <- deepinsideqrayot@coords[, 2]

sharonlong <- deepinsidesharon@coords[, 1]
sharonlat  <- deepinsidesharon@coords[, 2]

### create frames
qrayotframe <- as.data.frame(cbind(long = qrayotlong,
                                   lat  = qrayotlat,
                                   admin = "israel",
                                   region = "qrayot",
                        group = 1885.1,   # probably overlaps w/ italy
                                   order = 0,
                                   hole  = FALSE,
                                   piece = 1,
                                   id    = 1885
                                   ))

sharonframe <- as.data.frame(cbind(long   = sharonlong,
                                   lat    = sharonlat,
                                   admin  = "israel",
                                   region = "sharon",
                       group = 1886.1,   # probably overlaps w/ italy
                                   order = 0,
                                   hole  = FALSE,
                                   piece = 1,
                                   id    = 1886
                                   ))

### fix order = i++ column
golanend <- numberofjeru + 166
qrayotend <- golanend + length(qrayotlong)

clist <- seq(1, length(qrayotlong),  1)
dlist <- golanend +  clist
qrayotframe$order <- dlist

elist <- seq(1, length(sharonlong),  1)
flist <- qrayotend + elist
sharonframe$order <- flist

library(choroplethrAdmin1)
library(magrittr)
### subset
medina      <- "israel"
auldframe   <- admin1.map[which(admin1.map$admin == medina), ]
#length(auldframe)
#auldframe

#check before combining
#names(auldframe)
#names(golanframe)

# combine
ilframe <- rbind(auldframe, golanframe, qrayotframe, sharonframe)


}else if (args == "write"){

# writeout data  # http://www.cookbook-r.com/Data_input_and_output/Writing_data_to_a_file/
write.csv(ilframe, file =   "../includes/ilregions.csv", row.names = FALSE)

}else if (args == "writeAgain"){
save(ilframe, file="../includes/admin1.map.rdata")  ## only 1 country now


}else if (args == "saved"){

# import saved data # http://rprogramming.net/read-csv-in-r/
ilframe <- read.csv(file = "../includes/ilregions.csv", header = TRUE, sep = ",")
return(ilframe)
}else{
return("not implemented oy9arsldka")
     }
}


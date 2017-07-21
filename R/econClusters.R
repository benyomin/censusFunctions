#' subsets by econ cluster
#' @param arg nil
#' @keywords econ level, subset
#' @export econClusters
#' @examples
#' econCluster()

econClusters <- function(arg) {
  ## was in chapter 3
  ## levels(family2004$cluster)
## count(subset(family2010,cluster=="Highest"))
## count(subset(family2010,cluster=="Lowest"))
## only 1 in 10 households are labelled highest or lowest, 9/10 are NA
highestHouseholds <<- c(count(subset(family2004,cluster=="Highest")),
                          count(subset(family2005,cluster=="Highest")),
                          count(subset(family2006,cluster=="Highest")),
                          count(subset(family2007,cluster=="Highest")),
                          count(subset(family2008,cluster=="Highest")),
                          count(subset(family2009,cluster=="Highest")),
                          count(subset(family2010,cluster=="Highest")),
                          count(subset(family2011,cluster=="Highest")),
                          count(subset(family2012,cluster=="Highest")),
                          count(subset(family2013,cluster=="Highest")),
                          count(subset(family2014,cluster=="Highest")))

lowestHouseholds  <<- c(count(subset(family2004,cluster=="Lowest")),
                          count(subset(family2005,cluster=="Lowest")),
                          count(subset(family2006,cluster=="Lowest")),
                          count(subset(family2007,cluster=="Lowest")),
                          count(subset(family2008,cluster=="Lowest")),
                          count(subset(family2009,cluster=="Lowest")),
                          count(subset(family2010,cluster=="Lowest")),
                          count(subset(family2011,cluster=="Lowest")),
                          count(subset(family2012,cluster=="Lowest")),
                          count(subset(family2013,cluster=="Lowest")),
                          count(subset(family2014,cluster=="Lowest")))

middleHouseholds  <<- c(count(subset(family2004,is.na(cluster))),
                          count(subset(family2005,is.na(cluster))),
                          count(subset(family2006,is.na(cluster))),
                          count(subset(family2007,is.na(cluster))),
                          count(subset(family2008,is.na(cluster))),
                          count(subset(family2009,is.na(cluster))),
                          count(subset(family2010,is.na(cluster))),
                          count(subset(family2011,is.na(cluster))),
                          count(subset(family2012,is.na(cluster))),
                          count(subset(family2013,is.na(cluster))),
                          count(subset(family2014,is.na(cluster))))


   clusterTable <<-cbind(year,lowestHouseholds,middleHouseholds,highestHouseholds)
 }

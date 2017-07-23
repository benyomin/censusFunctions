#' subsets by econ cluster v: 2.0
#' @param arg nil
#' @keywords econ level, subset
#' @export econClusters
#' @examples
#' econCluster()
#' econCluster(TRUE)

econClusters <- function(arg = TRUE) {
  ## was in chapter 3
  ## levels(family2004$cluster)
## count(subset(family2010,cluster=="Highest"))
## count(subset(family2010,cluster=="Lowest"))
## only 1 in 10 households are labelled highest or lowest, 9/10 are NA
   highestHouseholds <- c(count(subset(familiesList[[1]],cluster=="Highest")),
                          count(subset(familiesList[[2]],cluster=="Highest")),
                          count(subset(familiesList[[3]],cluster=="Highest")),
                          count(subset(familiesList[[4]],cluster=="Highest")),
                          count(subset(familiesList[[5]],cluster=="Highest")),
                          count(subset(familiesList[[6]],cluster=="Highest")),
                          count(subset(familiesList[[7]],cluster=="Highest")),
                          count(subset(familiesList[[8]],cluster=="Highest")),
                          count(subset(familiesList[[9]],cluster=="Highest")),
                          count(subset(familiesList[[10]],cluster=="Highest")),
                          count(subset(familiesList[[11]],cluster=="Highest")))

   lowestHouseholds  <- c(count(subset(familiesList[[1]],cluster=="Lowest")),
                          count(subset(familiesList[[2]],cluster=="Lowest")),
                          count(subset(familiesList[[3]],cluster=="Lowest")),
                          count(subset(familiesList[[4]],cluster=="Lowest")),
                          count(subset(familiesList[[5]],cluster=="Lowest")),
                          count(subset(familiesList[[6]],cluster=="Lowest")),
                          count(subset(familiesList[[7]],cluster=="Lowest")),
                          count(subset(familiesList[[8]],cluster=="Lowest")),
                          count(subset(familiesList[[9]],cluster=="Lowest")),
                          count(subset(familiesList[[10]],cluster=="Lowest")),
                          count(subset(familiesList[[11]],cluster=="Lowest")))

   middleHouseholds  <- c(count(subset(familiesList[[1]],is.na(cluster))),
                          count(subset(familiesList[[2]],is.na(cluster))),
                          count(subset(familiesList[[3]],is.na(cluster))),
                          count(subset(familiesList[[4]],is.na(cluster))),
                          count(subset(familiesList[[5]],is.na(cluster))),
                          count(subset(familiesList[[6]],is.na(cluster))),
                          count(subset(familiesList[[7]],is.na(cluster))),
                          count(subset(familiesList[[8]],is.na(cluster))),
                          count(subset(familiesList[[9]],is.na(cluster))),
                          count(subset(familiesList[[10]],is.na(cluster))),
                          count(subset(familiesList[[11]],is.na(cluster))))

   clusterTable <<-cbind(year,lowestHouseholds,middleHouseholds,highestHouseholds)
 }

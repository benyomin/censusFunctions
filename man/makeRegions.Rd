% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeRegions.R
\name{makeRegions}
\alias{makeRegions}
\title{Add a region column based on $CODELOC (city) in the original data.
v. 3.65
This function adds columns $regionOne and $regionTwo
regionOne puts 13 cities in Gush Dan.
regionTwo puts  2 cities in Gush Dan.}
\usage{
makeRegions(arg = TRUE)
}
\arguments{
\item{arg}{The arg which you will perform the function on.}
}
\description{
Add a region column based on $CODELOC (city) in the original data.
v. 3.65
This function adds columns $regionOne and $regionTwo
regionOne puts 13 cities in Gush Dan.
regionTwo puts  2 cities in Gush Dan.
}
\examples{
makeRegions("data")
makeRegions()
makeRegions("dropSmallV2")
makeRegions("renters")
makeRegions("famYYYY")
makeRegions("famYYYYa")
makeRegions("sizesYYYY")
makeRegions("owners")
makeRegions("writeout")
makeRegions("writeout5")
makeRegions("dropSmallTowns")
makeRegions("fixSizes")
makeRegions("load")
}
\seealso{
Other manipulate: \code{\link{subsetCities}}
}
\keyword{city,}
\keyword{region}

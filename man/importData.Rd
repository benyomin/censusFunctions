% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/importData.R
\name{importData}
\alias{importData}
\title{Import data to workspace v: 2.5 - load Data5 from rds}
\usage{
importData(source)
}
\arguments{
\item{source}{Source for data, "raw" imports from .por files. "saved" files were previously parsed in R. Saved files used for quick analysis, raw files used for final project - maximum reproducibility from source data.}
}
\description{
This function imports Israeli census data from spss .por files distributed by the CBS.
}
\examples{
importData("raw")
importData("saved")
importData("load5")
importData("versionD")
importData("writeOutEXP")
importData("familyOldRaw")
importData("familyNewRaw")
importData("combine")
importData("tlv125")
importData("writeOutCombined")
importData("writeOutIND")
importData("rawIND")
importData("familyProcessed")
importData("process")
importData("importExpFamInd")
importData("importCombined")
}
\keyword{family,}
\keyword{import,}
\keyword{individual,}
\keyword{load,}
\keyword{por,}
\keyword{raw,}
\keyword{saved,}
\keyword{spss,}
\keyword{write}

#' Helper function to change directories.
#'
#' This function moves between 2 directories.
#' @param arg Paper or package?
#' @keywords dir, directory
#' @export
#' @examples
#' chngDir(package)
#' chngDir(febThesis)
#' chngDir(paper)
chngDir <- function(arg) {
    if (arg == "package") {
        setwd("~/")
        ## cd to local package censusFunctions
    } else if (arg == "paper") {
        setwd("~/proposal/secret/")
        ## cd to where we build the pdf
    } else if (arg == "index") {
setwd("/Volumes/Bmac/febThesis/index")
    } else if (arg == "febThesis") {
        setwd("~/school/febThesis")
        } else if (arg == "map") {
        setwd("~/scratch/toronto_neighbourhoods")    
    } else {
        return("invalid option, error code q3543dgp")
    }
}

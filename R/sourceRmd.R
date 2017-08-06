#' Source another chapter without switching buffers v.0.1
#'
#' Time saver for interactive use.
#' @family utility
#' @keywords source, load, eval
#' @export sourceRmd
#' @examples
#' sourceRmd()

sourceRmd <- function(file, local = FALSE, ...){
  options(knitr.duplicate.label = 'allow')

  tempR <- tempfile(tmpdir = ".", fileext = ".R")
  on.exit(unlink(tempR))
  knitr::purl(file, output=tempR, quiet = TRUE)

  envir <- globalenv()
  source(tempR, local = envir, ...)
}


## source:
## https://stackoverflow.com/questions/41962434/source-code-from-rmd-file-within-another-rmd#41962958

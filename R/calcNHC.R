#' Subtracts Housing Expenditures from Total Expenditures.
#' Saves this in $NHC column.
#'
#' This function subtracts.
#' @param arg Sentence describing arg.
#' #' @keywords rename, parse
#' @export
#' @examples
#' calcNHC(TRUE)
#' calcNHC(FALSE)
calNHC <- function(arg=TRUE){
exp2004owners$NHC<<-exp2004owners[,3] - exp2004owners[,5]
exp2005owners$NHC<<-exp2005owners[,3] - exp2005owners[,5]
exp2006owners$NHC<<-exp2006owners[,3] - exp2006owners[,5]
exp2007owners$NHC<<-exp2007owners[,3] - exp2007owners[,5]
exp2008owners$NHC<<-exp2008owners[,3] - exp2008owners[,5]
exp2009owners$NHC<<-exp2009owners[,3] - exp2009owners[,5]
exp2010owners$NHC<<-exp2010owners[,3] - exp2010owners[,5]
exp2011owners$NHC<<-exp2011owners[,3] - exp2011owners[,5]
exp2012owners$NHC<<-exp2012owners[,3] - exp2012owners[,5]
exp2013owners$NHC<<-exp2013owners[,3] - exp2013owners[,5]
exp2014owners$NHC<<-exp2014owners[,3] - exp2014owners[,5]
exp2004renters$NHC<<-exp2004renters[,3] - exp2004renters[,4]
exp2005renters$NHC<<-exp2005renters[,3] - exp2005renters[,4]
exp2006renters$NHC<<-exp2006renters[,3] - exp2006renters[,4]
exp2007renters$NHC<<-exp2007renters[,3] - exp2007renters[,4]
exp2008renters$NHC<<-exp2008renters[,3] - exp2008renters[,4]
exp2009renters$NHC<<-exp2009renters[,3] - exp2009renters[,4]
exp2010renters$NHC<<-exp2010renters[,3] - exp2010renters[,4]
exp2011renters$NHC<<-exp2011renters[,3] - exp2011renters[,4]
exp2012renters$NHC<<-exp2012renters[,3] - exp2012renters[,4]
exp2013renters$NHC<<-exp2013renters[,3] - exp2013renters[,4]
exp2014renters$NHC<<-exp2014renters[,3] - exp2014renters[,4]

}

#' Plot visual graphs of data.
#'
#' This function displays processed data visually.
#' @param plot Name of the plot to show. Plot names are in censusFunctions::createGraphs
#' @keywords plot, graph, display, viewer
#' @export
#' @examples
#' censusGraph("allIncome")
#' censusGraph("allConsumption")
censusGraph <- function(plot){
  if(plot=="allConsumption"){
    p7
        return(p7)
  }else if (plot=="allIncome"){
    pallIncome
        return(pallIncome)
    }else{return(0)}
}

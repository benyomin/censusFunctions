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
    plot$p7
        return(plot$p7)
  }else if (plot=="allIncome"){
    plot$pallIncome
        return(plot$pallIncome)
    }else{return(0)}
}

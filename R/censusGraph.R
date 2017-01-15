#' Plot visual graphs of data.
#'
#' This function displays processed data visually.
#' @param plot Name of the plot to show. Plot names are in censusFunctions::createGraphs
#' @keywords plot, graph, display, viewer
#' @export
#' @examples
#' censusGraph("allIncome")
#' censusGraph("allConsumption")
#' censusGraph("car")
censusGraph <- function(plot){
  if(plot=="allConsumption"){
        preparedPlot$p7
        return(preparedPlot$p7)
  }else if (plot=="allIncome"){
        preparedPlot$pallIncome
        return(preparedPlot$pallIncome)
  }else if (plot=="car"){
        preparedPlot$carBoxPlot
        return(preparedPlot$carBoxPlot)
  }else{
        return(0)}}


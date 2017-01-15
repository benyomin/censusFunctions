#' Create ggplot2 plots, but don't print to screen/page.
#'
#' This function aggregates graphing code; returns plots as a list.
#' @param arg1 defaults to TRUE
#' @keywords plots, graphs
#' @export
#' @examples
#' createGraphs(1)
#' createGraphs(0)
createGraphs <- function(arg1=1){
    if(arg1==TRUE){
#names(matIncome)
        ##sample
carBoxPlot <- qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"),
              fill=gear, main="Mileage by Gear Number",
              xlab="", ylab="Miles per Gallon")
plotIncome <-ggplot(data=matIncome)
pIncome <<- plotIncome+ geom_line(aes(x=someyears,
                       y=meanGrossIncome))+xlab("")+
  ylab("Mean monthly gross income")+
   ggtitle("Gross Income by Year - All Households")
#pIncome
#names(allY)
pallY <-ggplot(data=allY)
pallIncome <<- pallY+ geom_line(aes(x=year,
                       y=value,
                       col=key))+
  ylab("Mean Total Income (NIS/month)")+
    xlab("")+
   ggtitle("Total Income by home ownership")+
    geom_vline(xintercept = 2011)+
    geom_vline(xintercept = 2013)
#pallIncome
p2 <-ggplot(data=matConsRenters)
p3 <<- p2+ geom_line(aes(x=someyears,
                       y=meanConsRenters))+xlab("")+
  ylab("Mean monthly consumption expediture")+
   ggtitle("Consumption Expenditure by Year - Renters")

p4 <-ggplot(data=matConsOwners)
p5 <<- p2+ geom_line(aes(x=someyears,
                       y=meanConsOwners))+xlab("")+
  ylab("Mean monthly consumption expediture")+
   ggtitle("Consumption Expenditure by Year - Owners")
p6 <-ggplot(data=allConsumption)
p7 <<- p6+ geom_line(aes(x=year,
                       y=value,
                       col=key))+
  ylab("Mean household consumption (NIS/month)")+
    xlab("")+
   ggtitle("Total Consumption by home ownership")+
    geom_vline(xintercept = 2011)+
    geom_vline(xintercept = 2013)
p8 <-ggplot(data=allNHC)
p9 <<- p8+ geom_line(aes(x=year,
                       y=value,
                       col=key))+
  ylab("Mean household consumption (NIS/month)")+
    xlab("")+
   ggtitle("Non-housing Consumption by home ownership")+
    geom_vline(xintercept = 2011)+
      geom_vline(xintercept = 2010)+
      geom_vline(xintercept = 2013)
p10 <-ggplot(data=matIncome)
p11 <<- p10+ geom_line(aes(x=year,
                       y=value,
                       col=key))+
  ylab("Mean Income (NIS/month)")+
    xlab("")+
   ggtitle("Income by home ownership")+
    geom_vline(xintercept = 2011)+
      geom_vline(xintercept = 2010)+
      geom_vline(xintercept = 2013)
#p9

##Graph Consumption - Weighted.
pWeightedConsumption <- ggplot(data=allWeightedConsumption)
pWeightedConsumption1 <<- pWeightedConsumption + geom_line(aes(x=year,
                                        y=value,
                                        col=key))
#pWeightedConsumption1
##Graph Income - Weighted.
## pWeightedIncome <- ggplot(data=allWeightedIncome)
## pWeightedIncome1 <<- pWeightedIncome + geom_line(aes(x=year,
##                                         y=value,
##                                         col=key))
#pWeightedIncome1
##Graph Non-Housing Consumption - Weighted.
pWeightedNHC <- ggplot(data=allWeightedNHC)
pWeightedNHC1 <<- pWeightedNHC + geom_line(aes(x=year,
                                        y=value,
                                        col=key))
#pWeightedNHC1

preparedPlot <- c(carBoxPlot,
         pIncome,pallIncome,
         p3,p5,p7,p9,p11,
          pWeightedConsumption1
          )

      return(preparedPlot)
    }else if (arg1==FALSE){
        return(carBoxPlot)
    }else{return("Hello error 3009x7")}
}

#' Graphs I've made before  v0.1.4
#'
#' This function plots with ggplot2
#' @param class Owner or renter? Defaults to owner.
#' @param x What goes on the x-axis?
#' @param y What goes on the y-axis?
#' @param year What year do you want to see?
#' @keywords graph, plot
#' @family plot
#' @export
#' @examples
#' savedGraphs('owners', 'income', 'consumption', 2005)
#' savedGraphs('renters','consumption',   'rent', 2013)
#' savedGraphs('renters','income',        'rent', 2010)
savedGraphs <- function(class = "renters", x = "income", y = "consumption", year = 2004) {
    ## setup code ######################## consumption x income (owners) 2004
    frame1 <- as.data.frame(cbind(exp2004owners$grossIncome, exp2004owners$totalConsumption))
    ## consumption x income (renters) 2004
    frame2 <- as.data.frame(cbind(exp2004renters$grossIncome, exp2004renters$totalConsumption))
    ### own / income / consumption / 2004
    incCons2004own <- ggplot(frame1, aes(x = V1, y = V2)) + geom_point() + geom_smooth(method = lm) + 
        ggtitle("Income and Consumption (owners) 2004") + xlab("Total Income") + 
        ylab("Total Consumption")
    ### rent / income / consumption / 2004
    incCons2004rent <- ggplot(frame2, aes(x = V1, y = V2)) + geom_point() + geom_smooth(method = lm) + 
        ggtitle("Income and Consumption (renters) 2004") + xlab("Total Income") + 
        ylab("Total Consumption")
    
    if (class == "renters") {
        ## renters
        if (year == 2004) {
            if (x == "income") {
                if (y == "consumption") {
                  incCons2004rent + scale_x_continuous(limits = c(0, 80000))
                } else {
                  return("error 90345lnd")
                }
            } else if (x == "consumption") {
                return("error oarsnd99")
            } else {
                return("unsaved graph.")
            }
        } else if (year == 2005) {
            return("TBD 2005")
        } else if (year == 2006) {
            return("TBD 2006")
        } else {
            return("this year not available.")
        }
    } else {
        ## owners
        if (year == 2004) {
            if (x == "income") {
                if (y == "consumption") {
                  incCons2004own
                } else {
                  return("error 903ddd")
                }
            } else if (x == "consumption") {
                return("error oarssss9")
            } else {
                return("unsaved graph.")
            }
        } else if (year == 2005) {
            return("TBD 2005")
        } else if (year == 2006) {
            return("TBD 2006")
        } else {
            return("this year not available.")
        }
    }
}

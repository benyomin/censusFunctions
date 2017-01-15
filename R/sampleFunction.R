#' A sample function
#'
#' This function is a template for structuring my code.
#' @param arg1 "options are yes and no, enclosed in parens."
#' @keywords sample, generic
#' @export
#' @examples
#' sampleFunction("Y")
#' sampleFunction("N")
sampleFunction <- function(arg1){
    if(arg1=="Y"){"you said yes"
        return("hello, you said yes")
    }else if(arg1=="N"){"you said no"
        return("goodbye, you said no")
    }else{"the only options are Y and N"
        return("the only options are Y and N")
    }
}

#' Divide into owners and renters.
#' Is called by: weighHouseholds(), takes expYYYYs
#' Creates dataframes based on payment of rent.
#' @param class owners or renters, defaults to owners.
#' @param year Numeric, from 2004 to 2014
#' @keywords renter, owner, subset
#' @export
#' @examples
#' ownership("renters", 2012)
#' ownership("owners",  2004)

#### new version                                              ##
#### based on >0 in imputed rent                              ##
#### uses new column names
#[1] "grossIncome"      "rentalIncome"     "totalConsumption"
#[4] "rentEXP"          "imputedRent"      "WEIGHT"
#[7] "HHNUM"            "year"
ownership <- function(class, year){
    someyears   <- c(2004:2014)
    if(class=="renters"){  ## renters have 0 imputed rent
          if(year==2004){return(subset(exp2004s,imputedRent==0))
    }else if(year==2005){return(subset(exp2005s,imputedRent==0))
    }else if(year==2006){return(subset(exp2006s,imputedRent==0))
    }else if(year==2007){return(subset(exp2007s,imputedRent==0))
    }else if(year==2008){return(subset(exp2008s,imputedRent==0))
    }else if(year==2009){return(subset(exp2009s,imputedRent==0))
    }else if(year==2010){return(subset(exp2010s,imputedRent==0))
    }else if(year==2011){return(subset(exp2011s,imputedRent==0))
    }else if(year==2012){return(subset(exp2012s,imputedRent==0))
    }else if(year==2013){return(subset(exp2013s,imputedRent==0))
    }else if(year==2014){return(subset(exp2014s,imputedRent==0))
    }else{return(0)}
    }else{ ##class="owners"   owners have imputed rent on property
          if(year==2004){return(subset(exp2004s, imputedRent>0))
    }else if(year==2005){return(subset(exp2005s, imputedRent>0))
    }else if(year==2006){return(subset(exp2006s, imputedRent>0))
    }else if(year==2007){return(subset(exp2007s, imputedRent>0))
    }else if(year==2008){return(subset(exp2008s, imputedRent>0))
    }else if(year==2009){return(subset(exp2009s, imputedRent>0))
    }else if(year==2010){return(subset(exp2010s, imputedRent>0))
    }else if(year==2011){return(subset(exp2011s, imputedRent>0))
    }else if(year==2012){return(subset(exp2012s, imputedRent>0))
    }else if(year==2013){return(subset(exp2013s, imputedRent>0))
    }else if(year==2014){return(subset(exp2014s, imputedRent>0))
    }else{return(0)}}}
#### old version ####
#### based on >0 in rental expense## 
## ownership <- function(class, year){
##     someyears   <- c(2004:2014)
##     if(class=="renters"){
##           if(year==2004){return(subset(exp2004s, Q294>0))
##     }else if(year==2005){return(subset(exp2005s, Q308>0))
##     }else if(year==2006){return(subset(exp2006s, Q303>0))
##     }else if(year==2007){return(subset(exp2007s, Q321>0))
##     }else if(year==2008){return(subset(exp2008s, Q325>0))
##     }else if(year==2009){return(subset(exp2009s, Q358>0))
##     }else if(year==2010){return(subset(exp2010s, Q361>0))
##     }else if(year==2011){return(subset(exp2012s, Q366>0))
##     }else if(year==2012){return(subset(exp2012s, Q366>0))
##     }else if(year==2013){return(subset(exp2013s, Q382>0))
##     }else if(year==2014){return(subset(exp2014s, Q383>0))
##     }else{return(0)}
##     }else{ ##class="owners"
##           if(year==2004){return(subset(exp2004s, Q294==0))
##     }else if(year==2005){return(subset(exp2005s, Q308==0))
##     }else if(year==2006){return(subset(exp2006s, Q303==0))
##     }else if(year==2007){return(subset(exp2007s, Q321==0))
##     }else if(year==2008){return(subset(exp2008s, Q325==0))
##     }else if(year==2009){return(subset(exp2009s, Q358==0))
##     }else if(year==2010){return(subset(exp2010s, Q361==0))
##     }else if(year==2011){return(subset(exp2012s, Q366==0))
##     }else if(year==2012){return(subset(exp2012s, Q366==0))
##     }else if(year==2013){return(subset(exp2013s, Q382==0))
##     }else if(year==2014){return(subset(exp2014s, Q383==0))
##     }else{return(0)}}}

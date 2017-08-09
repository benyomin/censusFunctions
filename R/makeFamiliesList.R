#' Make list of family, individual and exp data to workspace v: 0.4
#' still needs attr label of $size set correctly
#' This function imports save data.  Defaults to "old" version.
#' @param source Source for data, 'raw' imports from .por files. 'saved' files were previously parsed in R.
#' @family setup
#' @keywords import, column, stock, size, manipulate
#' @export makeFamiliesList
#' @examples
#' makeFamilies("old")
#' makeFamiliesList("new")
#' makeFamiliesList()

makeFamiliesList <- function(arg = "old") {
  ## import saved data frames
  if(arg == "old"){
        XFI2004   <-   dget("../dataframes/expFamInd2004.txt")
        XFI2005   <-   dget("../dataframes/expFamInd2005.txt")
        XFI2006   <-   dget("../dataframes/expFamInd2006.txt")
        XFI2007   <-   dget("../dataframes/expFamInd2007.txt")
        XFI2008   <-   dget("../dataframes/expFamInd2008.txt")
        XFI2009   <-   dget("../dataframes/expFamInd2009.txt")
        XFI2010   <-   dget("../dataframes/expFamInd2010.txt")
        XFI2011   <-   dget("../dataframes/expFamInd2011.txt")
        XFI2012   <-   dget("../dataframes/expFamInd2012.txt")
        XFI2013   <-   dget("../dataframes/expFamInd2013.txt")
        XFI2014   <-   dget("../dataframes/expFamInd2014.txt")

 ## colnames(XFI2004)

### process - add column with size of house

fam2004 <- XFI2004 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))

fam2005 <- XFI2005 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))

fam2006 <- XFI2006 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))

fam2007 <- XFI2007 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))
fam2008 <- XFI2008 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))
fam2009 <- XFI2009 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))
fam2010 <- XFI2010 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))
fam2011 <- XFI2011 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))
fam2012 <- XFI2012 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))
fam2013 <- XFI2013 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ))
fam2014 <- XFI2014 %>%
     plyr::mutate(size = case_when(ROOMS  <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5            ~ 'large'
                          ))
        ## add stock data

tlv125<-importData("tlv125")

fam2004$stock<-tlv125["2004",2]
fam2005$stock<-tlv125["2005",2]
fam2006$stock<-tlv125["2006",2]
fam2007$stock<-tlv125["2007",2]
fam2008$stock<-tlv125["2008",2]
fam2009$stock<-tlv125["2009",2]
fam2010$stock<-tlv125["2010",2]
fam2011$stock<-tlv125["2011",2]
fam2012$stock<-tlv125["2012",2]
fam2013$stock<-tlv125["2013",2]
fam2014$stock<-tlv125["2014",2]

  family2014 <<- XFI2014  ## needed for $codeloc list of cities

        familiesList <<- list(fam2004, fam2005, fam2006, fam2007, fam2008,
            fam2009, fam2010, fam2011, fam2012, fam2013, fam2014)
        return(familiesList)

    }else if(arg == "new"){

### process - add column with size of house

fam2004a <- XFI2004 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ) %>% factor )

fam2005a <- XFI2005 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )

fam2006a <- XFI2006 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )

fam2007a <- XFI2007 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
fam2008a <- XFI2008 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
fam2009a <- XFI2009 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
fam2010a <- XFI2010 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
fam2011a <- XFI2011 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
fam2012a <- XFI2012 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
fam2013a <- XFI2013 %>%
     plyr::mutate(size = case_when(ROOMS    <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
fam2014a <- XFI2014 %>%
     plyr::mutate(size = case_when(ROOMS  <= 2     ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS >= 4.5             ~ 'large'
                          ) %>% factor )
        ## add stock data

tlv125<-importData("tlv125")

## fam2004a$stock <- integer(tlv125["2004",2])
## fam2005a$stock <- integer(tlv125["2005",2])
## fam2006a$stock <- integer(tlv125["2006",2])
## fam2007a$stock <- integer(tlv125["2007",2])
## fam2008a$stock <- integer(tlv125["2008",2])
## fam2009a$stock <- integer(tlv125["2009",2])
## fam2010a$stock <- integer(tlv125["2010",2])
## fam2011a$stock <- integer(tlv125["2011",2])
## fam2012a$stock <- integer(tlv125["2012",2])
## fam2013a$stock <- integer(tlv125["2013",2])
## fam2014a$stock <- integer(tlv125["2014",2])
fam2004a <- fam2004 %>%
        mutate(stock = integer(tlv125["2004",2]))
fam2005a <- fam2005 %>%
        mutate(stock = integer(tlv125["2005",2]))
fam2006a <- fam2006 %>%
        mutate(stock = integer(tlv125["2006",2]))
fam2007a <- fam2007 %>%
        mutate(stock = integer(tlv125["2007",2]))
fam2008a <- fam2008 %>%
        mutate(stock = integer(tlv125["2008",2]))
fam2009a <- fam2009 %>%
        mutate(stock = integer(tlv125["2009",2]))
fam2010a <- fam2010 %>%
        mutate(stock = integer(tlv125["2010",2]))
fam2011a <- fam2011 %>%
        mutate(stock = integer(tlv125["2011",2]))
fam2012a <- fam2012 %>%
        mutate(stock = integer(tlv125["2012",2]))
fam2013a <- fam2013 %>%
        mutate(stock = integer(tlv125["2013",2]))
fam2014a <- fam2014 %>%
        mutate(stock = integer(tlv125["2014",2]))

  #      familiesList <<- list(fam2004, fam2005, fam2006, fam2007, fam2008,
  #          fam2009, fam2010, fam2011, fam2012, fam2013, fam2014)
  #      return(familiesList)
                            }
}

# dput(familiesList, file = "../dataframes/XFI_stock_size_List.txt")

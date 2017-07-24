#' Make list of family, individual and exp data to workspace v: 0.3
#'
#' This function imports save data.
#' @param source Source for data, 'raw' imports from .por files. 'saved' files were previously parsed in R.
#' @keywords import, saved
#' @export makeFamiliesList
#' @examples
#' makeFamiliesList()
#' makeFamilies(TRUE)

makeFamiliesList <- function(arg = TRUE) {
  ## import saved data frames

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
### process - add column with size of house

fam2004 <- XFI2004 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))

fam2005 <- XFI2005 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))

fam2006 <- XFI2006 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))

fam2007 <- XFI2007 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))
fam2008 <- XFI2008 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))
fam2009 <- XFI2009 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))
fam2010 <- XFI2010 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))
fam2011 <- XFI2011 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))
fam2012 <- XFI2012 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))
fam2013 <- XFI2013 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
                          ))
fam2014 <- XFI2014 %>%
  mutate(size = case_when(ROOMS             <= 2   ~ 'tiny',
                          ROOMS > 2 & ROOMS <= 3   ~ 'small',
                          ROOMS > 3 & ROOMS <= 4   ~ 'medium',
                          ROOMS > 4 & ROOMS <= 4.5 ~ 'large',
                          ROOMS > 5                ~ 'largest'
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
}

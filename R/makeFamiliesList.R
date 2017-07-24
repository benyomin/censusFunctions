#' Make list of family, individual and exp data to workspace v: 0.1
#'
#' This function imports save data.
#' @param source Source for data, 'raw' imports from .por files. 'saved' files were previously parsed in R.
#' @keywords import, saved
#' @export makeFamiliesList
#' @examples
#' makeFamiliesList()
#' makeFamilies(TRUE)

makeFamiliesList <- function(arg = TRUE) {
        family2004<-dget("../dataframes/expFamInd2004.txt")
        family2005<-dget("../dataframes/expFamInd2005.txt")
        family2006<-dget("../dataframes/expFamInd2006.txt")
        family2007<-dget("../dataframes/expFamInd2007.txt")
        family2008<-dget("../dataframes/expFamInd2008.txt")
        family2009<-dget("../dataframes/expFamInd2009.txt")
        family2010<-dget("../dataframes/expFamInd2010.txt")
        family2011<-dget("../dataframes/expFamInd2011.txt")
        family2012<-dget("../dataframes/expFamInd2012.txt")
        family2013<-dget("../dataframes/expFamInd2013.txt")
        family2014<-dget("../dataframes/expFamInd2014.txt")
### process - add column with size of house

fam2004 <- family2004 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))

fam2005 <- family2005 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2006 <- family2006 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2007 <- family2007 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2008 <- family2008 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2009 <- family2009 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2010 <- family2010 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2011 <- family2011 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2012 <- family2012 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2013 <- family2013 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
                          ))
fam2014 <- family2014 %>%
  mutate(size = case_when(rooms             <= 2   ~ 'tiny',
                          rooms > 2 & rooms <= 3   ~ 'small',
                          rooms > 3 & rooms <= 4   ~ 'medium',
                          rooms > 4 & rooms <= 4.5 ~ 'large',
                          rooms > 5                ~ 'largest'
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

  family2014 <<- fam2014$codeloc  ## needed for $codeloc list of cities

        familiesList <<- list(fam2004, fam2005, fam2006, fam2007, fam2008,
            fam2009, fam2010, fam2011, fam2012, fam2013, fam2014)
        return(familiesList)
}

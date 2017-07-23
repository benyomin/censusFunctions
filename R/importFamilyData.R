#' Import family data to workspace v: 2.0
#'
#' This function imports Israeli census data from spss .por files distributed by the CBS.
#' @param source Source for data, 'raw' imports from .por files. 'saved' files were previously parsed in R.
#' @keywords import, saved, spss, por, raw, load
#' @export
#' @examples
#' importFamilyData('raw')
#' importFamilyData('saved')
#' importFamilyData('writeOut')
importFamilyData <- function(source) {
    if (source == "raw") {
        yr2000 <- spss.portable.file("../rawData/census/f461/f461fam.por")
        yr2001 <- spss.portable.file("../rawData/census/f462/f462fam.por")
        yr2002 <- spss.portable.file("../rawData/census/f463/f463fam.por")
        yr2003 <- spss.portable.file("../rawData/census/f464/f464fam.por")
        yr2004 <- spss.portable.file("../rawData/census/f466/f466fam.por")
        yr2005 <- spss.portable.file("../rawData/census/f467/f467fam.por")
        yr2006 <- spss.portable.file("../rawData/census/f468/n468fam.por")  #n-not-f
        yr2007 <- spss.portable.file("../rawData/census/f469/f469fam.por")
        yr2008 <- spss.portable.file("../rawData/census/f474/f474fam.por")
        yr2009 <- spss.portable.file("../rawData/census/f472/f472fam.por")
        yr2010 <- spss.portable.file("../rawData/census/f471/f471fam.por")
        yr2011 <- spss.portable.file("../rawData/census/f459/f459fam.por")
        yr2012 <- spss.portable.file("../rawData/census/f458/f458fam.por")
        yr2013 <- spss.portable.file("../rawData/census/f457/f457fam.por")
        yr2014 <- spss.portable.file("../rawData/census/f456/f456fam.por")
        ### choose several columns from the imported CBS data
        yr.2003 <- memisc::subset(yr2003, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, caseid = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2004 <- memisc::subset(yr2004, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2005 <- memisc::subset(yr2005, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2006 <- memisc::subset(yr2006, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2007 <- memisc::subset(yr2007, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2008 <- memisc::subset(yr2008, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2009 <- memisc::subset(yr2009, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2010 <- memisc::subset(yr2010, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2011 <- memisc::subset(yr2011, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2012 <- memisc::subset(yr2012, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2013 <- memisc::subset(yr2013, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        yr.2014 <- memisc::subset(yr2014, select = c(year = year, rent = rent, owner = owner, 
            keymoney = keymoney, cluster = cluster, hhnum = hhnum, rooms = rooms, 
            codeloc = codeloc, incgross = incgross, exptot = exptot, aptval = aptval, 
            typerent = typerent, weight = weight))
        ###### create data frames family2003 <<-as.data.frame(yr.2003)
        family2004 <- as.data.frame(yr.2004)
        family2005 <- as.data.frame(yr.2005)
        family2006 <- as.data.frame(yr.2006)
        family2007 <- as.data.frame(yr.2007)
        family2008 <- as.data.frame(yr.2008)
        family2009 <- as.data.frame(yr.2009)
        family2010 <- as.data.frame(yr.2010)
        family2011 <- as.data.frame(yr.2011)
        family2012 <- as.data.frame(yr.2012)
        family2013 <- as.data.frame(yr.2013)
        family2014 <- as.data.frame(yr.2014)
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

   familiesList <<- list(fam2004, fam2005, fam2006, fam2007, fam2008,
                         fam2009, fam2010, fam2011, fam2012, fam2013, fam2014)


###    old version
###        familiesList <<- list(family2004, family2005, family2006, family2007, family2008,
###            family2009, family2010, family2011, family2012, family2013, family2014)
        return(familiesList)
    } else if (source == "saved") {
        importData("familyProcessed")
        return("family data imported from saved subset")
    } else if (source == "writeOut") {
      ### version 2 - includes $size

         dput(familiesList, file = "../dataframes/famList.txt")
      ### version 1 - produced familyYYYY.txt files
        ## dput(familiesList, file = "../dataframes/familiesList.txt")
        ## dput(family2004, file = "../dataframes/family2004.txt")
        ## dput(family2005, file = "../dataframes/family2005.txt")
        ## dput(family2006, file = "../dataframes/family2006.txt")
        ## dput(family2007, file = "../dataframes/family2007.txt")
        ## dput(family2008, file = "../dataframes/family2008.txt")
        ## dput(family2009, file = "../dataframes/family2009.txt")
        ## dput(family2010, file = "../dataframes/family2010.txt")
        ## dput(family2011, file = "../dataframes/family2011.txt")
        ## dput(family2012, file = "../dataframes/family2012.txt")
        ## dput(family2013, file = "../dataframes/family2013.txt")
        ## dput(family2014, file = "../dataframes/family2014.txt")
        return("success #onsr9don")
    } else {
        return("error #.eil4g09")
    }
}

## code to write out saved versions save to ~/proposal/dataframes }else
## if(source=='writeOutfamilies'){ ## taken from working Feb16.Rmd
## dput(householdsList, file='../dataframes/householdsList.txt')
## dput(familiesList, file='../dataframes/familiesList.txt') dput(family2004,
## file='../dataframes/family2004.txt') dput(family2005,
## file='../dataframes/family2005.txt') dput(family2006,
## file='../dataframes/family2006.txt') dput(family2007,
## file='../dataframes/family2007.txt') dput(family2008,
## file='../dataframes/family2008.txt') dput(family2009,
## file='../dataframes/family2009.txt') dput(family2010,
## file='../dataframes/family2010.txt') dput(family2011,
## file='../dataframes/family2011.txt') dput(family2012,
## file='../dataframes/family2012.txt') dput(family2013,
## file='../dataframes/family2013.txt') dput(family2014,
## file='../dataframes/family2014.txt')

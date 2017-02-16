#' Import data to workspace v.0.8 import stock index
#'
#' This function imports Israeli census data from spss .por files distributed by the CBS.
#' @param source Source for data, "raw" imports from .por files. "saved" files were previously parsed in R. Saved files used for quick analysis, raw files used for final project - maximum reproducibility from source data.
#' @keywords import, saved, spss, por, raw, load, family, individual, write
#' @export
#' @examples
#' importData("raw")
#' importData("saved")
#' importData("writeOutEXP")
#' importData("familyOldRaw")
#' importData("familyNewRaw")
#' importData("combine")
#' importData("tlv125")
#' importData("writeOutCombined")
#' importData("writeOutIND")
#' importData("rawIND")
#' importData("familyProcessed")
#' importData("process")
#' importData("importExpFamInd")
#' importData("importCombined")
importData <- function(source){
  family="family"
  expenditure="raw"
  if(source=="familyOldRaw"){
fam2004 <- spss.get("../rawData/census/f466/f466fam.por",
                  use.value.labels = TRUE)
fam2005 <- spss.get("../rawData/census/f467/f467fam.por",
                  use.value.labels = TRUE)
fam2006 <- spss.get("../rawData/census/f468/n468fam.por",         #n not f
                  use.value.labels = TRUE)
fam2007 <- spss.get("../rawData/census/f469/f469fam.por",
                  use.value.labels = TRUE)
fam2008 <- spss.get("../rawData/census/f474/f474fam.por",
                  use.value.labels = TRUE)
fam2009 <- spss.get("../rawData/census/f472/f472fam.por",
                    use.value.labels = TRUE)
fam2010 <- fam2009
#### real 2010 is 471, throws error, duplicating 2009 for now
## fam2010 <- spss.get("../rawData/census/f471/f471fam.por",
##                     use.value.labels = TRUE)

fam2011<- fam2009

## fam2011 <- spss.get("../rawData/census/f459/f459fam.por",
##                     use.value.labels = TRUE)
fam2012 <- spss.get("../rawData/census/f458/f458fam.por",
                    use.value.labels = TRUE)
fam2013 <- spss.get("../rawData/census/f457/f457fam.por",
                    use.value.labels = TRUE)
fam2014 <- spss.get("../rawData/census/f456/f456fam.por",
                    use.value.labels = TRUE)

######  create data frames
      ##  note:  these only become local variables   ##
fam2004df <<-as.data.frame(fam2004)
# fam2004s <<-dplyr::select(fam2004df,v40,v48,v42,HHNUM)  #net income ,v43
fam2005df <<-as.data.frame(fam2005)
# fam2005s <<-dplyr::select(fam2005df,v40,v48,v42,HHNUM)  #net income,v43
fam2006df <<-as.data.frame(fam2006)
# fam2006s <<-dplyr::select(fam2006df,v40,v48,v42,HHNUM)
fam2007df <<-as.data.frame(fam2007)
# fam2007s <<-dplyr::select(fam2007df,v40,v48,v42,HHNUM)
fam2008df <<-as.data.frame(fam2008)
# fam2008s <<-dplyr::select(fam2008df,v40,v48,v42,HHNUM)
fam2009df <<-as.data.frame(fam2009)
# fam2009s <<-dplyr::select(fam2009df,v40,v48,v42,HHNUM)
fam2010df <<-as.data.frame(fam2010)
# fam2010s <<-dplyr::select(fam2010df,v40,v48,v42,HHNUM)
fam2011df <<-as.data.frame(fam2011)
# fam2011s <<-dplyr::select(fam2011df,v40,v48,v42,HHNUM)
fam2012df <<-as.data.frame(fam2012)
# fam2012s <<-dplyr::select(fam2012df,v40,v48,v42,HHNUM)
fam2013df <<-as.data.frame(fam2013)
# fam2013s <<-dplyr::select(fam2013df,v40,v48,v42,HHNUM)
fam2014df <<-as.data.frame(fam2014)
# fam2014s <<-dplyr::select(fam2014df,v40,v48,v42,HHNUM)

######  subset data frames
###subset columns in family data
fam2004s <<-dplyr::select(fam2004df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2005s <<-dplyr::select(fam2005df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2006s <<-dplyr::select(fam2006df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2007s <<-dplyr::select(fam2007df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2008s <<-dplyr::select(fam2008df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2009s <<-dplyr::select(fam2009df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2010s <<-dplyr::select(fam2010df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2011s <<-dplyr::select(fam2011df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2012s <<-dplyr::select(fam2012df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2013s <<-dplyr::select(fam2013df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,RELIGION,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)
fam2014s <<-dplyr::select(fam2014df,HHNUM,
                          ROOMS,INCOMENT,EXPTOT,RENT,
                          APTVAL,OWNER,NATIONAL,HHWERNRS,
                          CODELOC,SUBDIST,CLUSTER)

return("Family data imported. 59 vars in 2005, 100 vars in 2013")
  }else if(source=="familyNewRaw"){
    censusFunctions::importFamilyData("raw")
    return("success 05l09l")
  }else if(source=="familyProcessed"){
    ## new family with socio-econ var
householdsList <<- dget("../dataframes/householdsList.txt")
familiesList   <<- dget("../dataframes/familiesList.txt")
family2004     <<- dget("../dataframes/family2004.txt")
family2005     <<- dget("../dataframes/family2005.txt")
family2006     <<- dget("../dataframes/family2006.txt")
family2007     <<- dget("../dataframes/family2007.txt")
family2008     <<- dget("../dataframes/family2008.txt")
family2009     <<- dget("../dataframes/family2009.txt")
family2010     <<- dget("../dataframes/family2010.txt")
family2011     <<- dget("../dataframes/family2011.txt")
family2012     <<- dget("../dataframes/family2012.txt")
family2013     <<- dget("../dataframes/family2013.txt")
family2014     <<- dget("../dataframes/family2014.txt")
    ## already combine family and individual, perhaps not needed
exp2004s <-dget("../dataframes/exp2004s.txt")
exp2005s <-dget("../dataframes/exp2005s.txt")
exp2006s <-dget("../dataframes/exp2006s.txt")
exp2007s <-dget("../dataframes/exp2007s.txt")
exp2008s <-dget("../dataframes/exp2008s.txt")
exp2009s <-dget("../dataframes/exp2009s.txt")
exp2010s <-dget("../dataframes/exp2010s.txt")
exp2011s <-dget("../dataframes/exp2011s.txt")
exp2012s <-dget("../dataframes/exp2012s.txt")
exp2013s <-dget("../dataframes/exp2013s.txt")
exp2014s <-dget("../dataframes/exp2014s.txt")
return("success o9lardn4")
  }else if(source=="process"){
family2004$year<<-2004
family2005$year<<-2005
family2006$year<<-2006
family2007$year<<-2007
family2008$year<<-2008
family2009$year<<-2009
family2010$year<<-2010
family2011$year<<-2011
family2012$year<<-2012
family2013$year<<-2013
family2014$year<<-2014
  }else if(source=="rawIND"){
ind2004 <- spss.get("../rawData/census/f466/f466ind.por",
                  use.value.labels = TRUE)
ind2005 <- spss.get("../rawData/census/f467/f467ind.por",
                  use.value.labels = TRUE)
ind2006 <- spss.get("../rawData/census/f468/n468ind.por",  
                  use.value.labels = TRUE)
ind2007 <- spss.get("../rawData/census/f469/f469ind.por",
                  use.value.labels = TRUE)
ind2008 <- spss.get("../rawData/census/f474/f474ind.por",
                  use.value.labels = TRUE)
ind2009 <- spss.get("../rawData/census/f472/f472ind.por",
                    use.value.labels = TRUE)
ind2010 <- haven::read_por("../rawData/census/f471/f471ind.por")
ind2011 <- spss.get("../rawData/census/f459/f459ind.por",
                    use.value.labels = TRUE)
ind2012 <- spss.get("../rawData/census/f458/f458ind.por",
                    use.value.labels = TRUE)
ind2013 <- spss.get("../rawData/census/f457/f457ind.por",
                    use.value.labels = TRUE)
ind2014 <- spss.get("../rawData/census/f456/f456ind.por",
                    use.value.labels = TRUE)
ind2004s <<- select(ind2004,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2005s <<- select(ind2005,HHNUM,PERSNUM,RELATHH,AGE,SCHOOLY)
ind2006s <<- select(ind2006,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2007s <<- select(ind2007,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2008s <<- select(ind2008,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2009s <<- select(ind2009,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2010s <<- select(ind2010,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2011s <<- select(ind2011,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2012s <<- select(ind2012,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2013s <<- select(ind2013,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)
ind2014s <<- select(ind2014,HHNUM,PERSNUM,RELATHHH,AGE,SCHOOLY)

    return("imported raw individual data, subset selected columns")
  }else  if(source=="writeOutIND"){
dput(ind2004s, file="../dataframes/ind2004s.txt")
dput(ind2005s, file="../dataframes/ind2005s.txt")
dput(ind2006s, file="../dataframes/ind2006s.txt")
dput(ind2007s, file="../dataframes/ind2007s.txt")
dput(ind2008s, file="../dataframes/ind2008s.txt")
dput(ind2009s, file="../dataframes/ind2009s.txt")
dput(ind2010s, file="../dataframes/ind2010s.txt")
dput(ind2011s, file="../dataframes/ind2011s.txt")
dput(ind2012s, file="../dataframes/ind2012s.txt")
dput(ind2013s, file="../dataframes/ind2013s.txt")
dput(ind2014s, file="../dataframes/ind2014s.txt")
    return("wrote out IND data.")
  }else  if(source=="savedIND"){
ind2004s <<- dget("../dataframes/ind2004s.txt")
ind2005s <<- dget("../dataframes/ind2005s.txt")
ind2006s <<- dget("../dataframes/ind2006s.txt")
ind2007s <<- dget("../dataframes/ind2007s.txt")
ind2008s <<- dget("../dataframes/ind2008s.txt")
ind2009s <<- dget("../dataframes/ind2009s.txt")
ind2010s <<- dget("../dataframes/ind2010s.txt")
ind2011s <<- dget("../dataframes/ind2011s.txt")
ind2012s <<- dget("../dataframes/ind2012s.txt")
ind2013s <<- dget("../dataframes/ind2013s.txt")
ind2014s <<- dget("../dataframes/ind2014s.txt")
    return("imported Individual data from saved subset.")
  }else  if(source=="tlv125"){
    #import the new TLV stock data from the csv
tlv128<-readr::read_csv("../rawData/TLV-125midYearPrice.csv")
#View(tlv125)
tlv126<-tlv128[,c(1,4)] # select date and closing price columns
#View(tlv126)
tlv127<-tlv126[13:1,]  # remove extra rows
tlv125<-as.data.frame(tlv127)
rownames(tlv125)<-c(2004:2016)
#tlv128["2004",2] #"YYYY",2nd column is closing P. @ last day in June.
    return(tlv125)    
  }else  if(source=="raw"){
   #   source("../includes/importExpenditureRaw.R",  echo=FALSE)
exp2004 <- spss.get("../rawData/census/f466/f466exp.por",
                  use.value.labels = TRUE)
exp2005 <- spss.get("../rawData/census/f467/f467exp.por",
                  use.value.labels = TRUE)
exp2006 <- spss.get("../rawData/census/f468/f468exp.por",         #f again
                  use.value.labels = TRUE)
exp2007 <- spss.get("../rawData/census/f469/f469exp.por",
                  use.value.labels = TRUE)
exp2008 <- spss.get("../rawData/census/f474/f474exp.por",
                  use.value.labels = TRUE)
exp2009 <- spss.get("../rawData/census/f472/f472exp.por",
                    use.value.labels = TRUE)
exp2010 <- spss.get("../rawData/census/f471/f471exp.por",
                    use.value.labels = TRUE)
exp2011 <- spss.get("../rawData/census/f459/f459exp.por",
                    use.value.labels = TRUE)
exp2012 <- spss.get("../rawData/census/f458/f458exp.por",
                    use.value.labels = TRUE)
exp2013 <- spss.get("../rawData/census/f457/f457exp.por",
                    use.value.labels = TRUE)
exp2014 <- spss.get("../rawData/census/f456/f456exp.por",
                    use.value.labels = TRUE)
######
      ##  note:  these only become local variables   ##
exp2004df <-as.data.frame(exp2004)
exp2004s <-dplyr::select(exp2004df, Q1,Q22,Q64,Q294,Q297,WEIGHT,HHNUM)
exp2005df <-as.data.frame(exp2005)
exp2005s <-dplyr::select(exp2005df, Q1,Q24,Q69,Q308,Q311,WEIGHT,HHNUM)
exp2006df <-as.data.frame(exp2006)
exp2006s <-dplyr::select(exp2006df, Q1,Q24,Q66,Q303,Q306,WEIGHT,HHNUM)
exp2007df <-as.data.frame(exp2007)
exp2007s <-dplyr::select(exp2007df, Q1,Q24,Q69,Q321,Q326,WEIGHT,HHNUM)
exp2008df <-as.data.frame(exp2008)
exp2008s <-dplyr::select(exp2008df, Q1,Q24,Q69,Q325,Q330,WEIGHT,HHNUM)
exp2009df <-as.data.frame(exp2009)
exp2009s <-dplyr::select(exp2009df, Q1,Q26,Q77,Q358,Q363,WEIGHT,HHNUM)
exp2010df <-as.data.frame(exp2010)
exp2010s <-dplyr::select(exp2010df, Q1,Q27,Q78,Q361,Q367,WEIGHT,HHNUM)
exp2011df <-as.data.frame(exp2011)
exp2011s <-dplyr::select(exp2011df, Q1,Q28,Q81,Q366,Q372,WEIGHT,HHNUM)
exp2012df <-as.data.frame(exp2012)
exp2012s <-dplyr::select(exp2012df, Q1,Q28,Q81,Q366,Q372,WEIGHT,HHNUM)
exp2013df <-as.data.frame(exp2013)
exp2013s <-dplyr::select(exp2013df, Q1,Q28,Q83,Q382,Q387,WEIGHT,HHNUM)
exp2014df <-as.data.frame(exp2014)
exp2014s <-dplyr::select(exp2014df, Q1,Q28,Q84,Q383,Q389,WEIGHT,HHNUM)

renameColumns() ## adds a year column
                ## exports columns with <<- super assignment

        return("Imported Housing Expenditure Survey from Central Bureau of Statistics raw files.")
    }else if(source=="saved"){
 ##  source("../includes/getProcessedData.R", echo=FALSE)
exp2004s <<-dget("../dataframes/exp2004s.txt")
exp2005s <<-dget("../dataframes/exp2005s.txt")
exp2006s <<-dget("../dataframes/exp2006s.txt")
exp2007s <<-dget("../dataframes/exp2007s.txt")
exp2008s <<-dget("../dataframes/exp2008s.txt")
exp2009s <<-dget("../dataframes/exp2009s.txt")
exp2010s <<-dget("../dataframes/exp2010s.txt")
exp2011s <<-dget("../dataframes/exp2011s.txt")
exp2012s <<-dget("../dataframes/exp2012s.txt")
exp2013s <<-dget("../dataframes/exp2013s.txt")
exp2014s <<-dget("../dataframes/exp2014s.txt")

      return("Imported selected portions of Housing Expenditure Survey. Returns exp2004-14s")
    }else if(source=="writeOutfamilies"){
      ## taken from working Feb16.Rmd
dput(householdsList, file="../dataframes/householdsList.txt")
dput(familiesList,   file="../dataframes/familiesList.txt")
dput(family2004,     file="../dataframes/family2004.txt")
dput(family2005,     file="../dataframes/family2005.txt")
dput(family2006,     file="../dataframes/family2006.txt")
dput(family2007,     file="../dataframes/family2007.txt")
dput(family2008,     file="../dataframes/family2008.txt")
dput(family2009,     file="../dataframes/family2009.txt")
dput(family2010,     file="../dataframes/family2010.txt")
dput(family2011,     file="../dataframes/family2011.txt")
dput(family2012,     file="../dataframes/family2012.txt")
dput(family2013,     file="../dataframes/family2013.txt")
dput(family2014,     file="../dataframes/family2014.txt")
    }else if(source=="writeOutEXP"){
########################
## write out new data ##
########################
dput(exp2004s, file="../dataframes/exp2004s.txt")
dput(exp2005s, file="../dataframes/exp2005s.txt")
dput(exp2006s, file="../dataframes/exp2006s.txt")
dput(exp2007s, file="../dataframes/exp2007s.txt")
dput(exp2008s, file="../dataframes/exp2008s.txt")
dput(exp2009s, file="../dataframes/exp2009s.txt")
dput(exp2010s, file="../dataframes/exp2010s.txt")
dput(exp2011s, file="../dataframes/exp2011s.txt")
dput(exp2012s, file="../dataframes/exp2012s.txt")
dput(exp2013s, file="../dataframes/exp2013s.txt")
dput(exp2014s, file="../dataframes/exp2014s.txt")
      return("wrote out dataframes")
          }else if(source=="importCombined"){
####################################
## IMPORT    combined data frames ##
####################################
data2004<<-dget("../dataframes/data2004.txt")
data2005<<-dget("../dataframes/data2005.txt")
data2006<<-dget("../dataframes/data2006.txt")
data2007<<-dget("../dataframes/data2007.txt")
data2008<<-dget("../dataframes/data2008.txt")
data2009<<-dget("../dataframes/data2009.txt")
data2010<<-dget("../dataframes/data2010.txt")
data2011<<-dget("../dataframes/data2011.txt")
data2012<<-dget("../dataframes/data2012.txt")
data2013<<-dget("../dataframes/data2013.txt")
data2014<<-dget("../dataframes/data2014.txt")
      return("wrote out dataframes")
 }else if(source=="writeOutExpFamInd"){
####################################
## write out combined data frames ##
####################################
dput(expFamInd2004, file="../dataframes/expFamInd2004.txt")
dput(expFamInd2005, file="../dataframes/expFamInd2005.txt")
dput(expFamInd2006, file="../dataframes/expFamInd2006.txt")
dput(expFamInd2007, file="../dataframes/expFamInd2007.txt")
dput(expFamInd2008, file="../dataframes/expFamInd2008.txt")
dput(expFamInd2009, file="../dataframes/expFamInd2009.txt")
dput(expFamInd2010, file="../dataframes/expFamInd2010.txt")
dput(expFamInd2011, file="../dataframes/expFamInd2011.txt")
dput(expFamInd2012, file="../dataframes/expFamInd2012.txt")
dput(expFamInd2013, file="../dataframes/expFamInd2013.txt")
dput(expFamInd2014, file="../dataframes/expFamInd2014.txt")
      return("wrote out dataframes with Exp, Fam and Ind columns")
 }else if(source=="importExpFamInd"){
####################################
## import    combined data frames ##
####################################
expFamInd2004<<-dget("../dataframes/expFamInd2004.txt")
expFamInd2005<<-dget("../dataframes/expFamInd2005.txt")
expFamInd2006<<-dget("../dataframes/expFamInd2006.txt")
expFamInd2007<<-dget("../dataframes/expFamInd2007.txt")
expFamInd2008<<-dget("../dataframes/expFamInd2008.txt")
expFamInd2009<<-dget("../dataframes/expFamInd2009.txt")
expFamInd2010<<-dget("../dataframes/expFamInd2010.txt")
expFamInd2011<<-dget("../dataframes/expFamInd2011.txt")
expFamInd2012<<-dget("../dataframes/expFamInd2012.txt")
expFamInd2013<<-dget("../dataframes/expFamInd2013.txt")
expFamInd2014<<-dget("../dataframes/expFamInd2014.txt")
      return("wrote out dataframes with Exp, Fam and Ind columns")
          }else if(source=="writeOutCombined"){
####################################
## write out combined data frames ##
####################################
dput(data2004, file="../dataframes/data2004.txt")
dput(data2005, file="../dataframes/data2005.txt")
dput(data2006, file="../dataframes/data2006.txt")
dput(data2007, file="../dataframes/data2007.txt")
dput(data2008, file="../dataframes/data2008.txt")
dput(data2009, file="../dataframes/data2009.txt")
dput(data2010, file="../dataframes/data2010.txt")
dput(data2011, file="../dataframes/data2011.txt")
dput(data2012, file="../dataframes/data2012.txt")
dput(data2013, file="../dataframes/data2013.txt")
dput(data2014, file="../dataframes/data2014.txt")
      return("wrote out dataframes")
      }else if(source=="combine"){
data2004<<-merge(exp2004s,fam2004s, by="HHNUM")
data2005<<-merge(exp2005s,fam2005s, by="HHNUM")
data2006<<-merge(exp2006s,fam2006s, by="HHNUM")
data2007<<-merge(exp2007s,fam2007s, by="HHNUM")
data2008<<-merge(exp2008s,fam2008s, by="HHNUM")
data2009<<-merge(exp2009s,fam2009s, by="HHNUM")
data2010<<-merge(exp2010s,fam2010s, by="HHNUM")
data2011<<-merge(exp2011s,fam2011s, by="HHNUM")
data2012<<-merge(exp2012s,fam2012s, by="HHNUM")
data2013<<-merge(exp2013s,fam2013s, by="HHNUM")
data2014<<-merge(exp2014s,fam2014s, by="HHNUM")
    }else{   ##"the only options are Y and N"
        return("the only options are 'raw' and 'saved'")
    }}


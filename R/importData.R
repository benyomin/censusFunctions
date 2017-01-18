#' Import data to workspace v.0.5 correct subsetting
#'
#' This function imports Israeli census data from spss .por files distributed by the CBS.
#' @param source Source for data, "raw" imports from .por files. "saved" files were previously parsed in R.
#' @keywords import, saved, spss, por, raw, load
#' @export
#' @examples
#' importData("raw")
#' importData("saved")
#' importData("family")
#' importData("writeOutEXP")
#' importData("combine")
#' importData("writeOutCombined")
#' importData("importCombined")
importData <- function(source){
  family="family"
  expenditure="raw"
  if(source=="family"){
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


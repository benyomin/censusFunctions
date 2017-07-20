#' subsets the yearly surveys by city of residence
#' @param arg nil
#' @keywords city, subset
#' @export subsetCities
#' @examples
#' subsetCities()

subsetCities <- function(arg) {

  ## exports these    dataframes to the global environment
  ## cityList      <- 99
  ## survey2004   table of people surveyed by city and year
  ownerCounts   <- 99
  ownersList    <- 99
  ownersYYYY    <- 99
  renterCounts  <- 99
  rentersList   <- 99
  rentersYYYY   <- 99
  jerusalemYYYY <- 99

  ## renter /owner

renters2004  <<- subset(familiesList[[1]], rent=="Yes")
# head(renters2004)  #check
renters2005  <<- subset(familiesList[[2]], rent=="Yes")
renters2006  <<- subset(familiesList[[3]], rent=="Yes")
renters2007  <<- subset(familiesList[[4]], rent=="Yes")
renters2008  <<- subset(familiesList[[5]], rent=="Yes")
renters2009  <<- subset(familiesList[[6]], rent=="Yes")
renters2010  <<- subset(familiesList[[7]], rent=="Yes")
renters2011  <<- subset(familiesList[[8]], rent=="Yes")
renters2012  <<- subset(familiesList[[9]], rent=="Yes")
renters2013  <<- subset(familiesList[[10]],rent=="Yes")
renters2014  <<- subset(familiesList[[11]],rent=="Yes")

rentersList  <<- c(
renters2004,
renters2005,
renters2006,
renters2007,
renters2008,
renters2009,
renters2010,
renters2011,
renters2012,
renters2013,
renters2014)
## owners
#head(familiesList[[1]])
owners2004  <<- subset(familiesList[[1]], owner=="Yes")
# head(owners2004)  #check
owners2005  <<- subset(familiesList[[2]], owner=="Yes")
owners2006  <<- subset(familiesList[[3]], owner=="Yes")
owners2007  <<- subset(familiesList[[4]], owner=="Yes")
owners2008  <<- subset(familiesList[[5]], owner=="Yes")
owners2009  <<- subset(familiesList[[6]], owner=="Yes")
owners2010  <<- subset(familiesList[[7]], owner=="Yes")
owners2011  <<- subset(familiesList[[8]], owner=="Yes")
owners2012  <<- subset(familiesList[[9]], owner=="Yes")
owners2013  <<- subset(familiesList[[10]],owner=="Yes")
owners2014  <<- subset(familiesList[[11]],owner=="Yes")

ownersList <<- c(
owners2004,
owners2005,
owners2006,
owners2007,
owners2008,
owners2009,
owners2010,
owners2011,
owners2012,
owners2013,
owners2014)

countOwners <- c(
count(owners2004),
count(owners2005),
count(owners2006),
count(owners2007),
count(owners2008),
count(owners2009),
count(owners2010),
count(owners2011),
count(owners2012),
count(owners2013),
count(owners2014))

countRenters <- c(
count(renters2004),
count(renters2005),
count(renters2006),
count(renters2007),
count(renters2008),
count(renters2009),
count(renters2010),
count(renters2011),
count(renters2012),
count(renters2013),
count(renters2014))

renterCounts  <<-as.numeric(countRenters)
 ownerCounts  <<-as.numeric(countOwners)

  ## cities by year


#  cities <- as.character(levels(renters2004$codeloc))
#cityList <<- cities[-1]
jerusalem2004 <<- subset(familiesList[[1]], codeloc=="Jerusalem")
Ashdod2004    <<- subset(familiesList[[1]], codeloc=="Ashdod")
Haifa2004     <<- subset(familiesList[[1]], codeloc=="Haifa")
Herzliyya2004 <<- subset(familiesList[[1]], codeloc=="Herzliyya")
Holon2004    <<- subset(familiesList[[1]], codeloc=="Holon")
Lod2004      <<- subset(familiesList[[1]], codeloc=="Lod")
Hadera2004   <<- subset(familiesList[[1]], codeloc=="Hadera")
Ashqelon2004 <<- subset(familiesList[[1]], codeloc=="Ashqelon")
Nettanya2004 <<- subset(familiesList[[1]], codeloc=="Nettanya")
Rehovot2004  <<- subset(familiesList[[1]], codeloc=="Rehovot")
Ramla2004    <<- subset(familiesList[[1]], codeloc=="Ramla")
Raannana2004 <<- subset(familiesList[[1]], codeloc=="Raannana")
tlv2004      <<- subset(familiesList[[1]], codeloc=="Tel Aviv-Yafo")
bene2004     <<- subset(familiesList[[1]], codeloc=="Bene Beraq")
bat2004      <<- subset(familiesList[[1]], codeloc=="Bat Yam")
kefar2004    <<- subset(familiesList[[1]], codeloc=="Kefar Sava")
petah2004    <<- subset(familiesList[[1]], codeloc=="Petah Tiqwa")
rishon2004   <<- subset(familiesList[[1]], codeloc=="Rishon LeZiyon")
ramat2004    <<- subset(familiesList[[1]], codeloc=="Ramat Gan")
beer2004     <<- subset(familiesList[[1]], codeloc=="Beer Sheva")

jerusalem2005 <<- subset(familiesList[[2]], codeloc=="Jerusalem")
Ashdod2005    <<- subset(familiesList[[2]], codeloc=="Ashdod")
Haifa2005     <<- subset(familiesList[[2]], codeloc=="Haifa")
Herzliyya2005 <<- subset(familiesList[[2]], codeloc=="Herzliyya")
Holon2005    <<- subset(familiesList[[2]], codeloc=="Holon")
Lod2005      <<- subset(familiesList[[2]], codeloc=="Lod")
Hadera2005   <<- subset(familiesList[[2]], codeloc=="Hadera")
Ashqelon2005 <<- subset(familiesList[[2]], codeloc=="Ashqelon")
Nettanya2005 <<- subset(familiesList[[2]], codeloc=="Nettanya")
Rehovot2005  <<- subset(familiesList[[2]], codeloc=="Rehovot")
Ramla2005    <<- subset(familiesList[[2]], codeloc=="Ramla")
Raannana2005 <<- subset(familiesList[[2]], codeloc=="Raannana")
tlv2005      <<- subset(familiesList[[2]], codeloc=="Tel Aviv-Yafo")
bene2005     <<- subset(familiesList[[2]], codeloc=="Bene Beraq")
bat2005      <<- subset(familiesList[[2]], codeloc=="Bat Yam")
kefar2005    <<- subset(familiesList[[2]], codeloc=="Kefar Sava")
petah2005    <<- subset(familiesList[[2]], codeloc=="Petah Tiqwa")
rishon2005   <<- subset(familiesList[[2]], codeloc=="Rishon LeZiyon")
ramat2005    <<- subset(familiesList[[2]], codeloc=="Ramat Gan")
beer2005     <<- subset(familiesList[[2]], codeloc=="Beer Sheva")

jerusalem2006 <<- subset(familiesList[[3]], codeloc=="Jerusalem")
Ashdod2006    <<- subset(familiesList[[3]], codeloc=="Ashdod")
Haifa2006     <<- subset(familiesList[[3]], codeloc=="Haifa")
Herzliyya2006 <<- subset(familiesList[[3]], codeloc=="Herzliyya")
Holon2006    <<- subset(familiesList[[3]], codeloc=="Holon")
Lod2006      <<- subset(familiesList[[3]], codeloc=="Lod")
Hadera2006   <<- subset(familiesList[[3]], codeloc=="Hadera")
Ashqelon2006 <<- subset(familiesList[[3]], codeloc=="Ashqelon")
Nettanya2006 <<- subset(familiesList[[3]], codeloc=="Nettanya")
Rehovot2006  <<- subset(familiesList[[3]], codeloc=="Rehovot")
Ramla2006    <<- subset(familiesList[[3]], codeloc=="Ramla")
Raannana2006 <<- subset(familiesList[[3]], codeloc=="Raannana")
tlv2006      <<- subset(familiesList[[3]], codeloc=="Tel Aviv-Yafo")
bene2006     <<- subset(familiesList[[3]], codeloc=="Bene Beraq")
bat2006      <<- subset(familiesList[[3]], codeloc=="Bat Yam")
kefar2006    <<- subset(familiesList[[3]], codeloc=="Kefar Sava")
petah2006    <<- subset(familiesList[[3]], codeloc=="Petah Tiqwa")
rishon2006   <<- subset(familiesList[[3]], codeloc=="Rishon LeZiyon")
ramat2006    <<- subset(familiesList[[3]], codeloc=="Ramat Gan")
beer2006     <<- subset(familiesList[[3]], codeloc=="Beer Sheva")

jerusalem2007 <<- subset(familiesList[[4]], codeloc=="Jerusalem")
Ashdod2007    <<- subset(familiesList[[4]], codeloc=="Ashdod")
Haifa2007     <<- subset(familiesList[[4]], codeloc=="Haifa")
Herzliyya2007 <<- subset(familiesList[[4]], codeloc=="Herzliyya")
Holon2007    <<- subset(familiesList[[4]], codeloc=="Holon")
Lod2007      <<- subset(familiesList[[4]], codeloc=="Lod")
Hadera2007   <<- subset(familiesList[[4]], codeloc=="Hadera")
Ashqelon2007 <<- subset(familiesList[[4]], codeloc=="Ashqelon")
Nettanya2007 <<- subset(familiesList[[4]], codeloc=="Nettanya")
Rehovot2007  <<- subset(familiesList[[4]], codeloc=="Rehovot")
Ramla2007    <<- subset(familiesList[[4]], codeloc=="Ramla")
Raannana2007 <<- subset(familiesList[[4]], codeloc=="Raannana")
tlv2007      <<- subset(familiesList[[4]], codeloc=="Tel Aviv-Yafo")
bene2007     <<- subset(familiesList[[4]], codeloc=="Bene Beraq")
bat2007      <<- subset(familiesList[[4]], codeloc=="Bat Yam")
kefar2007    <<- subset(familiesList[[4]], codeloc=="Kefar Sava")
petah2007    <<- subset(familiesList[[4]], codeloc=="Petah Tiqwa")
rishon2007   <<- subset(familiesList[[4]], codeloc=="Rishon LeZiyon")
ramat2007    <<- subset(familiesList[[4]], codeloc=="Ramat Gan")
beer2007     <<- subset(familiesList[[4]], codeloc=="Beer Sheva")

jerusalem2008 <<- subset(familiesList[[5]], codeloc=="Jerusalem")
Ashdod2008    <<- subset(familiesList[[5]], codeloc=="Ashdod")
Haifa2008     <<- subset(familiesList[[5]], codeloc=="Haifa")
Herzliyya2008 <<- subset(familiesList[[5]], codeloc=="Herzliyya")
Holon2008    <<- subset(familiesList[[5]], codeloc=="Holon")
Lod2008      <<- subset(familiesList[[5]], codeloc=="Lod")
Hadera2008   <<- subset(familiesList[[5]], codeloc=="Hadera")
Ashqelon2008 <<- subset(familiesList[[5]], codeloc=="Ashqelon")
Nettanya2008 <<- subset(familiesList[[5]], codeloc=="Nettanya")
Rehovot2008  <<- subset(familiesList[[5]], codeloc=="Rehovot")
Ramla2008    <<- subset(familiesList[[5]], codeloc=="Ramla")
Raannana2008 <<- subset(familiesList[[5]], codeloc=="Raannana")
tlv2008      <<- subset(familiesList[[5]], codeloc=="Tel Aviv-Yafo")
bene2008     <<- subset(familiesList[[5]], codeloc=="Bene Beraq")
bat2008      <<- subset(familiesList[[5]], codeloc=="Bat Yam")
kefar2008    <<- subset(familiesList[[5]], codeloc=="Kefar Sava")
petah2008    <<- subset(familiesList[[5]], codeloc=="Petah Tiqwa")
rishon2008   <<- subset(familiesList[[5]], codeloc=="Rishon LeZiyon")
ramat2008    <<- subset(familiesList[[5]], codeloc=="Ramat Gan")
beer2008     <<- subset(familiesList[[5]], codeloc=="Beer Sheva")

jerusalem2009 <<- subset(familiesList[[6]], codeloc=="Jerusalem")
Ashdod2009    <<- subset(familiesList[[6]], codeloc=="Ashdod")
Haifa2009     <<- subset(familiesList[[6]], codeloc=="Haifa")
Herzliyya2009 <<- subset(familiesList[[6]], codeloc=="Herzliyya")
Holon2009    <<- subset(familiesList[[6]], codeloc=="Holon")
Lod2009      <<- subset(familiesList[[6]], codeloc=="Lod")
Hadera2009   <<- subset(familiesList[[6]], codeloc=="Hadera")
Ashqelon2009 <<- subset(familiesList[[6]], codeloc=="Ashqelon")
Nettanya2009 <<- subset(familiesList[[6]], codeloc=="Nettanya")
Rehovot2009  <<- subset(familiesList[[6]], codeloc=="Rehovot")
Ramla2009    <<- subset(familiesList[[6]], codeloc=="Ramla")
Raannana2009 <<- subset(familiesList[[6]], codeloc=="Raannana")
tlv2009      <<- subset(familiesList[[6]], codeloc=="Tel Aviv-Yafo")
bene2009     <<- subset(familiesList[[6]], codeloc=="Bene Beraq")
bat2009      <<- subset(familiesList[[6]], codeloc=="Bat Yam")
kefar2009    <<- subset(familiesList[[6]], codeloc=="Kefar Sava")
petah2009    <<- subset(familiesList[[6]], codeloc=="Petah Tiqwa")
rishon2009   <<- subset(familiesList[[6]], codeloc=="Rishon LeZiyon")
ramat2009    <<- subset(familiesList[[6]], codeloc=="Ramat Gan")
beer2009     <<- subset(familiesList[[6]], codeloc=="Beer Sheva")

jerusalem2010 <<- subset(familiesList[[7]], codeloc=="Jerusalem")
Ashdod2010    <<- subset(familiesList[[7]], codeloc=="Ashdod")
Haifa2010     <<- subset(familiesList[[7]], codeloc=="Haifa")
Herzliyya2010 <<- subset(familiesList[[7]], codeloc=="Herzliyya")
Holon2010    <<- subset(familiesList[[7]], codeloc=="Holon")
Lod2010      <<- subset(familiesList[[7]], codeloc=="Lod")
Hadera2010   <<- subset(familiesList[[7]], codeloc=="Hadera")
Ashqelon2010 <<- subset(familiesList[[7]], codeloc=="Ashqelon")
Nettanya2010 <<- subset(familiesList[[7]], codeloc=="Nettanya")
Rehovot2010  <<- subset(familiesList[[7]], codeloc=="Rehovot")
Ramla2010    <<- subset(familiesList[[7]], codeloc=="Ramla")
Raannana2010 <<- subset(familiesList[[7]], codeloc=="Raannana")
tlv2010      <<- subset(familiesList[[7]], codeloc=="Tel Aviv-Yafo")
bene2010     <<- subset(familiesList[[7]], codeloc=="Bene Beraq")
bat2010      <<- subset(familiesList[[7]], codeloc=="Bat Yam")
kefar2010    <<- subset(familiesList[[7]], codeloc=="Kefar Sava")
petah2010    <<- subset(familiesList[[7]], codeloc=="Petah Tiqwa")
rishon2010   <<- subset(familiesList[[7]], codeloc=="Rishon LeZiyon")
ramat2010    <<- subset(familiesList[[7]], codeloc=="Ramat Gan")
beer2010     <<- subset(familiesList[[7]], codeloc=="Beer Sheva")

jerusalem2011 <<- subset(familiesList[[8]], codeloc=="Jerusalem")
Ashdod2011    <<- subset(familiesList[[8]], codeloc=="Ashdod")
Haifa2011     <<- subset(familiesList[[8]], codeloc=="Haifa")
Herzliyya2011 <<- subset(familiesList[[8]], codeloc=="Herzliyya")
Holon2011    <<- subset(familiesList[[8]], codeloc=="Holon")
Lod2011      <<- subset(familiesList[[8]], codeloc=="Lod")
Hadera2011   <<- subset(familiesList[[8]], codeloc=="Hadera")
Ashqelon2011 <<- subset(familiesList[[8]], codeloc=="Ashqelon")
Nettanya2011 <<- subset(familiesList[[8]], codeloc=="Nettanya")
Rehovot2011  <<- subset(familiesList[[8]], codeloc=="Rehovot")
Ramla2011    <<- subset(familiesList[[8]], codeloc=="Ramla")
Raannana2011 <<- subset(familiesList[[8]], codeloc=="Raannana")
tlv2011      <<- subset(familiesList[[8]], codeloc=="Tel Aviv-Yafo")
bene2011     <<- subset(familiesList[[8]], codeloc=="Bene Beraq")
bat2011      <<- subset(familiesList[[8]], codeloc=="Bat Yam")
kefar2011    <<- subset(familiesList[[8]], codeloc=="Kefar Sava")
petah2011    <<- subset(familiesList[[8]], codeloc=="Petah Tiqwa")
rishon2011   <<- subset(familiesList[[8]], codeloc=="Rishon LeZiyon")
ramat2011    <<- subset(familiesList[[8]], codeloc=="Ramat Gan")
beer2011     <<- subset(familiesList[[8]], codeloc=="Beer Sheva")

jerusalem2012 <<- subset(familiesList[[9]], codeloc=="Jerusalem")
Ashdod2012    <<- subset(familiesList[[9]], codeloc=="Ashdod")
Haifa2012     <<- subset(familiesList[[9]], codeloc=="Haifa")
Herzliyya2012 <<- subset(familiesList[[9]], codeloc=="Herzliyya")
Holon2012    <<- subset(familiesList[[9]], codeloc=="Holon")
Lod2012      <<- subset(familiesList[[9]], codeloc=="Lod")
Hadera2012   <<- subset(familiesList[[9]], codeloc=="Hadera")
Ashqelon2012 <<- subset(familiesList[[9]], codeloc=="Ashqelon")
Nettanya2012 <<- subset(familiesList[[9]], codeloc=="Nettanya")
Rehovot2012  <<- subset(familiesList[[9]], codeloc=="Rehovot")
Ramla2012    <<- subset(familiesList[[9]], codeloc=="Ramla")
Raannana2012 <<- subset(familiesList[[9]], codeloc=="Raannana")
tlv2012      <<- subset(familiesList[[9]], codeloc=="Tel Aviv-Yafo")
bene2012     <<- subset(familiesList[[9]], codeloc=="Bene Beraq")
bat2012      <<- subset(familiesList[[9]], codeloc=="Bat Yam")
kefar2012    <<- subset(familiesList[[9]], codeloc=="Kefar Sava")
petah2012    <<- subset(familiesList[[9]], codeloc=="Petah Tiqwa")
rishon2012   <<- subset(familiesList[[9]], codeloc=="Rishon LeZiyon")
ramat2012    <<- subset(familiesList[[9]], codeloc=="Ramat Gan")
beer2012     <<- subset(familiesList[[9]], codeloc=="Beer Sheva")

jerusalem2013 <<- subset(familiesList[[10]], codeloc=="Jerusalem")
Ashdod2013    <<- subset(familiesList[[10]], codeloc=="Ashdod")
Haifa2013     <<- subset(familiesList[[10]], codeloc=="Haifa")
Herzliyya2013 <<- subset(familiesList[[10]], codeloc=="Herzliyya")
Holon2013    <<- subset(familiesList[[10]], codeloc=="Holon")
Lod2013      <<- subset(familiesList[[10]], codeloc=="Lod")
Hadera2013   <<- subset(familiesList[[10]], codeloc=="Hadera")
Ashqelon2013 <<- subset(familiesList[[10]], codeloc=="Ashqelon")
Nettanya2013 <<- subset(familiesList[[10]], codeloc=="Nettanya")
Rehovot2013  <<- subset(familiesList[[10]], codeloc=="Rehovot")
Ramla2013    <<- subset(familiesList[[10]], codeloc=="Ramla")
Raannana2013 <<- subset(familiesList[[10]], codeloc=="Raannana")
tlv2013      <<- subset(familiesList[[10]], codeloc=="Tel Aviv-Yafo")
bene2013     <<- subset(familiesList[[10]], codeloc=="Bene Beraq")
bat2013      <<- subset(familiesList[[10]], codeloc=="Bat Yam")
kefar2013    <<- subset(familiesList[[10]], codeloc=="Kefar Sava")
petah2013    <<- subset(familiesList[[10]], codeloc=="Petah Tiqwa")
rishon2013   <<- subset(familiesList[[10]], codeloc=="Rishon LeZiyon")
ramat2013    <<- subset(familiesList[[10]], codeloc=="Ramat Gan")
beer2013     <<- subset(familiesList[[10]], codeloc=="Beer Sheva")

jerusalem2014 <<- subset(familiesList[[11]], codeloc=="Jerusalem")
Ashdod2014    <<- subset(familiesList[[11]], codeloc=="Ashdod")
Haifa2014     <<- subset(familiesList[[11]], codeloc=="Haifa")
Herzliyya2014 <<- subset(familiesList[[11]], codeloc=="Herzliyya")
Holon2014    <<- subset(familiesList[[11]], codeloc=="Holon")
Lod2014      <<- subset(familiesList[[11]], codeloc=="Lod")
Hadera2014   <<- subset(familiesList[[11]], codeloc=="Hadera")
Ashqelon2014 <<- subset(familiesList[[11]], codeloc=="Ashqelon")
Nettanya2014 <<- subset(familiesList[[11]], codeloc=="Nettanya")
Rehovot2014  <<- subset(familiesList[[11]], codeloc=="Rehovot")
Ramla2014    <<- subset(familiesList[[11]], codeloc=="Ramla")
Raannana2014 <<- subset(familiesList[[11]], codeloc=="Raannana")
tlv2014      <<- subset(familiesList[[11]], codeloc=="Tel Aviv-Yafo")
bene2014     <<- subset(familiesList[[11]], codeloc=="Bene Beraq")
bat2014      <<- subset(familiesList[[11]], codeloc=="Bat Yam")
kefar2014    <<- subset(familiesList[[11]], codeloc=="Kefar Sava")
petah2014    <<- subset(familiesList[[11]], codeloc=="Petah Tiqwa")
rishon2014   <<- subset(familiesList[[11]], codeloc=="Rishon LeZiyon")
ramat2014    <<- subset(familiesList[[11]], codeloc=="Ramat Gan")
beer2014     <<- subset(familiesList[[11]], codeloc=="Beer Sheva")

  ## counts in cities


cities2004 <- c(
jerusalem2004,
Ashdod2004  ,
Haifa2004  ,
Herzliyya2004 ,
Holon2004   ,
Lod2004    ,
Hadera2004,
Ashqelon2004,
Nettanya2004,
Rehovot2004 ,
Ramla2004  ,
Raannana2004,
tlv2004    ,
bene2004  ,
bat2004  ,
kefar2004,
petah2004  ,
rishon2004,
ramat2004,
beer2004)

#cityList
#cities2004
counts2004 <- c(
jerusalem2004 %>% count %>% as.numeric,
Ashdod2004    %>% count %>% as.numeric,
Haifa2004     %>% count %>% as.numeric,
Herzliyya2004 %>% count %>% as.numeric,
Holon2004     %>% count %>% as.numeric,
Lod2004       %>% count %>% as.numeric,
Hadera2004    %>% count %>% as.numeric,
Ashqelon2004  %>% count %>% as.numeric,
Nettanya2004  %>% count %>% as.numeric,
Rehovot2004   %>% count %>% as.numeric,
Ramla2004     %>% count %>% as.numeric,
Raannana2004  %>% count %>% as.numeric,
tlv2004       %>% count %>% as.numeric,
bene2004      %>% count %>% as.numeric,
bat2004       %>% count %>% as.numeric,
kefar2004     %>% count %>% as.numeric,
petah2004     %>% count %>% as.numeric,
rishon2004    %>% count %>% as.numeric,
ramat2004     %>% count %>% as.numeric,
beer2004      %>% count %>% as.numeric)

# counts2004

length(cityList)
counts2005a <- (100:119)
counts2005a

## all years

counts2005 <-  c(
jerusalem2005 %>% count %>% as.numeric,
Ashdod2005    %>% count %>% as.numeric,
Haifa2005     %>% count %>% as.numeric,
Herzliyya2005 %>% count %>% as.numeric,
Holon2005     %>% count %>% as.numeric,
Lod2005       %>% count %>% as.numeric,
Hadera2005    %>% count %>% as.numeric,
Ashqelon2005  %>% count %>% as.numeric,
Nettanya2005  %>% count %>% as.numeric,
Rehovot2005   %>% count %>% as.numeric,
Ramla2005     %>% count %>% as.numeric,
Raannana2005  %>% count %>% as.numeric,
tlv2005       %>% count %>% as.numeric,
bene2005      %>% count %>% as.numeric,
bat2005       %>% count %>% as.numeric,
kefar2005     %>% count %>% as.numeric,
petah2005     %>% count %>% as.numeric,
rishon2005    %>% count %>% as.numeric,
ramat2005     %>% count %>% as.numeric,
beer2005      %>% count %>% as.numeric)

counts2006 <-  c(
jerusalem2006 %>% count %>% as.numeric,
Ashdod2006    %>% count %>% as.numeric,
Haifa2006     %>% count %>% as.numeric,
Herzliyya2006 %>% count %>% as.numeric,
Holon2006     %>% count %>% as.numeric,
Lod2006       %>% count %>% as.numeric,
Hadera2006    %>% count %>% as.numeric,
Ashqelon2006  %>% count %>% as.numeric,
Nettanya2006  %>% count %>% as.numeric,
Rehovot2006   %>% count %>% as.numeric,
Ramla2006     %>% count %>% as.numeric,
Raannana2006  %>% count %>% as.numeric,
tlv2006       %>% count %>% as.numeric,
bene2006      %>% count %>% as.numeric,
bat2006       %>% count %>% as.numeric,
kefar2006     %>% count %>% as.numeric,
petah2006     %>% count %>% as.numeric,
rishon2006    %>% count %>% as.numeric,
ramat2006     %>% count %>% as.numeric,
beer2006      %>% count %>% as.numeric)


counts2007 <-  c(
jerusalem2007 %>% count %>% as.numeric,
Ashdod2007    %>% count %>% as.numeric,
Haifa2007     %>% count %>% as.numeric,
Herzliyya2007 %>% count %>% as.numeric,
Holon2007     %>% count %>% as.numeric,
Lod2007       %>% count %>% as.numeric,
Hadera2007    %>% count %>% as.numeric,
Ashqelon2007  %>% count %>% as.numeric,
Nettanya2007  %>% count %>% as.numeric,
Rehovot2007   %>% count %>% as.numeric,
Ramla2007     %>% count %>% as.numeric,
Raannana2007  %>% count %>% as.numeric,
tlv2007       %>% count %>% as.numeric,
bene2007      %>% count %>% as.numeric,
bat2007       %>% count %>% as.numeric,
kefar2007     %>% count %>% as.numeric,
petah2007     %>% count %>% as.numeric,
rishon2007    %>% count %>% as.numeric,
ramat2007     %>% count %>% as.numeric,
beer2007      %>% count %>% as.numeric)

counts2008 <-  c(
jerusalem2008 %>% count %>% as.numeric,
Ashdod2008    %>% count %>% as.numeric,
Haifa2008     %>% count %>% as.numeric,
Herzliyya2008 %>% count %>% as.numeric,
Holon2008     %>% count %>% as.numeric,
Lod2008       %>% count %>% as.numeric,
Hadera2008    %>% count %>% as.numeric,
Ashqelon2008  %>% count %>% as.numeric,
Nettanya2008  %>% count %>% as.numeric,
Rehovot2008   %>% count %>% as.numeric,
Ramla2008     %>% count %>% as.numeric,
Raannana2008  %>% count %>% as.numeric,
tlv2008       %>% count %>% as.numeric,
bene2008      %>% count %>% as.numeric,
bat2008       %>% count %>% as.numeric,
kefar2008     %>% count %>% as.numeric,
petah2008     %>% count %>% as.numeric,
rishon2008    %>% count %>% as.numeric,
ramat2008     %>% count %>% as.numeric,
beer2008      %>% count %>% as.numeric)

counts2009 <-  c(
jerusalem2009 %>% count %>% as.numeric,
Ashdod2009    %>% count %>% as.numeric,
Haifa2009     %>% count %>% as.numeric,
Herzliyya2009 %>% count %>% as.numeric,
Holon2009     %>% count %>% as.numeric,
Lod2009       %>% count %>% as.numeric,
Hadera2009    %>% count %>% as.numeric,
Ashqelon2009  %>% count %>% as.numeric,
Nettanya2009  %>% count %>% as.numeric,
Rehovot2009   %>% count %>% as.numeric,
Ramla2009     %>% count %>% as.numeric,
Raannana2009  %>% count %>% as.numeric,
tlv2009       %>% count %>% as.numeric,
bene2009      %>% count %>% as.numeric,
bat2009       %>% count %>% as.numeric,
kefar2009     %>% count %>% as.numeric,
petah2009     %>% count %>% as.numeric,
rishon2009    %>% count %>% as.numeric,
ramat2009     %>% count %>% as.numeric,
beer2009      %>% count %>% as.numeric)

counts2010 <-  c(
jerusalem2010 %>% count %>% as.numeric,
Ashdod2010    %>% count %>% as.numeric,
Haifa2010     %>% count %>% as.numeric,
Herzliyya2010 %>% count %>% as.numeric,
Holon2010     %>% count %>% as.numeric,
Lod2010       %>% count %>% as.numeric,
Hadera2010    %>% count %>% as.numeric,
Ashqelon2010  %>% count %>% as.numeric,
Nettanya2010  %>% count %>% as.numeric,
Rehovot2010   %>% count %>% as.numeric,
Ramla2010     %>% count %>% as.numeric,
Raannana2010  %>% count %>% as.numeric,
tlv2010       %>% count %>% as.numeric,
bene2010      %>% count %>% as.numeric,
bat2010       %>% count %>% as.numeric,
kefar2010     %>% count %>% as.numeric,
petah2010     %>% count %>% as.numeric,
rishon2010    %>% count %>% as.numeric,
ramat2010     %>% count %>% as.numeric,
beer2010      %>% count %>% as.numeric)

counts2011 <-  c(
jerusalem2011 %>% count %>% as.numeric,
Ashdod2011    %>% count %>% as.numeric,
Haifa2011     %>% count %>% as.numeric,
Herzliyya2011 %>% count %>% as.numeric,
Holon2011     %>% count %>% as.numeric,
Lod2011       %>% count %>% as.numeric,
Hadera2011    %>% count %>% as.numeric,
Ashqelon2011  %>% count %>% as.numeric,
Nettanya2011  %>% count %>% as.numeric,
Rehovot2011   %>% count %>% as.numeric,
Ramla2011     %>% count %>% as.numeric,
Raannana2011  %>% count %>% as.numeric,
tlv2011       %>% count %>% as.numeric,
bene2011      %>% count %>% as.numeric,
bat2011       %>% count %>% as.numeric,
kefar2011     %>% count %>% as.numeric,
petah2011     %>% count %>% as.numeric,
rishon2011    %>% count %>% as.numeric,
ramat2011     %>% count %>% as.numeric,
beer2011      %>% count %>% as.numeric)

counts2012 <-  c(
jerusalem2012 %>% count %>% as.numeric,
Ashdod2012    %>% count %>% as.numeric,
Haifa2012     %>% count %>% as.numeric,
Herzliyya2012 %>% count %>% as.numeric,
Holon2012     %>% count %>% as.numeric,
Lod2012       %>% count %>% as.numeric,
Hadera2012    %>% count %>% as.numeric,
Ashqelon2012  %>% count %>% as.numeric,
Nettanya2012  %>% count %>% as.numeric,
Rehovot2012   %>% count %>% as.numeric,
Ramla2012     %>% count %>% as.numeric,
Raannana2012  %>% count %>% as.numeric,
tlv2012       %>% count %>% as.numeric,
bene2012      %>% count %>% as.numeric,
bat2012       %>% count %>% as.numeric,
kefar2012     %>% count %>% as.numeric,
petah2012     %>% count %>% as.numeric,
rishon2012    %>% count %>% as.numeric,
ramat2012     %>% count %>% as.numeric,
beer2012      %>% count %>% as.numeric)

counts2013 <-  c(
jerusalem2013 %>% count %>% as.numeric,
Ashdod2013    %>% count %>% as.numeric,
Haifa2013     %>% count %>% as.numeric,
Herzliyya2013 %>% count %>% as.numeric,
Holon2013     %>% count %>% as.numeric,
Lod2013       %>% count %>% as.numeric,
Hadera2013    %>% count %>% as.numeric,
Ashqelon2013  %>% count %>% as.numeric,
Nettanya2013  %>% count %>% as.numeric,
Rehovot2013   %>% count %>% as.numeric,
Ramla2013     %>% count %>% as.numeric,
Raannana2013  %>% count %>% as.numeric,
tlv2013       %>% count %>% as.numeric,
bene2013      %>% count %>% as.numeric,
bat2013       %>% count %>% as.numeric,
kefar2013     %>% count %>% as.numeric,
petah2013     %>% count %>% as.numeric,
rishon2013    %>% count %>% as.numeric,
ramat2013     %>% count %>% as.numeric,
beer2013      %>% count %>% as.numeric)

counts2014 <-  c(
jerusalem2014 %>% count %>% as.numeric,
Ashdod2014    %>% count %>% as.numeric,
Haifa2014     %>% count %>% as.numeric,
Herzliyya2014 %>% count %>% as.numeric,
Holon2014     %>% count %>% as.numeric,
Lod2014       %>% count %>% as.numeric,
Hadera2014    %>% count %>% as.numeric,
Ashqelon2014  %>% count %>% as.numeric,
Nettanya2014  %>% count %>% as.numeric,
Rehovot2014   %>% count %>% as.numeric,
Ramla2014     %>% count %>% as.numeric,
Raannana2014  %>% count %>% as.numeric,
tlv2014       %>% count %>% as.numeric,
bene2014      %>% count %>% as.numeric,
bat2014       %>% count %>% as.numeric,
kefar2014     %>% count %>% as.numeric,
petah2014     %>% count %>% as.numeric,
rishon2014    %>% count %>% as.numeric,
ramat2014     %>% count %>% as.numeric,
beer2014      %>% count %>% as.numeric)


## list
survey2004 <<-as.data.frame(rbind(list   = cityList,
                                "2004"  = counts2004,
                                "2005"  = counts2005,
                                "2006"  = counts2006,
                                "2007"  = counts2007,
                                "2008"  = counts2008,
                                "2009"  = counts2009,
                                "2010"  = counts2010,
                                "2011"  = counts2011,
                                "2012"  = counts2012,
                                "2013"  = counts2013,
                                "2014"  = counts2014))



}

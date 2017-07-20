#' subsets the yearly surveys by city of residence, v 0.83
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


cities <<- as.character(levels(family2014$codeloc))
#  cities <- as.character(levels(renters2004$codeloc))
cityList <<- cities[-1]
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
modiin2004         <<- subset(familiesList[[1]], codeloc=="Mod'in")
betshemesh2004     <<- subset(familiesList[[1]], codeloc=="Bet Shemesh")
nahariyya2004      <<- subset(familiesList[[1]], codeloc=="Nahariyya")
qiryatatta2004     <<- subset(familiesList[[1]], codeloc=="Qiryat Atta")
givatayim2004      <<- subset(familiesList[[1]], codeloc=="Giv'atayim")

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
modiin2005         <<- subset(familiesList[[2]], codeloc=="Mod'in")
betshemesh2005     <<- subset(familiesList[[2]], codeloc=="Bet Shemesh")
nahariyya2005      <<- subset(familiesList[[2]], codeloc=="Nahariyya")
qiryatatta2005     <<- subset(familiesList[[2]], codeloc=="Qiryat Atta")
givatayim2005      <<- subset(familiesList[[2]], codeloc=="Giv'atayim")


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
modiin2006         <<- subset(familiesList[[3]], codeloc=="Mod'in")
betshemesh2006     <<- subset(familiesList[[3]], codeloc=="Bet Shemesh")
nahariyya2006      <<- subset(familiesList[[3]], codeloc=="Nahariyya")
qiryatatta2006     <<- subset(familiesList[[3]], codeloc=="Qiryat Atta")
givatayim2006      <<- subset(familiesList[[3]], codeloc=="Giv'atayim")

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
modiin2007         <<- subset(familiesList[[4]], codeloc=="Mod'in")
betshemesh2007     <<- subset(familiesList[[4]], codeloc=="Bet Shemesh")
nahariyya2007      <<- subset(familiesList[[4]], codeloc=="Nahariyya")
qiryatatta2007     <<- subset(familiesList[[4]], codeloc=="Qiryat Atta")
givatayim2007      <<- subset(familiesList[[4]], codeloc=="Giv'atayim")


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
modiin2008         <<- subset(familiesList[[5]], codeloc=="Mod'in")
betshemesh2008     <<- subset(familiesList[[5]], codeloc=="Bet Shemesh")
nahariyya2008      <<- subset(familiesList[[5]], codeloc=="Nahariyya")
qiryatatta2008     <<- subset(familiesList[[5]], codeloc=="Qiryat Atta")
givatayim2008      <<- subset(familiesList[[5]], codeloc=="Giv'atayim")

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
modiin2009         <<- subset(familiesList[[6]], codeloc=="Mod'in")
betshemesh2009     <<- subset(familiesList[[6]], codeloc=="Bet Shemesh")
nahariyya2009      <<- subset(familiesList[[6]], codeloc=="Nahariyya")
qiryatatta2009     <<- subset(familiesList[[6]], codeloc=="Qiryat Atta")
givatayim2009      <<- subset(familiesList[[6]], codeloc=="Giv'atayim")

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
modiin2010         <<- subset(familiesList[[7]], codeloc=="Mod'in")
betshemesh2010     <<- subset(familiesList[[7]], codeloc=="Bet Shemesh")
nahariyya2010      <<- subset(familiesList[[7]], codeloc=="Nahariyya")
qiryatatta2010     <<- subset(familiesList[[7]], codeloc=="Qiryat Atta")
givatayim2010      <<- subset(familiesList[[7]], codeloc=="Giv'atayim")


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
modiin2011         <<- subset(familiesList[[8]], codeloc=="Mod'in")
betshemesh2011     <<- subset(familiesList[[8]], codeloc=="Bet Shemesh")
nahariyya2011      <<- subset(familiesList[[8]], codeloc=="Nahariyya")
qiryatatta2011     <<- subset(familiesList[[8]], codeloc=="Qiryat Atta")
givatayim2011      <<- subset(familiesList[[8]], codeloc=="Giv'atayim")


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
modiin2012         <<- subset(familiesList[[9]], codeloc=="Mod'in")
betshemesh2012     <<- subset(familiesList[[9]], codeloc=="Bet Shemesh")
nahariyya2012      <<- subset(familiesList[[9]], codeloc=="Nahariyya")
qiryatatta2012     <<- subset(familiesList[[9]], codeloc=="Qiryat Atta")
givatayim2012      <<- subset(familiesList[[9]], codeloc=="Giv'atayim")


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
modiin2013         <<- subset(familiesList[[10]], codeloc=="Mod'in")
betshemesh2013     <<- subset(familiesList[[10]], codeloc=="Bet Shemesh")
nahariyya2013      <<- subset(familiesList[[10]], codeloc=="Nahariyya")
qiryatatta2013     <<- subset(familiesList[[10]], codeloc=="Qiryat Atta")
givatayim2013      <<- subset(familiesList[[10]], codeloc=="Giv'atayim")

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
modiin2014         <<- subset(familiesList[[11]], codeloc=="Mod'in")
betshemesh2014     <<- subset(familiesList[[11]], codeloc=="Bet Shemesh")
nahariyya2014      <<- subset(familiesList[[11]], codeloc=="Nahariyya")
qiryatatta2014     <<- subset(familiesList[[11]], codeloc=="Qiryat Atta")
givatayim2014      <<- subset(familiesList[[11]], codeloc=="Giv'atayim")


  ## counts in cities


cities2004 <<- c(
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
                   beer2004,
                 modiin2004,
             betshemesh2004,
              nahariyya2004,
             qiryatatta2004,
              givatayim2004)

cities2005 <<- c(
              jerusalem2005,
                 Ashdod2005  ,
                  Haifa2005  ,
              Herzliyya2005 ,
                  Holon2005   ,
                    Lod2005    ,
                 Hadera2005,
               Ashqelon2005,
               Nettanya2005,
                Rehovot2005 ,
                  Ramla2005  ,
               Raannana2005,
                    tlv2005    ,
                   bene2005  ,
                    bat2005  ,
                  kefar2005,
                  petah2005  ,
                 rishon2005,
                  ramat2005,
                   beer2005,
                 modiin2005,
             betshemesh2005,
              nahariyya2005,
             qiryatatta2005,
              givatayim2005)


cities2006 <<- c(
              jerusalem2006,
                 Ashdod2006  ,
                  Haifa2006  ,
              Herzliyya2006 ,
                  Holon2006   ,
                    Lod2006    ,
                 Hadera2006,
               Ashqelon2006,
               Nettanya2006,
                Rehovot2006 ,
                  Ramla2006  ,
               Raannana2006,
                    tlv2006    ,
                   bene2006  ,
                    bat2006  ,
                  kefar2006,
                  petah2006  ,
                 rishon2006,
                  ramat2006,
                   beer2006,
                 modiin2006,
             betshemesh2006,
              nahariyya2006,
             qiryatatta2006,
              givatayim2006)


cities2007 <<- c(
              jerusalem2007,
                 Ashdod2007  ,
                  Haifa2007  ,
              Herzliyya2007 ,
                  Holon2007   ,
                    Lod2007    ,
                 Hadera2007,
               Ashqelon2007,
               Nettanya2007,
                Rehovot2007 ,
                  Ramla2007  ,
               Raannana2007,
                    tlv2007    ,
                   bene2007  ,
                    bat2007  ,
                  kefar2007,
                  petah2007  ,
                 rishon2007,
                  ramat2007,
                   beer2007,
                 modiin2007,
             betshemesh2007,
              nahariyya2007,
             qiryatatta2007,
              givatayim2007)


cities2008 <<- c(
              jerusalem2008,
                 Ashdod2008  ,
                  Haifa2008  ,
              Herzliyya2008 ,
                  Holon2008   ,
                    Lod2008    ,
                 Hadera2008,
               Ashqelon2008,
               Nettanya2008,
                Rehovot2008 ,
                  Ramla2008  ,
               Raannana2008,
                    tlv2008    ,
                   bene2008  ,
                    bat2008  ,
                  kefar2008,
                  petah2008  ,
                 rishon2008,
                  ramat2008,
                   beer2008,
                 modiin2008,
             betshemesh2008,
              nahariyya2008,
             qiryatatta2008,
              givatayim2008)


cities2009 <<- c(
              jerusalem2009,
                 Ashdod2009,
                  Haifa2009,
              Herzliyya2009,
                  Holon2009,
                    Lod2009,
                 Hadera2009,
               Ashqelon2009,
               Nettanya2009,
                Rehovot2009,
                  Ramla2009,
               Raannana2009,
                    tlv2009,
                   bene2009,
                    bat2009,
                  kefar2009,
                  petah2009,
                 rishon2009,
                  ramat2009,
                   beer2009,
                 modiin2009,
             betshemesh2009,
              nahariyya2009,
             qiryatatta2009,
              givatayim2009)


cities2010 <<- c(
              jerusalem2010,
                 Ashdod2010,
                  Haifa2010,
              Herzliyya2010,
                  Holon2010,
                    Lod2010,
                 Hadera2010,
               Ashqelon2010,
               Nettanya2010,
                Rehovot2010,
                  Ramla2010,
               Raannana2010,
                    tlv2010,
                   bene2010,
                    bat2010,
                  kefar2010,
                  petah2010,
                 rishon2010,
                  ramat2010,
                   beer2010,
                 modiin2010,
             betshemesh2010,
              nahariyya2010,
             qiryatatta2010,
              givatayim2010)

cities2011 <<- c(
              jerusalem2011,
                 Ashdod2011,
                  Haifa2011,
              Herzliyya2011 ,
                  Holon2011,
                    Lod2011,
                 Hadera2011,
               Ashqelon2011,
               Nettanya2011,
                Rehovot2011,
                  Ramla2011,
               Raannana2011,
                    tlv2011,
                   bene2011,
                    bat2011,
                  kefar2011,
                  petah2011,
                 rishon2011,
                  ramat2011,
                   beer2011,
                 modiin2011,
             betshemesh2011,
              nahariyya2011,
             qiryatatta2011,
              givatayim2011)


cities2012 <<- c(
              jerusalem2012,
                 Ashdod2012,
                  Haifa2012,
              Herzliyya2012,
                  Holon2012,
                    Lod2012,
                 Hadera2012,
               Ashqelon2012,
               Nettanya2012,
                Rehovot2012,
                  Ramla2012,
               Raannana2012,
                    tlv2012,
                   bene2012,
                    bat2012,
                  kefar2012,
                  petah2012,
                 rishon2012,
                  ramat2012,
                   beer2012,
                 modiin2012,
             betshemesh2012,
              nahariyya2012,
             qiryatatta2012,
              givatayim2012)

cities2013 <<- c(
              jerusalem2013,
                 Ashdod2013,
                  Haifa2013,
              Herzliyya2013,
                  Holon2013,
                    Lod2013,
                 Hadera2013,
               Ashqelon2013,
               Nettanya2013,
                Rehovot2013 ,
                  Ramla2013,
               Raannana2013,
                    tlv2013,
                   bene2013,
                    bat2013,
                  kefar2013,
                  petah2013,
                 rishon2013,
                  ramat2013,
                   beer2013,
                 modiin2013,
             betshemesh2013,
              nahariyya2013,
             qiryatatta2013,
              givatayim2013)


cities2014 <<- c(
              jerusalem2014,
                 Ashdod2014,
                  Haifa2014,
              Herzliyya2014,
                  Holon2014,
                    Lod2014,
                 Hadera2014,
               Ashqelon2014,
               Nettanya2014,
                Rehovot2014,
                  Ramla2014,
               Raannana2014,
                    tlv2014,
                   bene2014,
                    bat2014,
                  kefar2014,
                  petah2014,
                 rishon2014,
                  ramat2014,
                   beer2014,
                 modiin2014,
             betshemesh2014,
              nahariyya2014,
             qiryatatta2014,
              givatayim2014)

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
beer2004      %>% count %>% as.numeric,
modiin2004      %>% count %>% as.numeric,
betshemesh2004  %>% count %>% as.numeric,
nahariyya2004   %>% count %>% as.numeric,
qiryatatta2004  %>% count %>% as.numeric,
givatayim2004   %>% count %>% as.numeric)


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
beer2005      %>% count %>% as.numeric,
modiin2005      %>% count %>% as.numeric,
betshemesh2005  %>% count %>% as.numeric,
nahariyya2005   %>% count %>% as.numeric,
qiryatatta2005  %>% count %>% as.numeric,
givatayim2005   %>% count %>% as.numeric)

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
beer2006      %>% count %>% as.numeric,
modiin2006      %>% count %>% as.numeric,
betshemesh2006  %>% count %>% as.numeric,
nahariyya2006   %>% count %>% as.numeric,
qiryatatta2006  %>% count %>% as.numeric,
givatayim2006   %>% count %>% as.numeric)


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
beer2007      %>% count %>% as.numeric,
modiin2007      %>% count %>% as.numeric,
betshemesh2007  %>% count %>% as.numeric,
nahariyya2007   %>% count %>% as.numeric,
qiryatatta2007  %>% count %>% as.numeric,
givatayim2007   %>% count %>% as.numeric)

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
beer2008      %>% count %>% as.numeric,
modiin2008      %>% count %>% as.numeric,
betshemesh2008  %>% count %>% as.numeric,
nahariyya2008   %>% count %>% as.numeric,
qiryatatta2008  %>% count %>% as.numeric,
givatayim2008   %>% count %>% as.numeric)

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
beer2009      %>% count %>% as.numeric,
modiin2009      %>% count %>% as.numeric,
betshemesh2009  %>% count %>% as.numeric,
nahariyya2009   %>% count %>% as.numeric,
qiryatatta2009  %>% count %>% as.numeric,
givatayim2009   %>% count %>% as.numeric)

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
beer2010      %>% count %>% as.numeric,
modiin2010      %>% count %>% as.numeric,
betshemesh2010  %>% count %>% as.numeric,
nahariyya2010   %>% count %>% as.numeric,
qiryatatta2010  %>% count %>% as.numeric,
givatayim2010   %>% count %>% as.numeric)

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
beer2011      %>% count %>% as.numeric,
modiin2011      %>% count %>% as.numeric,
betshemesh2011  %>% count %>% as.numeric,
nahariyya2011   %>% count %>% as.numeric,
qiryatatta2011  %>% count %>% as.numeric,
givatayim2011   %>% count %>% as.numeric)

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
beer2012      %>% count %>% as.numeric,
modiin2012      %>% count %>% as.numeric,
betshemesh2012  %>% count %>% as.numeric,
nahariyya2012   %>% count %>% as.numeric,
qiryatatta2012  %>% count %>% as.numeric,
givatayim2012   %>% count %>% as.numeric)

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
beer2013      %>% count %>% as.numeric,
modiin2013      %>% count %>% as.numeric,
betshemesh2013  %>% count %>% as.numeric,
nahariyya2013   %>% count %>% as.numeric,
qiryatatta2013  %>% count %>% as.numeric,
givatayim2013   %>% count %>% as.numeric)

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
beer2014      %>% count %>% as.numeric,
modiin2014      %>% count %>% as.numeric,
betshemesh2014  %>% count %>% as.numeric,
nahariyya2014   %>% count %>% as.numeric,
qiryatatta2014  %>% count %>% as.numeric,
givatayim2014   %>% count %>% as.numeric)

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

surveyList  <<-as.data.frame(cbind(list   = cityList,
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

#' split ownersYYYY and rentersYYYY by city  v.2.2
#' This function creates subsets of renters and owners by city
#' @param arg Defaults to true.
#' @keywords owners, renters, subset
#' @export
#' @examples
#' ownershipByCity()
#' ownershipByCity(TRUE)
#' ownershipByCity(FALSE)

ownershipByCity <- function(arg = TRUE) {
  if (arg == TRUE) {

##############  define owners #########################

Ownerjerusalem2004      <<- dplyr::filter( owners2004 , CODELOC =="Jerusalem")
OwnerAshdod2004         <<- dplyr::filter( owners2004 , CODELOC =="Ashdod")
OwnerHaifa2004          <<- dplyr::filter( owners2004 , CODELOC =="Haifa")
OwnerHerzliyya2004      <<- dplyr::filter( owners2004 , CODELOC =="Herzliyya")
OwnerHolon2004          <<- dplyr::filter( owners2004 , CODELOC =="Holon")
OwnerLod2004            <<- dplyr::filter( owners2004 , CODELOC =="Lod")
OwnerHadera2004         <<- dplyr::filter( owners2004 , CODELOC =="Hadera")
OwnerAshqelon2004       <<- dplyr::filter( owners2004 , CODELOC =="Ashqelon")
OwnerNettanya2004       <<- dplyr::filter( owners2004 , CODELOC =="Nettanya")
OwnerRehovot2004        <<- dplyr::filter( owners2004 , CODELOC =="Rehovot")
OwnerRamla2004          <<- dplyr::filter( owners2004 , CODELOC =="Ramla")
OwnerRaannana2004       <<- dplyr::filter( owners2004 , CODELOC =="Raannana")
Ownertlv2004            <<- dplyr::filter( owners2004 , CODELOC =="Tel Aviv-Yafo")
Ownerbene2004           <<- dplyr::filter( owners2004 , CODELOC =="Bene Beraq")
Ownerbat2004            <<- dplyr::filter( owners2004 , CODELOC =="Bat Yam")
Ownerkefar2004          <<- dplyr::filter( owners2004 , CODELOC =="Kefar Sava")
Ownerpetah2004          <<- dplyr::filter( owners2004 , CODELOC =="Petah Tiqwa")
Ownerrishon2004         <<- dplyr::filter( owners2004 , CODELOC =="Rishon LeZiyon")
Ownerramat2004          <<- dplyr::filter( owners2004 , CODELOC =="Ramat Gan")
Ownerbeer2004           <<- dplyr::filter( owners2004 , CODELOC =="Beer Sheva")
Ownermodiin2004         <<- dplyr::filter( owners2004 , CODELOC =="Modi'in")
Ownerbetshemesh2004     <<- dplyr::filter( owners2004 , CODELOC =="Bet Shemesh")
Ownernahariyya2004      <<- dplyr::filter( owners2004 , CODELOC =="Nahariyya")
Ownerqiryatatta2004     <<- dplyr::filter( owners2004 , CODELOC =="Qiryat Atta")
Ownergivatayim2004      <<- dplyr::filter( owners2004 , CODELOC =="Giv'atayim")

Ownerjerusalem2005      <<- dplyr::filter( owners2005,  CODELOC =="Jerusalem")
OwnerAshdod2005         <<- dplyr::filter( owners2005,  CODELOC =="Ashdod")
OwnerHaifa2005          <<- dplyr::filter( owners2005,  CODELOC =="Haifa")
OwnerHerzliyya2005      <<- dplyr::filter( owners2005,  CODELOC =="Herzliyya")
OwnerHolon2005          <<- dplyr::filter( owners2005,  CODELOC =="Holon")
OwnerLod2005            <<- dplyr::filter( owners2005,  CODELOC =="Lod")
OwnerHadera2005         <<- dplyr::filter( owners2005,  CODELOC =="Hadera")
OwnerAshqelon2005       <<- dplyr::filter( owners2005,  CODELOC =="Ashqelon")
OwnerNettanya2005       <<- dplyr::filter( owners2005,  CODELOC =="Nettanya")
OwnerRehovot2005        <<- dplyr::filter( owners2005,  CODELOC =="Rehovot")
OwnerRamla2005          <<- dplyr::filter( owners2005,  CODELOC =="Ramla")
OwnerRaannana2005       <<- dplyr::filter( owners2005,  CODELOC =="Raannana")
Ownertlv2005            <<- dplyr::filter( owners2005,  CODELOC =="Tel Aviv-Yafo")
Ownerbene2005           <<- dplyr::filter( owners2005,  CODELOC =="Bene Beraq")
Ownerbat2005            <<- dplyr::filter( owners2005,  CODELOC =="Bat Yam")
Ownerkefar2005          <<- dplyr::filter( owners2005,  CODELOC =="Kefar Sava")
Ownerpetah2005          <<- dplyr::filter( owners2005,  CODELOC =="Petah Tiqwa")
Ownerrishon2005         <<- dplyr::filter( owners2005,  CODELOC =="Rishon LeZiyon")
Ownerramat2005          <<- dplyr::filter( owners2005,  CODELOC =="Ramat Gan")
Ownerbeer2005           <<- dplyr::filter( owners2005,  CODELOC =="Beer Sheva")
Ownermodiin2005         <<- dplyr::filter( owners2005,  CODELOC =="Modi'in")
Ownerbetshemesh2005     <<- dplyr::filter( owners2005,  CODELOC =="Bet Shemesh")
Ownernahariyya2005      <<- dplyr::filter( owners2005,  CODELOC =="Nahariyya")
Ownerqiryatatta2005     <<- dplyr::filter( owners2005,  CODELOC =="Qiryat Atta")
Ownergivatayim2005      <<- dplyr::filter( owners2005,  CODELOC =="Giv'atayim")

Ownerjerusalem2006      <<- dplyr::filter( owners2006,  CODELOC =="Jerusalem")
OwnerAshdod2006         <<- dplyr::filter( owners2006,  CODELOC =="Ashdod")
OwnerHaifa2006          <<- dplyr::filter( owners2006,  CODELOC =="Haifa")
OwnerHerzliyya2006      <<- dplyr::filter( owners2006,  CODELOC =="Herzliyya")
OwnerHolon2006          <<- dplyr::filter( owners2006,  CODELOC =="Holon")
OwnerLod2006            <<- dplyr::filter( owners2006,  CODELOC =="Lod")
OwnerHadera2006         <<- dplyr::filter( owners2006,  CODELOC =="Hadera")
OwnerAshqelon2006       <<- dplyr::filter( owners2006,  CODELOC =="Ashqelon")
OwnerNettanya2006       <<- dplyr::filter( owners2006,  CODELOC =="Nettanya")
OwnerRehovot2006        <<- dplyr::filter( owners2006,  CODELOC =="Rehovot")
OwnerRamla2006          <<- dplyr::filter( owners2006,  CODELOC =="Ramla")
OwnerRaannana2006       <<- dplyr::filter( owners2006,  CODELOC =="Raannana")
Ownertlv2006            <<- dplyr::filter( owners2006,  CODELOC =="Tel Aviv-Yafo")
Ownerbene2006           <<- dplyr::filter( owners2006,  CODELOC =="Bene Beraq")
Ownerbat2006            <<- dplyr::filter( owners2006,  CODELOC =="Bat Yam")
Ownerkefar2006          <<- dplyr::filter( owners2006,  CODELOC =="Kefar Sava")
Ownerpetah2006          <<- dplyr::filter( owners2006,  CODELOC =="Petah Tiqwa")
Ownerrishon2006         <<- dplyr::filter( owners2006,  CODELOC =="Rishon LeZiyon")
Ownerramat2006          <<- dplyr::filter( owners2006,  CODELOC =="Ramat Gan")
Ownerbeer2006           <<- dplyr::filter( owners2006,  CODELOC =="Beer Sheva")
Ownermodiin2006         <<- dplyr::filter( owners2006,  CODELOC =="Modi'in")
Ownerbetshemesh2006     <<- dplyr::filter( owners2006,  CODELOC =="Bet Shemesh")
Ownernahariyya2006      <<- dplyr::filter( owners2006,  CODELOC =="Nahariyya")
Ownerqiryatatta2006     <<- dplyr::filter( owners2006,  CODELOC =="Qiryat Atta")
Ownergivatayim2006      <<- dplyr::filter( owners2006,  CODELOC =="Giv'atayim")

Ownerjerusalem2007       <<- dplyr::filter( owners2007,  CODELOC =="Jerusalem")
OwnerAshdod2007    <<- dplyr::filter( owners2007,  CODELOC =="Ashdod")
OwnerHaifa2007     <<- dplyr::filter( owners2007,  CODELOC =="Haifa")
OwnerHerzliyya2007 <<- dplyr::filter( owners2007,  CODELOC =="Herzliyya")
OwnerHolon2007    <<- dplyr::filter( owners2007,  CODELOC =="Holon")
OwnerLod2007      <<- dplyr::filter( owners2007,  CODELOC =="Lod")
OwnerHadera2007   <<- dplyr::filter( owners2007,  CODELOC =="Hadera")
OwnerAshqelon2007 <<- dplyr::filter( owners2007,  CODELOC =="Ashkelon")
OwnerNettanya2007 <<- dplyr::filter( owners2007,  CODELOC =="Netanya")
OwnerRehovot2007  <<- dplyr::filter( owners2007,  CODELOC =="Rehovot")
OwnerRamla2007    <<- dplyr::filter( owners2007,  CODELOC =="Ramla")
OwnerRaannana2007 <<- dplyr::filter( owners2007,  CODELOC =="Raannana")
Ownertlv2007      <<- dplyr::filter( owners2007,  CODELOC =="Tel Aviv-Yaffo")
Ownerbene2007     <<- dplyr::filter( owners2007,  CODELOC =="Bene Beraq")
Ownerbat2007      <<- dplyr::filter( owners2007,  CODELOC =="Bat Yam")
Ownerkefar2007    <<- dplyr::filter( owners2007,  CODELOC =="Kefar Sava")
Ownerpetah2007    <<- dplyr::filter( owners2007,  CODELOC =="Petah Tiqwa")
Ownerrishon2007   <<- dplyr::filter( owners2007,  CODELOC =="Rishon LeZiyon")
Ownerramat2007    <<- dplyr::filter( owners2007,  CODELOC =="Ramat Gan")
Ownerbeer2007     <<- dplyr::filter( owners2007,  CODELOC =="Beer Sheva")
Ownermodiin2007         <<- dplyr::filter( owners2007,  CODELOC =="Modi'in")
Ownerbetshemesh2007     <<- dplyr::filter( owners2007,  CODELOC =="Bet Shemesh")
Ownernahariyya2007      <<- dplyr::filter( owners2007,  CODELOC =="Nahariyya")
Ownerqiryatatta2007     <<- dplyr::filter( owners2007,  CODELOC =="Qiryat Atta")
Ownergivatayim2007      <<- dplyr::filter( owners2007,  CODELOC =="Giv'atayim")


Ownerjerusalem2008 <<- dplyr::filter( owners2008,  CODELOC =="Jerusalem")
OwnerAshdod2008    <<- dplyr::filter( owners2008,  CODELOC =="Ashdod")
OwnerHaifa2008     <<- dplyr::filter( owners2008,  CODELOC =="Haifa")
OwnerHerzliyya2008 <<- dplyr::filter( owners2008,  CODELOC =="Herzliyya")
OwnerHolon2008    <<- dplyr::filter( owners2008,  CODELOC =="Holon")
OwnerLod2008      <<- dplyr::filter( owners2008,  CODELOC =="Lod")
OwnerHadera2008   <<- dplyr::filter( owners2008,  CODELOC =="Hadera")
OwnerAshqelon2008 <<- dplyr::filter( owners2008,  CODELOC =="Ashkelon")
OwnerNettanya2008 <<- dplyr::filter( owners2008,  CODELOC =="Netanya")
OwnerRehovot2008  <<- dplyr::filter( owners2008,  CODELOC =="Rehovot")
OwnerRamla2008    <<- dplyr::filter( owners2008,  CODELOC =="Ramla")
OwnerRaannana2008 <<- dplyr::filter( owners2008,  CODELOC =="Raannana")
Ownertlv2008      <<- dplyr::filter( owners2008,  CODELOC =="Tel Aviv-Yaffo")
Ownerbene2008     <<- dplyr::filter( owners2008,  CODELOC =="Bene Beraq")
Ownerbat2008      <<- dplyr::filter( owners2008,  CODELOC =="Bat Yam")
Ownerkefar2008    <<- dplyr::filter( owners2008,  CODELOC =="Kefar Sava")
Ownerpetah2008    <<- dplyr::filter( owners2008,  CODELOC =="Petah Tiqwa")
Ownerrishon2008   <<- dplyr::filter( owners2008,  CODELOC =="Rishon LeZiyon")
Ownerramat2008    <<- dplyr::filter( owners2008,  CODELOC =="Ramat Gan")
Ownerbeer2008     <<- dplyr::filter( owners2008,  CODELOC =="Beer Sheva")
Ownermodiin2008         <<- dplyr::filter( owners2008,  CODELOC =="Modi'in")
Ownerbetshemesh2008     <<- dplyr::filter( owners2008,  CODELOC =="Bet Shemesh")
Ownernahariyya2008      <<- dplyr::filter( owners2008,  CODELOC =="Nahariyya")
Ownerqiryatatta2008     <<- dplyr::filter( owners2008,  CODELOC =="Qiryat Atta")
Ownergivatayim2008      <<- dplyr::filter( owners2008,  CODELOC =="Giv'atayim")

Ownerjerusalem2009 <<- dplyr::filter( owners2009,  CODELOC =="Jerusalem")
OwnerAshdod2009    <<- dplyr::filter( owners2009,  CODELOC =="Ashdod")
OwnerHaifa2009     <<- dplyr::filter( owners2009,  CODELOC =="Haifa")
OwnerHerzliyya2009 <<- dplyr::filter( owners2009,  CODELOC =="Herzliyya")
OwnerHolon2009    <<- dplyr::filter( owners2009,  CODELOC =="Holon")
OwnerLod2009      <<- dplyr::filter( owners2009,  CODELOC =="Lod")
OwnerHadera2009   <<- dplyr::filter( owners2009,  CODELOC =="Hadera")
OwnerAshqelon2009 <<- dplyr::filter( owners2009,  CODELOC =="Ashkelon")
OwnerNettanya2009 <<- dplyr::filter( owners2009,  CODELOC =="Netanya")
OwnerRehovot2009  <<- dplyr::filter( owners2009,  CODELOC =="Rehovot")
OwnerRamla2009    <<- dplyr::filter( owners2009,  CODELOC =="Ramla")
OwnerRaannana2009 <<- dplyr::filter( owners2009,  CODELOC =="Raannana")
Ownertlv2009      <<- dplyr::filter( owners2009,  CODELOC =="Tel Aviv-Yaffo")
Ownerbene2009     <<- dplyr::filter( owners2009,  CODELOC =="Bene Beraq")
Ownerbat2009      <<- dplyr::filter( owners2009,  CODELOC =="Bat Yam")
Ownerkefar2009    <<- dplyr::filter( owners2009,  CODELOC =="Kefar Sava")
Ownerpetah2009    <<- dplyr::filter( owners2009,  CODELOC =="Petah Tiqwa")
Ownerrishon2009   <<- dplyr::filter( owners2009,  CODELOC =="Rishon LeZiyon")
Ownerramat2009    <<- dplyr::filter( owners2009,  CODELOC =="Ramat Gan")
Ownerbeer2009     <<- dplyr::filter( owners2009,  CODELOC =="Beer Sheva")
Ownermodiin2009         <<- dplyr::filter( owners2009,  CODELOC =="Modi'in")
Ownerbetshemesh2009     <<- dplyr::filter( owners2009,  CODELOC =="Bet Shemesh")
Ownernahariyya2009      <<- dplyr::filter( owners2009,  CODELOC =="Nahariyya")
Ownerqiryatatta2009     <<- dplyr::filter( owners2009,  CODELOC =="Qiryat Atta")
Ownergivatayim2009      <<- dplyr::filter( owners2009,  CODELOC =="Giv'atayim")

Ownerjerusalem2010       <<- dplyr::filter( owners2010,  CODELOC =="Jerusalem")
OwnerAshdod2010    <<- dplyr::filter( owners2010,  CODELOC =="Ashdod")
OwnerHaifa2010     <<- dplyr::filter( owners2010,  CODELOC =="Haifa")
OwnerHerzliyya2010 <<- dplyr::filter( owners2010,  CODELOC =="Herzliyya")
OwnerHolon2010    <<- dplyr::filter( owners2010,  CODELOC =="Holon")
OwnerLod2010      <<- dplyr::filter( owners2010,  CODELOC =="Lod")
OwnerHadera2010   <<- dplyr::filter( owners2010,  CODELOC =="Hadera")
OwnerAshqelon2010 <<- dplyr::filter( owners2010,  CODELOC =="Ashkelon")
OwnerNettanya2010 <<- dplyr::filter( owners2010,  CODELOC =="Netanya")
OwnerRehovot2010  <<- dplyr::filter( owners2010,  CODELOC =="Rehovot")
OwnerRamla2010    <<- dplyr::filter( owners2010,  CODELOC =="Ramla")
OwnerRaannana2010 <<- dplyr::filter( owners2010,  CODELOC =="Raannana")
Ownertlv2010      <<- dplyr::filter( owners2010,  CODELOC =="Tel Aviv-Yaffo")
Ownerbene2010     <<- dplyr::filter( owners2010,  CODELOC =="Bene Beraq")
Ownerbat2010      <<- dplyr::filter( owners2010,  CODELOC =="Bat Yam")
Ownerkefar2010    <<- dplyr::filter( owners2010,  CODELOC =="Kefar Sava")
Ownerpetah2010    <<- dplyr::filter( owners2010,  CODELOC =="Petah Tiqwa")
Ownerrishon2010   <<- dplyr::filter( owners2010,  CODELOC =="Rishon LeZiyon")
Ownerramat2010    <<- dplyr::filter( owners2010,  CODELOC =="Ramat Gan")
Ownerbeer2010     <<- dplyr::filter( owners2010,  CODELOC =="Beer Sheva")
Ownermodiin2010         <<- dplyr::filter( owners2010,  CODELOC =="Modi'in")
Ownerbetshemesh2010     <<- dplyr::filter( owners2010,  CODELOC =="Bet Shemesh")
Ownernahariyya2010      <<- dplyr::filter( owners2010,  CODELOC =="Nahariyya")
Ownerqiryatatta2010     <<- dplyr::filter( owners2010,  CODELOC =="Qiryat Atta")
Ownergivatayim2010      <<- dplyr::filter( owners2010,  CODELOC =="Giv'atayim")

Ownerjerusalem2011 <<- dplyr::filter( owners2011,  CODELOC =="Jerusalem")
OwnerAshdod2011    <<- dplyr::filter( owners2011,  CODELOC =="Ashdod")
OwnerHaifa2011     <<- dplyr::filter( owners2011,  CODELOC =="Haifa")
OwnerHerzliyya2011 <<- dplyr::filter( owners2011,  CODELOC =="Herzliyya")
OwnerHolon2011    <<- dplyr::filter( owners2011,  CODELOC =="Holon")
OwnerLod2011      <<- dplyr::filter( owners2011,  CODELOC =="Lod")
OwnerHadera2011   <<- dplyr::filter( owners2011,  CODELOC =="Hadera")
OwnerAshqelon2011 <<- dplyr::filter( owners2011,  CODELOC =="Ashkelon")
OwnerNettanya2011 <<- dplyr::filter( owners2011,  CODELOC =="Netanya")
OwnerRehovot2011  <<- dplyr::filter( owners2011,  CODELOC =="Rehovot")
OwnerRamla2011    <<- dplyr::filter( owners2011,  CODELOC =="Ramla")
OwnerRaannana2011 <<- dplyr::filter( owners2011,  CODELOC =="Raannana")
Ownertlv2011      <<- dplyr::filter( owners2011,  CODELOC =="Tel Aviv-Yaffo")
Ownerbene2011     <<- dplyr::filter( owners2011,  CODELOC =="Bene Beraq")
Ownerbat2011      <<- dplyr::filter( owners2011,  CODELOC =="Bat Yam")
Ownerkefar2011    <<- dplyr::filter( owners2011,  CODELOC =="Kefar Sava")
Ownerpetah2011    <<- dplyr::filter( owners2011,  CODELOC =="Petah Tiqwa")
Ownerrishon2011   <<- dplyr::filter( owners2011,  CODELOC =="Rishon LeZiyon")
Ownerramat2011    <<- dplyr::filter( owners2011,  CODELOC =="Ramat Gan")
Ownerbeer2011     <<- dplyr::filter( owners2011,  CODELOC =="Beer Sheva")
Ownermodiin2011         <<- dplyr::filter( owners2011,  CODELOC =="Modi'in")
Ownerbetshemesh2011     <<- dplyr::filter( owners2011,  CODELOC =="Bet Shemesh")
Ownernahariyya2011      <<- dplyr::filter( owners2011,  CODELOC =="Nahariyya")
Ownerqiryatatta2011     <<- dplyr::filter( owners2011,  CODELOC =="Qiryat Atta")
Ownergivatayim2011      <<- dplyr::filter( owners2011,  CODELOC =="Giv'atayim")


Ownerjerusalem2012 <<- dplyr::filter( owners2012,  CODELOC =="Jerusalem")
OwnerAshdod2012    <<- dplyr::filter( owners2012,  CODELOC =="Ashdod")
OwnerHaifa2012     <<- dplyr::filter( owners2012,  CODELOC =="Haifa")
OwnerHerzliyya2012 <<- dplyr::filter( owners2012,  CODELOC =="Herzliyya")
OwnerHolon2012    <<- dplyr::filter( owners2012,  CODELOC =="Holon")
OwnerLod2012      <<- dplyr::filter( owners2012,  CODELOC =="Lod")
OwnerHadera2012   <<- dplyr::filter( owners2012,  CODELOC =="Hadera")
OwnerAshqelon2012 <<- dplyr::filter( owners2012,  CODELOC =="Ashkelon")
OwnerNettanya2012 <<- dplyr::filter( owners2012,  CODELOC =="Netanya")
OwnerRehovot2012  <<- dplyr::filter( owners2012,  CODELOC =="Rehovot")
OwnerRamla2012    <<- dplyr::filter( owners2012,  CODELOC =="Ramla")
OwnerRaannana2012 <<- dplyr::filter( owners2012,  CODELOC =="Raannana")
Ownertlv2012      <<- dplyr::filter( owners2012,  CODELOC =="Tel Aviv-Yaffo")
Ownerbene2012     <<- dplyr::filter( owners2012,  CODELOC =="Bene Beraq")
Ownerbat2012      <<- dplyr::filter( owners2012,  CODELOC =="Bat Yam")
Ownerkefar2012    <<- dplyr::filter( owners2012,  CODELOC =="Kefar Sava")
Ownerpetah2012    <<- dplyr::filter( owners2012,  CODELOC =="Petah Tiqwa")
Ownerrishon2012   <<- dplyr::filter( owners2012,  CODELOC =="Rishon LeZiyon")
Ownerramat2012    <<- dplyr::filter( owners2012,  CODELOC =="Ramat Gan")
Ownerbeer2012     <<- dplyr::filter( owners2012,  CODELOC =="Beer Sheva")
Ownermodiin2012         <<- dplyr::filter( owners2012,  CODELOC =="Modi'in")
Ownerbetshemesh2012     <<- dplyr::filter( owners2012,  CODELOC =="Bet Shemesh")
Ownernahariyya2012      <<- dplyr::filter( owners2012,  CODELOC =="Nahariyya")
Ownerqiryatatta2012     <<- dplyr::filter( owners2012,  CODELOC =="Qiryat Atta")
Ownergivatayim2012      <<- dplyr::filter( owners2012,  CODELOC =="Giv'atayim")

Ownerjerusalem2013 <<- dplyr::filter( owners2013,  CODELOC =="Jerusalem")
OwnerAshdod2013    <<- dplyr::filter( owners2013, CODELOC =="Ashdod")
OwnerHaifa2013     <<- dplyr::filter( owners2013, CODELOC =="Haifa")
OwnerHerzliyya2013 <<- dplyr::filter( owners2013, CODELOC =="Herzliyya")
OwnerHolon2013    <<- dplyr::filter( owners2013, CODELOC =="Holon")
OwnerLod2013      <<- dplyr::filter( owners2013, CODELOC =="Lod")
OwnerHadera2013   <<- dplyr::filter( owners2013, CODELOC =="Hadera")
OwnerAshqelon2013 <<- dplyr::filter( owners2013, CODELOC =="Ashkelon")
OwnerNettanya2013 <<- dplyr::filter( owners2013, CODELOC =="Netanya")
OwnerRehovot2013  <<- dplyr::filter( owners2013, CODELOC =="Rehovot")
OwnerRamla2013    <<- dplyr::filter( owners2013, CODELOC =="Ramla")
OwnerRaannana2013 <<- dplyr::filter( owners2013, CODELOC =="Raannana")
Ownertlv2013      <<- dplyr::filter( owners2013, CODELOC =="Tel Aviv-Yaffo")
Ownerbene2013     <<- dplyr::filter( owners2013, CODELOC =="Bene Beraq")
Ownerbat2013      <<- dplyr::filter( owners2013, CODELOC =="Bat Yam")
Ownerkefar2013    <<- dplyr::filter( owners2013, CODELOC =="Kefar Sava")
Ownerpetah2013    <<- dplyr::filter( owners2013, CODELOC =="Petah Tiqwa")
Ownerrishon2013   <<- dplyr::filter( owners2013, CODELOC =="Rishon LeZiyon")
Ownerramat2013    <<- dplyr::filter( owners2013, CODELOC =="Ramat Gan")
Ownerbeer2013     <<- dplyr::filter( owners2013, CODELOC =="Beer Sheva")
Ownermodiin2013         <<- dplyr::filter( owners2013, CODELOC =="Modi'in")
Ownerbetshemesh2013     <<- dplyr::filter( owners2013, CODELOC =="Bet Shemesh")
Ownernahariyya2013      <<- dplyr::filter( owners2013, CODELOC =="Nahariyya")
Ownerqiryatatta2013     <<- dplyr::filter( owners2013, CODELOC =="Qiryat Atta")
Ownergivatayim2013      <<- dplyr::filter( owners2013, CODELOC =="Giv'atayim")

Ownerjerusalem2014 <<- dplyr::filter( owners2014,  CODELOC =="Jerusalem")
OwnerAshdod2014    <<- dplyr::filter( owners2014,  CODELOC =="Ashdod")
OwnerHaifa2014     <<- dplyr::filter( owners2014,  CODELOC =="Haifa")
OwnerHerzliyya2014 <<- dplyr::filter( owners2014,  CODELOC =="Herzliyya")
OwnerHolon2014    <<- dplyr::filter( owners2014,  CODELOC =="Holon")
OwnerLod2014      <<- dplyr::filter( owners2014,  CODELOC =="Lod")
OwnerHadera2014   <<- dplyr::filter( owners2014,  CODELOC =="Hadera")
OwnerAshqelon2014 <<- dplyr::filter( owners2014,  CODELOC =="Ashkelon")
OwnerNettanya2014 <<- dplyr::filter( owners2014,  CODELOC =="Netanya")
OwnerRehovot2014  <<- dplyr::filter( owners2014,  CODELOC =="Rehovot")
OwnerRamla2014    <<- dplyr::filter( owners2014,  CODELOC =="Ramla")
OwnerRaannana2014 <<- dplyr::filter( owners2014,  CODELOC =="Raannana")
Ownertlv2014      <<- dplyr::filter( owners2014,  CODELOC =="Tel Aviv-Yaffo")
Ownerbene2014     <<- dplyr::filter( owners2014,  CODELOC =="Bene Beraq")
Ownerbat2014      <<- dplyr::filter( owners2014,  CODELOC =="Bat Yam")
Ownerkefar2014    <<- dplyr::filter( owners2014,  CODELOC =="Kefar Sava")
Ownerpetah2014    <<- dplyr::filter( owners2014,  CODELOC =="Petah Tiqwa")
Ownerrishon2014   <<- dplyr::filter( owners2014,  CODELOC =="Rishon LeZiyon")
Ownerramat2014    <<- dplyr::filter( owners2014,  CODELOC =="Ramat Gan")
Ownerbeer2014     <<- dplyr::filter( owners2014,  CODELOC =="Beer Sheva")
Ownermodiin2014   <<- dplyr::filter( owners2014,  CODELOC =="Modi'in")
Ownerbetshemesh2014 <<- dplyr::filter( owners2014,  CODELOC =="Bet Shemesh")
Ownernahariyya2014  <<- dplyr::filter( owners2014,  CODELOC =="Nahariyya")
Ownerqiryatatta2014 <<- dplyr::filter( owners2014,  CODELOC =="Qiryat Ata")
Ownergivatayim2014  <<- dplyr::filter( owners2014,  CODELOC =="Giv'atayim")


##############  end define owners  #########################


##############  define     renters  #########################

Renterjerusalem2004      <<- dplyr::filter( renters2004 , CODELOC =="Jerusalem")
RenterAshdod2004         <<- dplyr::filter( renters2004 , CODELOC =="Ashdod")
RenterHaifa2004          <<- dplyr::filter( renters2004 , CODELOC =="Haifa")
RenterHerzliyya2004      <<- dplyr::filter( renters2004 , CODELOC =="Herzliyya")
RenterHolon2004          <<- dplyr::filter( renters2004 , CODELOC =="Holon")
RenterLod2004            <<- dplyr::filter( renters2004 , CODELOC =="Lod")
RenterHadera2004         <<- dplyr::filter( renters2004 , CODELOC =="Hadera")
RenterAshqelon2004       <<- dplyr::filter( renters2004 , CODELOC =="Ashqelon")
RenterNettanya2004       <<- dplyr::filter( renters2004 , CODELOC =="Nettanya")
RenterRehovot2004        <<- dplyr::filter( renters2004 , CODELOC =="Rehovot")
RenterRamla2004          <<- dplyr::filter( renters2004 , CODELOC =="Ramla")
RenterRaannana2004       <<- dplyr::filter( renters2004 , CODELOC =="Raannana")
Rentertlv2004            <<- dplyr::filter( renters2004 , CODELOC =="Tel Aviv-Yafo")
Renterbene2004           <<- dplyr::filter( renters2004 , CODELOC =="Bene Beraq")
Renterbat2004            <<- dplyr::filter( renters2004 , CODELOC =="Bat Yam")
Renterkefar2004          <<- dplyr::filter( renters2004 , CODELOC =="Kefar Sava")
Renterpetah2004          <<- dplyr::filter( renters2004 , CODELOC =="Petah Tiqwa")
Renterrishon2004         <<- dplyr::filter( renters2004 , CODELOC =="Rishon LeZiyon")
Renterramat2004          <<- dplyr::filter( renters2004 , CODELOC =="Ramat Gan")
Renterbeer2004           <<- dplyr::filter( renters2004 , CODELOC =="Beer Sheva")
Rentermodiin2004         <<- dplyr::filter( renters2004 , CODELOC =="Modi'in")
Renterbetshemesh2004     <<- dplyr::filter( renters2004 , CODELOC =="Bet Shemesh")
Renternahariyya2004      <<- dplyr::filter( renters2004 , CODELOC =="Nahariyya")
Renterqiryatatta2004     <<- dplyr::filter( renters2004 , CODELOC =="Qiryat Atta")
Rentergivatayim2004      <<- dplyr::filter( renters2004 , CODELOC =="Giv'atayim")

Renterjerusalem2005      <<- dplyr::filter( renters2005,  CODELOC =="Jerusalem")
RenterAshdod2005         <<- dplyr::filter( renters2005,  CODELOC =="Ashdod")
RenterHaifa2005          <<- dplyr::filter( renters2005,  CODELOC =="Haifa")
RenterHerzliyya2005      <<- dplyr::filter( renters2005,  CODELOC =="Herzliyya")
RenterHolon2005          <<- dplyr::filter( renters2005,  CODELOC =="Holon")
RenterLod2005            <<- dplyr::filter( renters2005,  CODELOC =="Lod")
RenterHadera2005         <<- dplyr::filter( renters2005,  CODELOC =="Hadera")
RenterAshqelon2005       <<- dplyr::filter( renters2005,  CODELOC =="Ashqelon")
RenterNettanya2005       <<- dplyr::filter( renters2005,  CODELOC =="Nettanya")
RenterRehovot2005        <<- dplyr::filter( renters2005,  CODELOC =="Rehovot")
RenterRamla2005          <<- dplyr::filter( renters2005,  CODELOC =="Ramla")
RenterRaannana2005       <<- dplyr::filter( renters2005,  CODELOC =="Raannana")
Rentertlv2005            <<- dplyr::filter( renters2005,  CODELOC =="Tel Aviv-Yafo")
Renterbene2005           <<- dplyr::filter( renters2005,  CODELOC =="Bene Beraq")
Renterbat2005            <<- dplyr::filter( renters2005,  CODELOC =="Bat Yam")
Renterkefar2005          <<- dplyr::filter( renters2005,  CODELOC =="Kefar Sava")
Renterpetah2005          <<- dplyr::filter( renters2005,  CODELOC =="Petah Tiqwa")
Renterrishon2005         <<- dplyr::filter( renters2005,  CODELOC =="Rishon LeZiyon")
Renterramat2005          <<- dplyr::filter( renters2005,  CODELOC =="Ramat Gan")
Renterbeer2005           <<- dplyr::filter( renters2005,  CODELOC =="Beer Sheva")
Rentermodiin2005         <<- dplyr::filter( renters2005,  CODELOC =="Modi'in")
Renterbetshemesh2005     <<- dplyr::filter( renters2005,  CODELOC =="Bet Shemesh")
Renternahariyya2005      <<- dplyr::filter( renters2005,  CODELOC =="Nahariyya")
Renterqiryatatta2005     <<- dplyr::filter( renters2005,  CODELOC =="Qiryat Atta")
Rentergivatayim2005      <<- dplyr::filter( renters2005,  CODELOC =="Giv'atayim")

Renterjerusalem2006      <<- dplyr::filter( renters2006,  CODELOC =="Jerusalem")
RenterAshdod2006         <<- dplyr::filter( renters2006,  CODELOC =="Ashdod")
RenterHaifa2006          <<- dplyr::filter( renters2006,  CODELOC =="Haifa")
RenterHerzliyya2006      <<- dplyr::filter( renters2006,  CODELOC =="Herzliyya")
RenterHolon2006          <<- dplyr::filter( renters2006,  CODELOC =="Holon")
RenterLod2006            <<- dplyr::filter( renters2006,  CODELOC =="Lod")
RenterHadera2006         <<- dplyr::filter( renters2006,  CODELOC =="Hadera")
RenterAshqelon2006       <<- dplyr::filter( renters2006,  CODELOC =="Ashqelon")
RenterNettanya2006       <<- dplyr::filter( renters2006,  CODELOC =="Nettanya")
RenterRehovot2006        <<- dplyr::filter( renters2006,  CODELOC =="Rehovot")
RenterRamla2006          <<- dplyr::filter( renters2006,  CODELOC =="Ramla")
RenterRaannana2006       <<- dplyr::filter( renters2006,  CODELOC =="Raannana")
Rentertlv2006            <<- dplyr::filter( renters2006,  CODELOC =="Tel Aviv-Yafo")
Renterbene2006           <<- dplyr::filter( renters2006,  CODELOC =="Bene Beraq")
Renterbat2006            <<- dplyr::filter( renters2006,  CODELOC =="Bat Yam")
Renterkefar2006          <<- dplyr::filter( renters2006,  CODELOC =="Kefar Sava")
Renterpetah2006          <<- dplyr::filter( renters2006,  CODELOC =="Petah Tiqwa")
Renterrishon2006         <<- dplyr::filter( renters2006,  CODELOC =="Rishon LeZiyon")
Renterramat2006          <<- dplyr::filter( renters2006,  CODELOC =="Ramat Gan")
Renterbeer2006           <<- dplyr::filter( renters2006,  CODELOC =="Beer Sheva")
Rentermodiin2006         <<- dplyr::filter( renters2006,  CODELOC =="Modi'in")
Renterbetshemesh2006     <<- dplyr::filter( renters2006,  CODELOC =="Bet Shemesh")
Renternahariyya2006      <<- dplyr::filter( renters2006,  CODELOC =="Nahariyya")
Renterqiryatatta2006     <<- dplyr::filter( renters2006,  CODELOC =="Qiryat Atta")
Rentergivatayim2006      <<- dplyr::filter( renters2006,  CODELOC =="Giv'atayim")

Renterjerusalem2007 <<- dplyr::filter( renters2007,  CODELOC =="Jerusalem")
RenterAshdod2007    <<- dplyr::filter( renters2007,  CODELOC =="Ashdod")
RenterHaifa2007     <<- dplyr::filter( renters2007,  CODELOC =="Haifa")
RenterHerzliyya2007 <<- dplyr::filter( renters2007,  CODELOC =="Herzliyya")
RenterHolon2007    <<- dplyr::filter( renters2007,  CODELOC =="Holon")
RenterLod2007      <<- dplyr::filter( renters2007,  CODELOC =="Lod")
RenterHadera2007   <<- dplyr::filter( renters2007,  CODELOC =="Hadera")
RenterAshqelon2007 <<- dplyr::filter( renters2007,  CODELOC =="Ashkelon")
RenterNettanya2007 <<- dplyr::filter( renters2007,  CODELOC =="Netanya")
RenterRehovot2007  <<- dplyr::filter( renters2007,  CODELOC =="Rehovot")
RenterRamla2007    <<- dplyr::filter( renters2007,  CODELOC =="Ramla")
RenterRaannana2007 <<- dplyr::filter( renters2007,  CODELOC =="Raannana")
Rentertlv2007      <<- dplyr::filter( renters2007,  CODELOC =="Tel Aviv-Yaffo")
Renterbene2007     <<- dplyr::filter( renters2007,  CODELOC =="Bene Beraq")
Renterbat2007      <<- dplyr::filter( renters2007,  CODELOC =="Bat Yam")
Renterkefar2007    <<- dplyr::filter( renters2007,  CODELOC =="Kefar Sava")
Renterpetah2007    <<- dplyr::filter( renters2007,  CODELOC =="Petah Tiqwa")
Renterrishon2007   <<- dplyr::filter( renters2007,  CODELOC =="Rishon LeZiyon")
Renterramat2007    <<- dplyr::filter( renters2007,  CODELOC =="Ramat Gan")
Renterbeer2007     <<- dplyr::filter( renters2007,  CODELOC =="Beer Sheva")
Rentermodiin2007         <<- dplyr::filter( renters2007,  CODELOC =="Modi'in")
Renterbetshemesh2007     <<- dplyr::filter( renters2007,  CODELOC =="Bet Shemesh")
Renternahariyya2007      <<- dplyr::filter( renters2007,  CODELOC =="Nahariyya")
Renterqiryatatta2007     <<- dplyr::filter( renters2007,  CODELOC =="Qiryat Atta")
Rentergivatayim2007      <<- dplyr::filter( renters2007,  CODELOC =="Giv'atayim")


Renterjerusalem2008 <<- dplyr::filter( renters2008,  CODELOC =="Jerusalem")
RenterAshdod2008    <<- dplyr::filter( renters2008,  CODELOC =="Ashdod")
RenterHaifa2008     <<- dplyr::filter( renters2008,  CODELOC =="Haifa")
RenterHerzliyya2008 <<- dplyr::filter( renters2008,  CODELOC =="Herzliyya")
RenterHolon2008    <<- dplyr::filter( renters2008,  CODELOC =="Holon")
RenterLod2008      <<- dplyr::filter( renters2008,  CODELOC =="Lod")
RenterHadera2008   <<- dplyr::filter( renters2008,  CODELOC =="Hadera")
RenterAshqelon2008 <<- dplyr::filter( renters2008,  CODELOC =="Ashkelon")
RenterNettanya2008 <<- dplyr::filter( renters2008,  CODELOC =="Netanya")
RenterRehovot2008  <<- dplyr::filter( renters2008,  CODELOC =="Rehovot")
RenterRamla2008    <<- dplyr::filter( renters2008,  CODELOC =="Ramla")
RenterRaannana2008 <<- dplyr::filter( renters2008,  CODELOC =="Raannana")
Rentertlv2008      <<- dplyr::filter( renters2008,  CODELOC =="Tel Aviv-Yaffo")
Renterbene2008     <<- dplyr::filter( renters2008,  CODELOC =="Bene Beraq")
Renterbat2008      <<- dplyr::filter( renters2008,  CODELOC =="Bat Yam")
Renterkefar2008    <<- dplyr::filter( renters2008,  CODELOC =="Kefar Sava")
Renterpetah2008    <<- dplyr::filter( renters2008,  CODELOC =="Petah Tiqwa")
Renterrishon2008   <<- dplyr::filter( renters2008,  CODELOC =="Rishon LeZiyon")
Renterramat2008    <<- dplyr::filter( renters2008,  CODELOC =="Ramat Gan")
Renterbeer2008     <<- dplyr::filter( renters2008,  CODELOC =="Beer Sheva")
Rentermodiin2008         <<- dplyr::filter( renters2008,  CODELOC =="Modi'in")
Renterbetshemesh2008     <<- dplyr::filter( renters2008,  CODELOC =="Bet Shemesh")
Renternahariyya2008      <<- dplyr::filter( renters2008,  CODELOC =="Nahariyya")
Renterqiryatatta2008     <<- dplyr::filter( renters2008,  CODELOC =="Qiryat Atta")
Rentergivatayim2008      <<- dplyr::filter( renters2008,  CODELOC =="Giv'atayim")

Renterjerusalem2009 <<- dplyr::filter( renters2009,  CODELOC =="Jerusalem")
RenterAshdod2009    <<- dplyr::filter( renters2009,  CODELOC =="Ashdod")
RenterHaifa2009     <<- dplyr::filter( renters2009,  CODELOC =="Haifa")
RenterHerzliyya2009 <<- dplyr::filter( renters2009,  CODELOC =="Herzliyya")
RenterHolon2009    <<- dplyr::filter( renters2009,  CODELOC =="Holon")
RenterLod2009      <<- dplyr::filter( renters2009,  CODELOC =="Lod")
RenterHadera2009   <<- dplyr::filter( renters2009,  CODELOC =="Hadera")
RenterAshqelon2009 <<- dplyr::filter( renters2009,  CODELOC =="Ashkelon")
RenterNettanya2009 <<- dplyr::filter( renters2009,  CODELOC =="Netanya")
RenterRehovot2009  <<- dplyr::filter( renters2009,  CODELOC =="Rehovot")
RenterRamla2009    <<- dplyr::filter( renters2009,  CODELOC =="Ramla")
RenterRaannana2009 <<- dplyr::filter( renters2009,  CODELOC =="Raannana")
Rentertlv2009      <<- dplyr::filter( renters2009,  CODELOC =="Tel Aviv-Yaffo")
Renterbene2009     <<- dplyr::filter( renters2009,  CODELOC =="Bene Beraq")
Renterbat2009      <<- dplyr::filter( renters2009,  CODELOC =="Bat Yam")
Renterkefar2009    <<- dplyr::filter( renters2009,  CODELOC =="Kefar Sava")
Renterpetah2009    <<- dplyr::filter( renters2009,  CODELOC =="Petah Tiqwa")
Renterrishon2009   <<- dplyr::filter( renters2009,  CODELOC =="Rishon LeZiyon")
Renterramat2009    <<- dplyr::filter( renters2009,  CODELOC =="Ramat Gan")
Renterbeer2009     <<- dplyr::filter( renters2009,  CODELOC =="Beer Sheva")
Rentermodiin2009         <<- dplyr::filter( renters2009,  CODELOC =="Modi'in")
Renterbetshemesh2009     <<- dplyr::filter( renters2009,  CODELOC =="Bet Shemesh")
Renternahariyya2009      <<- dplyr::filter( renters2009,  CODELOC =="Nahariyya")
Renterqiryatatta2009     <<- dplyr::filter( renters2009,  CODELOC =="Qiryat Atta")
Rentergivatayim2009      <<- dplyr::filter( renters2009,  CODELOC =="Giv'atayim")

Renterjerusalem2010 <<- dplyr::filter( renters2010,  CODELOC =="Jerusalem")
RenterAshdod2010    <<- dplyr::filter( renters2010,  CODELOC =="Ashdod")
RenterHaifa2010     <<- dplyr::filter( renters2010,  CODELOC =="Haifa")
RenterHerzliyya2010 <<- dplyr::filter( renters2010,  CODELOC =="Herzliyya")
RenterHolon2010    <<- dplyr::filter( renters2010,  CODELOC =="Holon")
RenterLod2010      <<- dplyr::filter( renters2010,  CODELOC =="Lod")
RenterHadera2010   <<- dplyr::filter( renters2010,  CODELOC =="Hadera")
RenterAshqelon2010 <<- dplyr::filter( renters2010,  CODELOC =="Ashkelon")
RenterNettanya2010 <<- dplyr::filter( renters2010,  CODELOC =="Netanya")
RenterRehovot2010  <<- dplyr::filter( renters2010,  CODELOC =="Rehovot")
RenterRamla2010    <<- dplyr::filter( renters2010,  CODELOC =="Ramla")
RenterRaannana2010 <<- dplyr::filter( renters2010,  CODELOC =="Raannana")
Rentertlv2010      <<- dplyr::filter( renters2010,  CODELOC =="Tel Aviv-Yaffo")
Renterbene2010     <<- dplyr::filter( renters2010,  CODELOC =="Bene Beraq")
Renterbat2010      <<- dplyr::filter( renters2010,  CODELOC =="Bat Yam")
Renterkefar2010    <<- dplyr::filter( renters2010,  CODELOC =="Kefar Sava")
Renterpetah2010    <<- dplyr::filter( renters2010,  CODELOC =="Petah Tiqwa")
Renterrishon2010   <<- dplyr::filter( renters2010,  CODELOC =="Rishon LeZiyon")
Renterramat2010    <<- dplyr::filter( renters2010,  CODELOC =="Ramat Gan")
Renterbeer2010     <<- dplyr::filter( renters2010,  CODELOC =="Beer Sheva")
Rentermodiin2010         <<- dplyr::filter( renters2010,  CODELOC =="Modi'in")
Renterbetshemesh2010     <<- dplyr::filter( renters2010,  CODELOC =="Bet Shemesh")
Renternahariyya2010      <<- dplyr::filter( renters2010,  CODELOC =="Nahariyya")
Renterqiryatatta2010     <<- dplyr::filter( renters2010,  CODELOC =="Qiryat Atta")
Rentergivatayim2010      <<- dplyr::filter( renters2010,  CODELOC =="Giv'atayim")

Renterjerusalem2011 <<- dplyr::filter( renters2011,  CODELOC =="Jerusalem")
RenterAshdod2011    <<- dplyr::filter( renters2011,  CODELOC =="Ashdod")
RenterHaifa2011     <<- dplyr::filter( renters2011,  CODELOC =="Haifa")
RenterHerzliyya2011 <<- dplyr::filter( renters2011,  CODELOC =="Herzliyya")
RenterHolon2011    <<- dplyr::filter( renters2011,  CODELOC =="Holon")
RenterLod2011      <<- dplyr::filter( renters2011,  CODELOC =="Lod")
RenterHadera2011   <<- dplyr::filter( renters2011,  CODELOC =="Hadera")
RenterAshqelon2011 <<- dplyr::filter( renters2011,  CODELOC =="Ashkelon")
RenterNettanya2011 <<- dplyr::filter( renters2011,  CODELOC =="Netanya")
RenterRehovot2011  <<- dplyr::filter( renters2011,  CODELOC =="Rehovot")
RenterRamla2011    <<- dplyr::filter( renters2011,  CODELOC =="Ramla")
RenterRaannana2011 <<- dplyr::filter( renters2011,  CODELOC =="Raannana")
Rentertlv2011      <<- dplyr::filter( renters2011,  CODELOC =="Tel Aviv-Yaffo")
Renterbene2011     <<- dplyr::filter( renters2011,  CODELOC =="Bene Beraq")
Renterbat2011      <<- dplyr::filter( renters2011,  CODELOC =="Bat Yam")
Renterkefar2011    <<- dplyr::filter( renters2011,  CODELOC =="Kefar Sava")
Renterpetah2011    <<- dplyr::filter( renters2011,  CODELOC =="Petah Tiqwa")
Renterrishon2011   <<- dplyr::filter( renters2011,  CODELOC =="Rishon LeZiyon")
Renterramat2011    <<- dplyr::filter( renters2011,  CODELOC =="Ramat Gan")
Renterbeer2011     <<- dplyr::filter( renters2011,  CODELOC =="Beer Sheva")
Rentermodiin2011         <<- dplyr::filter( renters2011,  CODELOC =="Modi'in")
Renterbetshemesh2011     <<- dplyr::filter( renters2011,  CODELOC =="Bet Shemesh")
Renternahariyya2011      <<- dplyr::filter( renters2011,  CODELOC =="Nahariyya")
Renterqiryatatta2011     <<- dplyr::filter( renters2011,  CODELOC =="Qiryat Atta")
Rentergivatayim2011      <<- dplyr::filter( renters2011,  CODELOC =="Giv'atayim")


Renterjerusalem2012 <<- dplyr::filter( renters2012,  CODELOC =="Jerusalem")
RenterAshdod2012    <<- dplyr::filter( renters2012,  CODELOC =="Ashdod")
RenterHaifa2012     <<- dplyr::filter( renters2012,  CODELOC =="Haifa")
RenterHerzliyya2012 <<- dplyr::filter( renters2012,  CODELOC =="Herzliyya")
RenterHolon2012    <<- dplyr::filter( renters2012,  CODELOC =="Holon")
RenterLod2012      <<- dplyr::filter( renters2012,  CODELOC =="Lod")
RenterHadera2012   <<- dplyr::filter( renters2012,  CODELOC =="Hadera")
RenterAshqelon2012 <<- dplyr::filter( renters2012,  CODELOC =="Ashkelon")
RenterNettanya2012 <<- dplyr::filter( renters2012,  CODELOC =="Netanya")
RenterRehovot2012  <<- dplyr::filter( renters2012,  CODELOC =="Rehovot")
RenterRamla2012    <<- dplyr::filter( renters2012,  CODELOC =="Ramla")
RenterRaannana2012 <<- dplyr::filter( renters2012,  CODELOC =="Raannana")
Rentertlv2012      <<- dplyr::filter( renters2012,  CODELOC =="Tel Aviv-Yaffo")
Renterbene2012     <<- dplyr::filter( renters2012,  CODELOC =="Bene Beraq")
Renterbat2012      <<- dplyr::filter( renters2012,  CODELOC =="Bat Yam")
Renterkefar2012    <<- dplyr::filter( renters2012,  CODELOC =="Kefar Sava")
Renterpetah2012    <<- dplyr::filter( renters2012,  CODELOC =="Petah Tiqwa")
Renterrishon2012   <<- dplyr::filter( renters2012,  CODELOC =="Rishon LeZiyon")
Renterramat2012    <<- dplyr::filter( renters2012,  CODELOC =="Ramat Gan")
Renterbeer2012     <<- dplyr::filter( renters2012,  CODELOC =="Beer Sheva")
Rentermodiin2012         <<- dplyr::filter( renters2012,  CODELOC =="Modi'in")
Renterbetshemesh2012     <<- dplyr::filter( renters2012,  CODELOC =="Bet Shemesh")
Renternahariyya2012      <<- dplyr::filter( renters2012,  CODELOC =="Nahariyya")
Renterqiryatatta2012     <<- dplyr::filter( renters2012,  CODELOC =="Qiryat Atta")
Rentergivatayim2012      <<- dplyr::filter( renters2012,  CODELOC =="Giv'atayim")

Renterjerusalem2013 <<- dplyr::filter( renters2013,  CODELOC =="Jerusalem")
RenterAshdod2013    <<- dplyr::filter( renters2013, CODELOC =="Ashdod")
RenterHaifa2013     <<- dplyr::filter( renters2013, CODELOC =="Haifa")
RenterHerzliyya2013 <<- dplyr::filter( renters2013, CODELOC =="Herzliyya")
RenterHolon2013    <<- dplyr::filter( renters2013, CODELOC =="Holon")
RenterLod2013      <<- dplyr::filter( renters2013, CODELOC =="Lod")
RenterHadera2013   <<- dplyr::filter( renters2013, CODELOC =="Hadera")
RenterAshqelon2013 <<- dplyr::filter( renters2013, CODELOC =="Ashkelon")
RenterNettanya2013 <<- dplyr::filter( renters2013, CODELOC =="Netanya")
RenterRehovot2013  <<- dplyr::filter( renters2013, CODELOC =="Rehovot")
RenterRamla2013    <<- dplyr::filter( renters2013, CODELOC =="Ramla")
RenterRaannana2013 <<- dplyr::filter( renters2013, CODELOC =="Raannana")
Rentertlv2013      <<- dplyr::filter( renters2013, CODELOC =="Tel Aviv-Yaffo")
Renterbene2013     <<- dplyr::filter( renters2013, CODELOC =="Bene Beraq")
Renterbat2013      <<- dplyr::filter( renters2013, CODELOC =="Bat Yam")
Renterkefar2013    <<- dplyr::filter( renters2013, CODELOC =="Kefar Sava")
Renterpetah2013    <<- dplyr::filter( renters2013, CODELOC =="Petah Tiqwa")
Renterrishon2013   <<- dplyr::filter( renters2013, CODELOC =="Rishon LeZiyon")
Renterramat2013    <<- dplyr::filter( renters2013, CODELOC =="Ramat Gan")
Renterbeer2013     <<- dplyr::filter( renters2013, CODELOC =="Beer Sheva")
Rentermodiin2013         <<- dplyr::filter( renters2013, CODELOC =="Modi'in")
Renterbetshemesh2013     <<- dplyr::filter( renters2013, CODELOC =="Bet Shemesh")
Renternahariyya2013      <<- dplyr::filter( renters2013, CODELOC =="Nahariyya")
Renterqiryatatta2013     <<- dplyr::filter( renters2013, CODELOC =="Qiryat Atta")
Rentergivatayim2013      <<- dplyr::filter( renters2013, CODELOC =="Giv'atayim")

Renterjerusalem2014 <<- dplyr::filter( renters2014,  CODELOC =="Jerusalem")
RenterAshdod2014    <<- dplyr::filter( renters2014,  CODELOC =="Ashdod")
RenterHaifa2014     <<- dplyr::filter( renters2014,  CODELOC =="Haifa")
RenterHerzliyya2014 <<- dplyr::filter( renters2014,  CODELOC =="Herzliyya")
RenterHolon2014    <<- dplyr::filter( renters2014,  CODELOC =="Holon")
RenterLod2014      <<- dplyr::filter( renters2014,  CODELOC =="Lod")
RenterHadera2014   <<- dplyr::filter( renters2014,  CODELOC =="Hadera")
RenterAshqelon2014 <<- dplyr::filter( renters2014,  CODELOC =="Ashkelon")
RenterNettanya2014 <<- dplyr::filter( renters2014,  CODELOC =="Netanya")
RenterRehovot2014  <<- dplyr::filter( renters2014,  CODELOC =="Rehovot")
RenterRamla2014    <<- dplyr::filter( renters2014,  CODELOC =="Ramla")
RenterRaannana2014 <<- dplyr::filter( renters2014,  CODELOC =="Raannana")
Rentertlv2014      <<- dplyr::filter( renters2014,  CODELOC =="Tel Aviv-Yaffo")
Renterbene2014     <<- dplyr::filter( renters2014,  CODELOC =="Bene Beraq")
Renterbat2014      <<- dplyr::filter( renters2014,  CODELOC =="Bat Yam")
Renterkefar2014    <<- dplyr::filter( renters2014,  CODELOC =="Kefar Sava")
Renterpetah2014    <<- dplyr::filter( renters2014,  CODELOC =="Petah Tiqwa")
Renterrishon2014   <<- dplyr::filter( renters2014,  CODELOC =="Rishon LeZiyon")
Renterramat2014    <<- dplyr::filter( renters2014,  CODELOC =="Ramat Gan")
Renterbeer2014     <<- dplyr::filter( renters2014,  CODELOC =="Beer Sheva")
Rentermodiin2014         <<- dplyr::filter( renters2014,  CODELOC =="Modi'in")
Renterbetshemesh2014     <<- dplyr::filter( renters2014,  CODELOC =="Bet Shemesh")
Renternahariyya2014      <<- dplyr::filter( renters2014,  CODELOC =="Nahariyya")
Renterqiryatatta2014     <<- dplyr::filter( renters2014,  CODELOC =="Qiryat Ata")
Rentergivatayim2014      <<- dplyr::filter( renters2014,  CODELOC =="Giv'atayim")

########### count owners by city #####################


     ownerCity2004    <<- c(
Ownerjerusalem2004     %>%  nrow,
OwnerAshdod2004        %>%  nrow,
OwnerHaifa2004         %>%  nrow,
OwnerHerzliyya2004     %>%  nrow,
OwnerHolon2004         %>%  nrow,
OwnerLod2004           %>%  nrow,
OwnerHadera2004        %>%  nrow,
OwnerAshqelon2004      %>%  nrow,
OwnerNettanya2004      %>%  nrow,
OwnerRehovot2004       %>%  nrow,
OwnerRamla2004         %>%  nrow,
OwnerRaannana2004      %>%  nrow,
Ownertlv2004           %>%  nrow,
Ownerbene2004          %>%  nrow,
Ownerbat2004           %>%  nrow,
Ownerkefar2004         %>%  nrow,
Ownerpetah2004         %>%  nrow,
Ownerrishon2004        %>%  nrow,
Ownerramat2004         %>%  nrow,
Ownerbeer2004          %>%  nrow,
Ownermodiin2004        %>%  nrow,
Ownerbetshemesh2004    %>%  nrow,
Ownernahariyya2004     %>%  nrow,
Ownerqiryatatta2004    %>%  nrow,
Ownergivatayim2004     %>%  nrow)


     ownerCity2005    <<- c(
Ownerjerusalem2005     %>%  nrow,
OwnerAshdod2005        %>%  nrow,
OwnerHaifa2005         %>%  nrow,
OwnerHerzliyya2005     %>%  nrow,
OwnerHolon2005         %>%  nrow,
OwnerLod2005           %>%  nrow,
OwnerHadera2005        %>%  nrow,
OwnerAshqelon2005      %>%  nrow,
OwnerNettanya2005      %>%  nrow,
OwnerRehovot2005       %>%  nrow,
OwnerRamla2005         %>%  nrow,
OwnerRaannana2005      %>%  nrow,
Ownertlv2005           %>%  nrow,
Ownerbene2005          %>%  nrow,
Ownerbat2005           %>%  nrow,
Ownerkefar2005         %>%  nrow,
Ownerpetah2005         %>%  nrow,
Ownerrishon2005        %>%  nrow,
Ownerramat2005         %>%  nrow,
Ownerbeer2005          %>%  nrow,
Ownermodiin2005        %>%  nrow,
Ownerbetshemesh2005    %>%  nrow,
Ownernahariyya2005     %>%  nrow,
Ownerqiryatatta2005    %>%  nrow,
Ownergivatayim2005     %>%  nrow)


     ownerCity2006    <<- c(
Ownerjerusalem2006     %>%  nrow,
OwnerAshdod2006        %>%  nrow,
OwnerHaifa2006         %>%  nrow,
OwnerHerzliyya2006     %>%  nrow,
OwnerHolon2006         %>%  nrow,
OwnerLod2006           %>%  nrow,
OwnerHadera2006        %>%  nrow,
OwnerAshqelon2006      %>%  nrow,
OwnerNettanya2006      %>%  nrow,
OwnerRehovot2006       %>%  nrow,
OwnerRamla2006         %>%  nrow,
OwnerRaannana2006      %>%  nrow,
Ownertlv2006           %>%  nrow,
Ownerbene2006          %>%  nrow,
Ownerbat2006           %>%  nrow,
Ownerkefar2006         %>%  nrow,
Ownerpetah2006         %>%  nrow,
Ownerrishon2006        %>%  nrow,
Ownerramat2006         %>%  nrow,
Ownerbeer2006          %>%  nrow,
Ownermodiin2006        %>%  nrow,
Ownerbetshemesh2006    %>%  nrow,
Ownernahariyya2006     %>%  nrow,
Ownerqiryatatta2006    %>%  nrow,
Ownergivatayim2006     %>%  nrow)


     ownerCity2007    <<- c(
Ownerjerusalem2007     %>%  nrow,
OwnerAshdod2007        %>%  nrow,
OwnerHaifa2007         %>%  nrow,
OwnerHerzliyya2007     %>%  nrow,
OwnerHolon2007         %>%  nrow,
OwnerLod2007           %>%  nrow,
OwnerHadera2007        %>%  nrow,
OwnerAshqelon2007      %>%  nrow,
OwnerNettanya2007      %>%  nrow,
OwnerRehovot2007       %>%  nrow,
OwnerRamla2007         %>%  nrow,
OwnerRaannana2007      %>%  nrow,
Ownertlv2007           %>%  nrow,
Ownerbene2007          %>%  nrow,
Ownerbat2007           %>%  nrow,
Ownerkefar2007         %>%  nrow,
Ownerpetah2007         %>%  nrow,
Ownerrishon2007        %>%  nrow,
Ownerramat2007         %>%  nrow,
Ownerbeer2007          %>%  nrow,
Ownermodiin2007        %>%  nrow,
Ownerbetshemesh2007    %>%  nrow,
Ownernahariyya2007     %>%  nrow,
Ownerqiryatatta2007    %>%  nrow,
Ownergivatayim2007     %>%  nrow)


     ownerCity2008    <<- c(
Ownerjerusalem2008     %>%  nrow,
OwnerAshdod2008        %>%  nrow,
OwnerHaifa2008         %>%  nrow,
OwnerHerzliyya2008     %>%  nrow,
OwnerHolon2008         %>%  nrow,
OwnerLod2008           %>%  nrow,
OwnerHadera2008        %>%  nrow,
OwnerAshqelon2008      %>%  nrow,
OwnerNettanya2008      %>%  nrow,
OwnerRehovot2008       %>%  nrow,
OwnerRamla2008         %>%  nrow,
OwnerRaannana2008      %>%  nrow,
Ownertlv2008           %>%  nrow,
Ownerbene2008          %>%  nrow,
Ownerbat2008           %>%  nrow,
Ownerkefar2008         %>%  nrow,
Ownerpetah2008         %>%  nrow,
Ownerrishon2008        %>%  nrow,
Ownerramat2008         %>%  nrow,
Ownerbeer2008          %>%  nrow,
Ownermodiin2008        %>%  nrow,
Ownerbetshemesh2008    %>%  nrow,
Ownernahariyya2008     %>%  nrow,
Ownerqiryatatta2008    %>%  nrow,
Ownergivatayim2008     %>%  nrow)


     ownerCity2009    <<- c(
Ownerjerusalem2009     %>%  nrow,
OwnerAshdod2009        %>%  nrow,
OwnerHaifa2009         %>%  nrow,
OwnerHerzliyya2009     %>%  nrow,
OwnerHolon2009         %>%  nrow,
OwnerLod2009           %>%  nrow,
OwnerHadera2009        %>%  nrow,
OwnerAshqelon2009      %>%  nrow,
OwnerNettanya2009      %>%  nrow,
OwnerRehovot2009       %>%  nrow,
OwnerRamla2009         %>%  nrow,
OwnerRaannana2009      %>%  nrow,
Ownertlv2009           %>%  nrow,
Ownerbene2009          %>%  nrow,
Ownerbat2009           %>%  nrow,
Ownerkefar2009         %>%  nrow,
Ownerpetah2009         %>%  nrow,
Ownerrishon2009        %>%  nrow,
Ownerramat2009         %>%  nrow,
Ownerbeer2009          %>%  nrow,
Ownermodiin2009        %>%  nrow,
Ownerbetshemesh2009    %>%  nrow,
Ownernahariyya2009     %>%  nrow,
Ownerqiryatatta2009    %>%  nrow,
Ownergivatayim2009     %>%  nrow)


     ownerCity2010    <<- c(
Ownerjerusalem2010     %>%  nrow,
OwnerAshdod2010        %>%  nrow,
OwnerHaifa2010         %>%  nrow,
OwnerHerzliyya2010     %>%  nrow,
OwnerHolon2010         %>%  nrow,
OwnerLod2010           %>%  nrow,
OwnerHadera2010        %>%  nrow,
OwnerAshqelon2010      %>%  nrow,
OwnerNettanya2010      %>%  nrow,
OwnerRehovot2010       %>%  nrow,
OwnerRamla2010         %>%  nrow,
OwnerRaannana2010      %>%  nrow,
Ownertlv2010           %>%  nrow,
Ownerbene2010          %>%  nrow,
Ownerbat2010           %>%  nrow,
Ownerkefar2010         %>%  nrow,
Ownerpetah2010         %>%  nrow,
Ownerrishon2010        %>%  nrow,
Ownerramat2010         %>%  nrow,
Ownerbeer2010          %>%  nrow,
Ownermodiin2010        %>%  nrow,
Ownerbetshemesh2010    %>%  nrow,
Ownernahariyya2010     %>%  nrow,
Ownerqiryatatta2010    %>%  nrow,
Ownergivatayim2010     %>%  nrow)


     ownerCity2011    <<- c(
Ownerjerusalem2011     %>%  nrow,
OwnerAshdod2011        %>%  nrow,
OwnerHaifa2011         %>%  nrow,
OwnerHerzliyya2011     %>%  nrow,
OwnerHolon2011         %>%  nrow,
OwnerLod2011           %>%  nrow,
OwnerHadera2011        %>%  nrow,
OwnerAshqelon2011      %>%  nrow,
OwnerNettanya2011      %>%  nrow,
OwnerRehovot2011       %>%  nrow,
OwnerRamla2011         %>%  nrow,
OwnerRaannana2011      %>%  nrow,
Ownertlv2011           %>%  nrow,
Ownerbene2011          %>%  nrow,
Ownerbat2011           %>%  nrow,
Ownerkefar2011         %>%  nrow,
Ownerpetah2011         %>%  nrow,
Ownerrishon2011        %>%  nrow,
Ownerramat2011         %>%  nrow,
Ownerbeer2011          %>%  nrow,
Ownermodiin2011        %>%  nrow,
Ownerbetshemesh2011    %>%  nrow,
Ownernahariyya2011     %>%  nrow,
Ownerqiryatatta2011    %>%  nrow,
Ownergivatayim2011     %>%  nrow)


     ownerCity2012    <<- c(
Ownerjerusalem2012     %>%  nrow,
OwnerAshdod2012        %>%  nrow,
OwnerHaifa2012         %>%  nrow,
OwnerHerzliyya2012     %>%  nrow,
OwnerHolon2012         %>%  nrow,
OwnerLod2012           %>%  nrow,
OwnerHadera2012        %>%  nrow,
OwnerAshqelon2012      %>%  nrow,
OwnerNettanya2012      %>%  nrow,
OwnerRehovot2012       %>%  nrow,
OwnerRamla2012         %>%  nrow,
OwnerRaannana2012      %>%  nrow,
Ownertlv2012           %>%  nrow,
Ownerbene2012          %>%  nrow,
Ownerbat2012           %>%  nrow,
Ownerkefar2012         %>%  nrow,
Ownerpetah2012         %>%  nrow,
Ownerrishon2012        %>%  nrow,
Ownerramat2012         %>%  nrow,
Ownerbeer2012          %>%  nrow,
Ownermodiin2012        %>%  nrow,
Ownerbetshemesh2012    %>%  nrow,
Ownernahariyya2012     %>%  nrow,
Ownerqiryatatta2012    %>%  nrow,
Ownergivatayim2012     %>%  nrow)


     ownerCity2013    <<- c(
Ownerjerusalem2013     %>%  nrow,
OwnerAshdod2013        %>%  nrow,
OwnerHaifa2013         %>%  nrow,
OwnerHerzliyya2013     %>%  nrow,
OwnerHolon2013         %>%  nrow,
OwnerLod2013           %>%  nrow,
OwnerHadera2013        %>%  nrow,
OwnerAshqelon2013      %>%  nrow,
OwnerNettanya2013      %>%  nrow,
OwnerRehovot2013       %>%  nrow,
OwnerRamla2013         %>%  nrow,
OwnerRaannana2013      %>%  nrow,
Ownertlv2013           %>%  nrow,
Ownerbene2013          %>%  nrow,
Ownerbat2013           %>%  nrow,
Ownerkefar2013         %>%  nrow,
Ownerpetah2013         %>%  nrow,
Ownerrishon2013        %>%  nrow,
Ownerramat2013         %>%  nrow,
Ownerbeer2013          %>%  nrow,
Ownermodiin2013        %>%  nrow,
Ownerbetshemesh2013    %>%  nrow,
Ownernahariyya2013     %>%  nrow,
Ownerqiryatatta2013    %>%  nrow,
Ownergivatayim2013     %>%  nrow)


     ownerCity2014    <<- c(
Ownerjerusalem2014     %>%  nrow,
OwnerAshdod2014        %>%  nrow,
OwnerHaifa2014         %>%  nrow,
OwnerHerzliyya2014     %>%  nrow,
OwnerHolon2014         %>%  nrow,
OwnerLod2014           %>%  nrow,
OwnerHadera2014        %>%  nrow,
OwnerAshqelon2014      %>%  nrow,
OwnerNettanya2014      %>%  nrow,
OwnerRehovot2014       %>%  nrow,
OwnerRamla2014         %>%  nrow,
OwnerRaannana2014      %>%  nrow,
Ownertlv2014           %>%  nrow,
Ownerbene2014          %>%  nrow,
Ownerbat2014           %>%  nrow,
Ownerkefar2014         %>%  nrow,
Ownerpetah2014         %>%  nrow,
Ownerrishon2014        %>%  nrow,
Ownerramat2014         %>%  nrow,
Ownerbeer2014          %>%  nrow,
Ownermodiin2014        %>%  nrow,
Ownerbetshemesh2014    %>%  nrow,
Ownernahariyya2014     %>%  nrow,
Ownerqiryatatta2014    %>%  nrow,
Ownergivatayim2014     %>%  nrow)


############   renter ####################

     renterCity2004    <<- c(
Renterjerusalem2004     %>%  nrow,
RenterAshdod2004        %>%  nrow,
RenterHaifa2004         %>%  nrow,
RenterHerzliyya2004     %>%  nrow,
RenterHolon2004         %>%  nrow,
RenterLod2004           %>%  nrow,
RenterHadera2004        %>%  nrow,
RenterAshqelon2004      %>%  nrow,
RenterNettanya2004      %>%  nrow,
RenterRehovot2004       %>%  nrow,
RenterRamla2004         %>%  nrow,
RenterRaannana2004      %>%  nrow,
Rentertlv2004           %>%  nrow,
Renterbene2004          %>%  nrow,
Renterbat2004           %>%  nrow,
Renterkefar2004         %>%  nrow,
Renterpetah2004         %>%  nrow,
Renterrishon2004        %>%  nrow,
Renterramat2004         %>%  nrow,
Renterbeer2004          %>%  nrow,
Rentermodiin2004        %>%  nrow,
Renterbetshemesh2004    %>%  nrow,
Renternahariyya2004     %>%  nrow,
Renterqiryatatta2004    %>%  nrow,
Rentergivatayim2004     %>%  nrow)


     renterCity2005    <<- c(
Renterjerusalem2005     %>%  nrow,
RenterAshdod2005        %>%  nrow,
RenterHaifa2005         %>%  nrow,
RenterHerzliyya2005     %>%  nrow,
RenterHolon2005         %>%  nrow,
RenterLod2005           %>%  nrow,
RenterHadera2005        %>%  nrow,
RenterAshqelon2005      %>%  nrow,
RenterNettanya2005      %>%  nrow,
RenterRehovot2005       %>%  nrow,
RenterRamla2005         %>%  nrow,
RenterRaannana2005      %>%  nrow,
Rentertlv2005           %>%  nrow,
Renterbene2005          %>%  nrow,
Renterbat2005           %>%  nrow,
Renterkefar2005         %>%  nrow,
Renterpetah2005         %>%  nrow,
Renterrishon2005        %>%  nrow,
Renterramat2005         %>%  nrow,
Renterbeer2005          %>%  nrow,
Rentermodiin2005        %>%  nrow,
Renterbetshemesh2005    %>%  nrow,
Renternahariyya2005     %>%  nrow,
Renterqiryatatta2005    %>%  nrow,
Rentergivatayim2005     %>%  nrow)


     renterCity2006    <<- c(
Renterjerusalem2006     %>%  nrow,
RenterAshdod2006        %>%  nrow,
RenterHaifa2006         %>%  nrow,
RenterHerzliyya2006     %>%  nrow,
RenterHolon2006         %>%  nrow,
RenterLod2006           %>%  nrow,
RenterHadera2006        %>%  nrow,
RenterAshqelon2006      %>%  nrow,
RenterNettanya2006      %>%  nrow,
RenterRehovot2006       %>%  nrow,
RenterRamla2006         %>%  nrow,
RenterRaannana2006      %>%  nrow,
Rentertlv2006           %>%  nrow,
Renterbene2006          %>%  nrow,
Renterbat2006           %>%  nrow,
Renterkefar2006         %>%  nrow,
Renterpetah2006         %>%  nrow,
Renterrishon2006        %>%  nrow,
Renterramat2006         %>%  nrow,
Renterbeer2006          %>%  nrow,
Rentermodiin2006        %>%  nrow,
Renterbetshemesh2006    %>%  nrow,
Renternahariyya2006     %>%  nrow,
Renterqiryatatta2006    %>%  nrow,
Rentergivatayim2006     %>%  nrow)


     renterCity2007    <<- c(
Renterjerusalem2007     %>%  nrow,
RenterAshdod2007        %>%  nrow,
RenterHaifa2007         %>%  nrow,
RenterHerzliyya2007     %>%  nrow,
RenterHolon2007         %>%  nrow,
RenterLod2007           %>%  nrow,
RenterHadera2007        %>%  nrow,
RenterAshqelon2007      %>%  nrow,
RenterNettanya2007      %>%  nrow,
RenterRehovot2007       %>%  nrow,
RenterRamla2007         %>%  nrow,
RenterRaannana2007      %>%  nrow,
Rentertlv2007           %>%  nrow,
Renterbene2007          %>%  nrow,
Renterbat2007           %>%  nrow,
Renterkefar2007         %>%  nrow,
Renterpetah2007         %>%  nrow,
Renterrishon2007        %>%  nrow,
Renterramat2007         %>%  nrow,
Renterbeer2007          %>%  nrow,
Rentermodiin2007        %>%  nrow,
Renterbetshemesh2007    %>%  nrow,
Renternahariyya2007     %>%  nrow,
Renterqiryatatta2007    %>%  nrow,
Rentergivatayim2007     %>%  nrow)


     renterCity2008    <<- c(
Renterjerusalem2008     %>%  nrow,
RenterAshdod2008        %>%  nrow,
RenterHaifa2008         %>%  nrow,
RenterHerzliyya2008     %>%  nrow,
RenterHolon2008         %>%  nrow,
RenterLod2008           %>%  nrow,
RenterHadera2008        %>%  nrow,
RenterAshqelon2008      %>%  nrow,
RenterNettanya2008      %>%  nrow,
RenterRehovot2008       %>%  nrow,
RenterRamla2008         %>%  nrow,
RenterRaannana2008      %>%  nrow,
Rentertlv2008           %>%  nrow,
Renterbene2008          %>%  nrow,
Renterbat2008           %>%  nrow,
Renterkefar2008         %>%  nrow,
Renterpetah2008         %>%  nrow,
Renterrishon2008        %>%  nrow,
Renterramat2008         %>%  nrow,
Renterbeer2008          %>%  nrow,
Rentermodiin2008        %>%  nrow,
Renterbetshemesh2008    %>%  nrow,
Renternahariyya2008     %>%  nrow,
Renterqiryatatta2008    %>%  nrow,
Rentergivatayim2008     %>%  nrow)


     renterCity2009    <<- c(
Renterjerusalem2009     %>%  nrow,
RenterAshdod2009        %>%  nrow,
RenterHaifa2009         %>%  nrow,
RenterHerzliyya2009     %>%  nrow,
RenterHolon2009         %>%  nrow,
RenterLod2009           %>%  nrow,
RenterHadera2009        %>%  nrow,
RenterAshqelon2009      %>%  nrow,
RenterNettanya2009      %>%  nrow,
RenterRehovot2009       %>%  nrow,
RenterRamla2009         %>%  nrow,
RenterRaannana2009      %>%  nrow,
Rentertlv2009           %>%  nrow,
Renterbene2009          %>%  nrow,
Renterbat2009           %>%  nrow,
Renterkefar2009         %>%  nrow,
Renterpetah2009         %>%  nrow,
Renterrishon2009        %>%  nrow,
Renterramat2009         %>%  nrow,
Renterbeer2009          %>%  nrow,
Rentermodiin2009        %>%  nrow,
Renterbetshemesh2009    %>%  nrow,
Renternahariyya2009     %>%  nrow,
Renterqiryatatta2009    %>%  nrow,
Rentergivatayim2009     %>%  nrow)


     renterCity2010    <<- c(
Renterjerusalem2010     %>%  nrow,
RenterAshdod2010        %>%  nrow,
RenterHaifa2010         %>%  nrow,
RenterHerzliyya2010     %>%  nrow,
RenterHolon2010         %>%  nrow,
RenterLod2010           %>%  nrow,
RenterHadera2010        %>%  nrow,
RenterAshqelon2010      %>%  nrow,
RenterNettanya2010      %>%  nrow,
RenterRehovot2010       %>%  nrow,
RenterRamla2010         %>%  nrow,
RenterRaannana2010      %>%  nrow,
Rentertlv2010           %>%  nrow,
Renterbene2010          %>%  nrow,
Renterbat2010           %>%  nrow,
Renterkefar2010         %>%  nrow,
Renterpetah2010         %>%  nrow,
Renterrishon2010        %>%  nrow,
Renterramat2010         %>%  nrow,
Renterbeer2010          %>%  nrow,
Rentermodiin2010        %>%  nrow,
Renterbetshemesh2010    %>%  nrow,
Renternahariyya2010     %>%  nrow,
Renterqiryatatta2010    %>%  nrow,
Rentergivatayim2010     %>%  nrow)


     renterCity2011    <<- c(
Renterjerusalem2011     %>%  nrow,
RenterAshdod2011        %>%  nrow,
RenterHaifa2011         %>%  nrow,
RenterHerzliyya2011     %>%  nrow,
RenterHolon2011         %>%  nrow,
RenterLod2011           %>%  nrow,
RenterHadera2011        %>%  nrow,
RenterAshqelon2011      %>%  nrow,
RenterNettanya2011      %>%  nrow,
RenterRehovot2011       %>%  nrow,
RenterRamla2011         %>%  nrow,
RenterRaannana2011      %>%  nrow,
Rentertlv2011           %>%  nrow,
Renterbene2011          %>%  nrow,
Renterbat2011           %>%  nrow,
Renterkefar2011         %>%  nrow,
Renterpetah2011         %>%  nrow,
Renterrishon2011        %>%  nrow,
Renterramat2011         %>%  nrow,
Renterbeer2011          %>%  nrow,
Rentermodiin2011        %>%  nrow,
Renterbetshemesh2011    %>%  nrow,
Renternahariyya2011     %>%  nrow,
Renterqiryatatta2011    %>%  nrow,
Rentergivatayim2011     %>%  nrow)


     renterCity2012    <<- c(
Renterjerusalem2012     %>%  nrow,
RenterAshdod2012        %>%  nrow,
RenterHaifa2012         %>%  nrow,
RenterHerzliyya2012     %>%  nrow,
RenterHolon2012         %>%  nrow,
RenterLod2012           %>%  nrow,
RenterHadera2012        %>%  nrow,
RenterAshqelon2012      %>%  nrow,
RenterNettanya2012      %>%  nrow,
RenterRehovot2012       %>%  nrow,
RenterRamla2012         %>%  nrow,
RenterRaannana2012      %>%  nrow,
Rentertlv2012           %>%  nrow,
Renterbene2012          %>%  nrow,
Renterbat2012           %>%  nrow,
Renterkefar2012         %>%  nrow,
Renterpetah2012         %>%  nrow,
Renterrishon2012        %>%  nrow,
Renterramat2012         %>%  nrow,
Renterbeer2012          %>%  nrow,
Rentermodiin2012        %>%  nrow,
Renterbetshemesh2012    %>%  nrow,
Renternahariyya2012     %>%  nrow,
Renterqiryatatta2012    %>%  nrow,
Rentergivatayim2012     %>%  nrow)


     renterCity2013    <<- c(
Renterjerusalem2013     %>%  nrow,
RenterAshdod2013        %>%  nrow,
RenterHaifa2013         %>%  nrow,
RenterHerzliyya2013     %>%  nrow,
RenterHolon2013         %>%  nrow,
RenterLod2013           %>%  nrow,
RenterHadera2013        %>%  nrow,
RenterAshqelon2013      %>%  nrow,
RenterNettanya2013      %>%  nrow,
RenterRehovot2013       %>%  nrow,
RenterRamla2013         %>%  nrow,
RenterRaannana2013      %>%  nrow,
Rentertlv2013           %>%  nrow,
Renterbene2013          %>%  nrow,
Renterbat2013           %>%  nrow,
Renterkefar2013         %>%  nrow,
Renterpetah2013         %>%  nrow,
Renterrishon2013        %>%  nrow,
Renterramat2013         %>%  nrow,
Renterbeer2013          %>%  nrow,
Rentermodiin2013        %>%  nrow,
Renterbetshemesh2013    %>%  nrow,
Renternahariyya2013     %>%  nrow,
Renterqiryatatta2013    %>%  nrow,
Rentergivatayim2013     %>%  nrow)


     renterCity2014    <<- c(
Renterjerusalem2014     %>%  nrow,
RenterAshdod2014        %>%  nrow,
RenterHaifa2014         %>%  nrow,
RenterHerzliyya2014     %>%  nrow,
RenterHolon2014         %>%  nrow,
RenterLod2014           %>%  nrow,
RenterHadera2014        %>%  nrow,
RenterAshqelon2014      %>%  nrow,
RenterNettanya2014      %>%  nrow,
RenterRehovot2014       %>%  nrow,
RenterRamla2014         %>%  nrow,
RenterRaannana2014      %>%  nrow,
Rentertlv2014           %>%  nrow,
Renterbene2014          %>%  nrow,
Renterbat2014           %>%  nrow,
Renterkefar2014         %>%  nrow,
Renterpetah2014         %>%  nrow,
Renterrishon2014        %>%  nrow,
Renterramat2014         %>%  nrow,
Renterbeer2014          %>%  nrow,
Rentermodiin2014        %>%  nrow,
Renterbetshemesh2014    %>%  nrow,
Renternahariyya2014     %>%  nrow,
Renterqiryatatta2014    %>%  nrow,
Rentergivatayim2014     %>%  nrow)


renterCityList  <<-as.data.frame(cbind(list   = cityOrder,
                                "2004"  = renterCity2004,
                                "2005"  = renterCity2005,
                                "2006"  = renterCity2006,
                                "2007"  = renterCity2007,
                                "2008"  = renterCity2008,
                                "2009"  = renterCity2009,
                                "2010"  = renterCity2010,
                                "2011"  = renterCity2011,
                                "2012"  = renterCity2012,
                                "2013"  = renterCity2013,
                                "2014"  = renterCity2014))

ownerCityList  <<-as.data.frame(cbind(list   = cityOrder,
                                "2004"  = ownerCity2004,
                                "2005"  = ownerCity2005,
                                "2006"  = ownerCity2006,
                                "2007"  = ownerCity2007,
                                "2008"  = ownerCity2008,
                                "2009"  = ownerCity2009,
                                "2010"  = ownerCity2010,
                                "2011"  = ownerCity2011,
                                "2012"  = ownerCity2012,
                                "2013"  = ownerCity2013,
                                "2014"  = ownerCity2014))



############   renter ### above median income      ###############

 richRenterjerusalem2004     <- Renterjerusalem2004     %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterAshdod2004        <- RenterAshdod2004        %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterHaifa2004         <- RenterHaifa2004         %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterHerzliyya2004     <- RenterHerzliyya2004     %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterHolon2004         <- RenterHolon2004         %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterLod2004           <- RenterLod2004           %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterHadera2004        <- RenterHadera2004        %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterAshqelon2004      <- RenterAshqelon2004      %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterNettanya2004      <- RenterNettanya2004      %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterRehovot2004       <- RenterRehovot2004       %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterRamla2004         <- RenterRamla2004         %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterRaannana2004      <- RenterRaannana2004      %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRentertlv2004           <- Rentertlv2004           %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterbene2004          <- Renterbene2004          %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterbat2004           <- Renterbat2004           %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterkefar2004         <- Renterkefar2004         %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterpetah2004         <- Renterpetah2004         %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterrishon2004        <- Renterrishon2004        %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterramat2004         <- Renterramat2004         %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterbeer2004          <- Renterbeer2004          %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRentermodiin2004        <- Rentermodiin2004        %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterbetshemesh2004    <- Renterbetshemesh2004    %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenternahariyya2004     <- Renternahariyya2004     %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterqiryatatta2004    <- Renterqiryatatta2004    %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRentergivatayim2004     <- Rentergivatayim2004     %>%  dplyr::filter( INCGROSS > medianInc2004)

 richRenterjerusalem2005     <- Renterjerusalem2005     %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterAshdod2005        <- RenterAshdod2005        %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterHaifa2005         <- RenterHaifa2005         %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterHerzliyya2005     <- RenterHerzliyya2005     %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterHolon2005         <- RenterHolon2005         %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterLod2005           <- RenterLod2005           %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterHadera2005        <- RenterHadera2005        %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterAshqelon2005      <- RenterAshqelon2005      %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterNettanya2005      <- RenterNettanya2005      %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterRehovot2005       <- RenterRehovot2005       %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterRamla2005         <- RenterRamla2005         %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterRaannana2005      <- RenterRaannana2005      %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRentertlv2005           <- Rentertlv2005           %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterbene2005          <- Renterbene2005          %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterbat2005           <- Renterbat2005           %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterkefar2005         <- Renterkefar2005         %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterpetah2005         <- Renterpetah2005         %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterrishon2005        <- Renterrishon2005        %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterramat2005         <- Renterramat2005         %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterbeer2005          <- Renterbeer2005          %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRentermodiin2005        <- Rentermodiin2005        %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterbetshemesh2005    <- Renterbetshemesh2005    %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenternahariyya2005     <- Renternahariyya2005     %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRenterqiryatatta2005    <- Renterqiryatatta2005    %>%  dplyr::filter( INCGROSS > medianInc2005)
 richRentergivatayim2005     <- Rentergivatayim2005     %>%  dplyr::filter( INCGROSS > medianInc2005)

 richRenterjerusalem2006     <- Renterjerusalem2006     %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterAshdod2006        <- RenterAshdod2006        %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterHaifa2006         <- RenterHaifa2006         %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterHerzliyya2006     <- RenterHerzliyya2006     %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterHolon2006         <- RenterHolon2006         %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterLod2006           <- RenterLod2006           %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterHadera2006        <- RenterHadera2006        %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterAshqelon2006      <- RenterAshqelon2006      %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterNettanya2006      <- RenterNettanya2006      %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterRehovot2006       <- RenterRehovot2006       %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterRamla2006         <- RenterRamla2006         %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterRaannana2006      <- RenterRaannana2006      %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRentertlv2006           <- Rentertlv2006           %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterbene2006          <- Renterbene2006          %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterbat2006           <- Renterbat2006           %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterkefar2006         <- Renterkefar2006         %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterpetah2006         <- Renterpetah2006         %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterrishon2006        <- Renterrishon2006        %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterramat2006         <- Renterramat2006         %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterbeer2006          <- Renterbeer2006          %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRentermodiin2006        <- Rentermodiin2006        %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterbetshemesh2006    <- Renterbetshemesh2006    %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenternahariyya2006     <- Renternahariyya2006     %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRenterqiryatatta2006    <- Renterqiryatatta2006    %>%  dplyr::filter( INCGROSS > medianInc2006)
 richRentergivatayim2006     <- Rentergivatayim2006     %>%  dplyr::filter( INCGROSS > medianInc2006)

 richRenterjerusalem2007     <- Renterjerusalem2007     %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterAshdod2007        <- RenterAshdod2007        %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterHaifa2007         <- RenterHaifa2007         %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterHerzliyya2007     <- RenterHerzliyya2007     %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterHolon2007         <- RenterHolon2007         %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterLod2007           <- RenterLod2007           %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterHadera2007        <- RenterHadera2007        %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterAshqelon2007      <- RenterAshqelon2007      %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterNettanya2007      <- RenterNettanya2007      %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterRehovot2007       <- RenterRehovot2007       %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterRamla2007         <- RenterRamla2007         %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterRaannana2007      <- RenterRaannana2007      %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRentertlv2007           <- Rentertlv2007           %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterbene2007          <- Renterbene2007          %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterbat2007           <- Renterbat2007           %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterkefar2007         <- Renterkefar2007         %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterpetah2007         <- Renterpetah2007         %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterrishon2007        <- Renterrishon2007        %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterramat2007         <- Renterramat2007         %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterbeer2007          <- Renterbeer2007          %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRentermodiin2007        <- Rentermodiin2007        %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterbetshemesh2007    <- Renterbetshemesh2007    %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenternahariyya2007     <- Renternahariyya2007     %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRenterqiryatatta2007    <- Renterqiryatatta2007    %>%  dplyr::filter( INCGROSS > medianInc2007)
 richRentergivatayim2007     <- Rentergivatayim2007     %>%  dplyr::filter( INCGROSS > medianInc2007)

 richRenterjerusalem2008     <- Renterjerusalem2008     %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterAshdod2008        <- RenterAshdod2008        %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterHaifa2008         <- RenterHaifa2008         %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterHerzliyya2008     <- RenterHerzliyya2008     %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterHolon2008         <- RenterHolon2008         %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterLod2008           <- RenterLod2008           %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterHadera2008        <- RenterHadera2008        %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterAshqelon2008      <- RenterAshqelon2008      %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterNettanya2008      <- RenterNettanya2008      %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterRehovot2008       <- RenterRehovot2008       %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterRamla2008         <- RenterRamla2008         %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterRaannana2008      <- RenterRaannana2008      %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRentertlv2008           <- Rentertlv2008           %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterbene2008          <- Renterbene2008          %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterbat2008           <- Renterbat2008           %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterkefar2008         <- Renterkefar2008         %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterpetah2008         <- Renterpetah2008         %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterrishon2008        <- Renterrishon2008        %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterramat2008         <- Renterramat2008         %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterbeer2008          <- Renterbeer2008          %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRentermodiin2008        <- Rentermodiin2008        %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterbetshemesh2008    <- Renterbetshemesh2008    %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenternahariyya2008     <- Renternahariyya2008     %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRenterqiryatatta2008    <- Renterqiryatatta2008    %>%  dplyr::filter( INCGROSS > medianInc2008)
 richRentergivatayim2008     <- Rentergivatayim2008     %>%  dplyr::filter( INCGROSS > medianInc2008)

 richRenterjerusalem2009     <- Renterjerusalem2009     %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterAshdod2009        <- RenterAshdod2009        %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterHaifa2009         <- RenterHaifa2009         %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterHerzliyya2009     <- RenterHerzliyya2009     %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterHolon2009         <- RenterHolon2009         %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterLod2009           <- RenterLod2009           %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterHadera2009        <- RenterHadera2009        %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterAshqelon2009      <- RenterAshqelon2009      %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterNettanya2009      <- RenterNettanya2009      %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterRehovot2009       <- RenterRehovot2009       %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterRamla2009         <- RenterRamla2009         %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterRaannana2009      <- RenterRaannana2009      %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRentertlv2009           <- Rentertlv2009           %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterbene2009          <- Renterbene2009          %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterbat2009           <- Renterbat2009           %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterkefar2009         <- Renterkefar2009         %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterpetah2009         <- Renterpetah2009         %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterrishon2009        <- Renterrishon2009        %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterramat2009         <- Renterramat2009         %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterbeer2009          <- Renterbeer2009          %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRentermodiin2009        <- Rentermodiin2009        %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterbetshemesh2009    <- Renterbetshemesh2009    %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenternahariyya2009     <- Renternahariyya2009     %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRenterqiryatatta2009    <- Renterqiryatatta2009    %>%  dplyr::filter( INCGROSS > medianInc2009)
 richRentergivatayim2009     <- Rentergivatayim2009     %>%  dplyr::filter( INCGROSS > medianInc2009)

 richRenterjerusalem2010     <- Renterjerusalem2010     %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterAshdod2010        <- RenterAshdod2010        %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterHaifa2010         <- RenterHaifa2010         %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterHerzliyya2010     <- RenterHerzliyya2010     %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterHolon2010         <- RenterHolon2010         %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterLod2010           <- RenterLod2010           %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterHadera2010        <- RenterHadera2010        %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterAshqelon2010      <- RenterAshqelon2010      %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterNettanya2010      <- RenterNettanya2010      %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterRehovot2010       <- RenterRehovot2010       %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterRamla2010         <- RenterRamla2010         %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterRaannana2010      <- RenterRaannana2010      %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRentertlv2010           <- Rentertlv2010           %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterbene2010          <- Renterbene2010          %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterbat2010           <- Renterbat2010           %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterkefar2010         <- Renterkefar2010         %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterpetah2010         <- Renterpetah2010         %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterrishon2010        <- Renterrishon2010        %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterramat2010         <- Renterramat2010         %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterbeer2010          <- Renterbeer2010          %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRentermodiin2010        <- Rentermodiin2010        %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterbetshemesh2010    <- Renterbetshemesh2010    %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenternahariyya2010     <- Renternahariyya2010     %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRenterqiryatatta2010    <- Renterqiryatatta2010    %>%  dplyr::filter( INCGROSS > medianInc2010)
 richRentergivatayim2010     <- Rentergivatayim2010     %>%  dplyr::filter( INCGROSS > medianInc2010)

 richRenterjerusalem2011     <- Renterjerusalem2011     %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterAshdod2011        <- RenterAshdod2011        %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterHaifa2011         <- RenterHaifa2011         %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterHerzliyya2011     <- RenterHerzliyya2011     %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterHolon2011         <- RenterHolon2011         %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterLod2011           <- RenterLod2011           %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterHadera2011        <- RenterHadera2011        %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterAshqelon2011      <- RenterAshqelon2011      %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterNettanya2011      <- RenterNettanya2011      %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterRehovot2011       <- RenterRehovot2011       %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterRamla2011         <- RenterRamla2011         %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterRaannana2011      <- RenterRaannana2011      %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRentertlv2011           <- Rentertlv2011           %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterbene2011          <- Renterbene2011          %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterbat2011           <- Renterbat2011           %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterkefar2011         <- Renterkefar2011         %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterpetah2011         <- Renterpetah2011         %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterrishon2011        <- Renterrishon2011        %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterramat2011         <- Renterramat2011         %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterbeer2011          <- Renterbeer2011          %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRentermodiin2011        <- Rentermodiin2011        %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterbetshemesh2011    <- Renterbetshemesh2011    %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenternahariyya2011     <- Renternahariyya2011     %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRenterqiryatatta2011    <- Renterqiryatatta2011    %>%  dplyr::filter( INCGROSS > medianInc2011)
 richRentergivatayim2011     <- Rentergivatayim2011     %>%  dplyr::filter( INCGROSS > medianInc2011)

 richRenterjerusalem2012     <- Renterjerusalem2012     %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterAshdod2012        <- RenterAshdod2012        %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterHaifa2012         <- RenterHaifa2012         %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterHerzliyya2012     <- RenterHerzliyya2012     %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterHolon2012         <- RenterHolon2012         %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterLod2012           <- RenterLod2012           %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterHadera2012        <- RenterHadera2012        %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterAshqelon2012      <- RenterAshqelon2012      %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterNettanya2012      <- RenterNettanya2012      %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterRehovot2012       <- RenterRehovot2012       %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterRamla2012         <- RenterRamla2012         %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterRaannana2012      <- RenterRaannana2012      %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRentertlv2012           <- Rentertlv2012           %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterbene2012          <- Renterbene2012          %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterbat2012           <- Renterbat2012           %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterkefar2012         <- Renterkefar2012         %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterpetah2012         <- Renterpetah2012         %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterrishon2012        <- Renterrishon2012        %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterramat2012         <- Renterramat2012         %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterbeer2012          <- Renterbeer2012          %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRentermodiin2012        <- Rentermodiin2012        %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterbetshemesh2012    <- Renterbetshemesh2012    %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenternahariyya2012     <- Renternahariyya2012     %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRenterqiryatatta2012    <- Renterqiryatatta2012    %>%  dplyr::filter( INCGROSS > medianInc2012)
 richRentergivatayim2012     <- Rentergivatayim2012     %>%  dplyr::filter( INCGROSS > medianInc2012)

 richRenterjerusalem2013     <- Renterjerusalem2013     %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterAshdod2013        <- RenterAshdod2013        %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterHaifa2013         <- RenterHaifa2013         %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterHerzliyya2013     <- RenterHerzliyya2013     %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterHolon2013         <- RenterHolon2013         %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterLod2013           <- RenterLod2013           %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterHadera2013        <- RenterHadera2013        %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterAshqelon2013      <- RenterAshqelon2013      %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterNettanya2013      <- RenterNettanya2013      %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterRehovot2013       <- RenterRehovot2013       %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterRamla2013         <- RenterRamla2013         %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterRaannana2013      <- RenterRaannana2013      %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRentertlv2013           <- Rentertlv2013           %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterbene2013          <- Renterbene2013          %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterbat2013           <- Renterbat2013           %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterkefar2013         <- Renterkefar2013         %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterpetah2013         <- Renterpetah2013         %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterrishon2013        <- Renterrishon2013        %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterramat2013         <- Renterramat2013         %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterbeer2013          <- Renterbeer2013          %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRentermodiin2013        <- Rentermodiin2013        %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterbetshemesh2013    <- Renterbetshemesh2013    %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenternahariyya2013     <- Renternahariyya2013     %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRenterqiryatatta2013    <- Renterqiryatatta2013    %>%  dplyr::filter( INCGROSS > medianInc2013)
 richRentergivatayim2013     <- Rentergivatayim2013     %>%  dplyr::filter( INCGROSS > medianInc2013)

 richRenterjerusalem2014     <- Renterjerusalem2014     %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterAshdod2014        <- RenterAshdod2014        %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterHaifa2014         <- RenterHaifa2014         %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterHerzliyya2014     <- RenterHerzliyya2014     %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterHolon2014         <- RenterHolon2014         %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterLod2014           <- RenterLod2014           %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterHadera2014        <- RenterHadera2014        %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterAshqelon2014      <- RenterAshqelon2014      %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterNettanya2014      <- RenterNettanya2014      %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterRehovot2014       <- RenterRehovot2014       %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterRamla2014         <- RenterRamla2014         %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterRaannana2014      <- RenterRaannana2014      %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRentertlv2014           <- Rentertlv2014           %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterbene2014          <- Renterbene2014          %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterbat2014           <- Renterbat2014           %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterkefar2014         <- Renterkefar2014         %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterpetah2014         <- Renterpetah2014         %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterrishon2014        <- Renterrishon2014        %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterramat2014         <- Renterramat2014         %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterbeer2014          <- Renterbeer2014          %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRentermodiin2014        <- Rentermodiin2014        %>%  dplyr::filter( INCGROSS > medianInc2014)
 richRenterbetshemesh2014    <- Renterbetshemesh2014    %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenternahariyya2014     <- Renternahariyya2014     %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRenterqiryatatta2014    <- Renterqiryatatta2014    %>%  dplyr::filter( INCGROSS > medianInc2004)
 richRentergivatayim2014     <- Rentergivatayim2014     %>%  dplyr::filter( INCGROSS > medianInc2004)

############   renter ##### below median income  ###############

 poorRenterjerusalem2004     <- Renterjerusalem2004     %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterAshdod2004        <- RenterAshdod2004        %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterHaifa2004         <- RenterHaifa2004         %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterHerzliyya2004     <- RenterHerzliyya2004     %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterHolon2004         <- RenterHolon2004         %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterLod2004           <- RenterLod2004           %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterHadera2004        <- RenterHadera2004        %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterAshqelon2004      <- RenterAshqelon2004      %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterNettanya2004      <- RenterNettanya2004      %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterRehovot2004       <- RenterRehovot2004       %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterRamla2004         <- RenterRamla2004         %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterRaannana2004      <- RenterRaannana2004      %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRentertlv2004           <- Rentertlv2004           %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterbene2004          <- Renterbene2004          %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterbat2004           <- Renterbat2004           %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterkefar2004         <- Renterkefar2004         %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterpetah2004         <- Renterpetah2004         %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterrishon2004        <- Renterrishon2004        %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterramat2004         <- Renterramat2004         %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterbeer2004          <- Renterbeer2004          %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRentermodiin2004        <- Rentermodiin2004        %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterbetshemesh2004    <- Renterbetshemesh2004    %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenternahariyya2004     <- Renternahariyya2004     %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterqiryatatta2004    <- Renterqiryatatta2004    %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRentergivatayim2004     <- Rentergivatayim2004     %>%  dplyr::filter( INCGROSS <= medianInc2004)

 poorRenterjerusalem2005     <- Renterjerusalem2005     %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterAshdod2005        <- RenterAshdod2005        %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterHaifa2005         <- RenterHaifa2005         %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterHerzliyya2005     <- RenterHerzliyya2005     %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterHolon2005         <- RenterHolon2005         %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterLod2005           <- RenterLod2005           %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterHadera2005        <- RenterHadera2005        %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterAshqelon2005      <- RenterAshqelon2005      %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterNettanya2005      <- RenterNettanya2005      %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterRehovot2005       <- RenterRehovot2005       %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterRamla2005         <- RenterRamla2005         %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterRaannana2005      <- RenterRaannana2005      %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRentertlv2005           <- Rentertlv2005           %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterbene2005          <- Renterbene2005          %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterbat2005           <- Renterbat2005           %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterkefar2005         <- Renterkefar2005         %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterpetah2005         <- Renterpetah2005         %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterrishon2005        <- Renterrishon2005        %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterramat2005         <- Renterramat2005         %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterbeer2005          <- Renterbeer2005          %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRentermodiin2005        <- Rentermodiin2005        %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterbetshemesh2005    <- Renterbetshemesh2005    %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenternahariyya2005     <- Renternahariyya2005     %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRenterqiryatatta2005    <- Renterqiryatatta2005    %>%  dplyr::filter( INCGROSS <= medianInc2005)
 poorRentergivatayim2005     <- Rentergivatayim2005     %>%  dplyr::filter( INCGROSS <= medianInc2005)

 poorRenterjerusalem2006     <- Renterjerusalem2006     %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterAshdod2006        <- RenterAshdod2006        %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterHaifa2006         <- RenterHaifa2006         %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterHerzliyya2006     <- RenterHerzliyya2006     %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterHolon2006         <- RenterHolon2006         %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterLod2006           <- RenterLod2006           %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterHadera2006        <- RenterHadera2006        %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterAshqelon2006      <- RenterAshqelon2006      %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterNettanya2006      <- RenterNettanya2006      %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterRehovot2006       <- RenterRehovot2006       %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterRamla2006         <- RenterRamla2006         %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterRaannana2006      <- RenterRaannana2006      %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRentertlv2006           <- Rentertlv2006           %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterbene2006          <- Renterbene2006          %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterbat2006           <- Renterbat2006           %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterkefar2006         <- Renterkefar2006         %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterpetah2006         <- Renterpetah2006         %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterrishon2006        <- Renterrishon2006        %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterramat2006         <- Renterramat2006         %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterbeer2006          <- Renterbeer2006          %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRentermodiin2006        <- Rentermodiin2006        %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterbetshemesh2006    <- Renterbetshemesh2006    %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenternahariyya2006     <- Renternahariyya2006     %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRenterqiryatatta2006    <- Renterqiryatatta2006    %>%  dplyr::filter( INCGROSS <= medianInc2006)
 poorRentergivatayim2006     <- Rentergivatayim2006     %>%  dplyr::filter( INCGROSS <= medianInc2006)

 poorRenterjerusalem2007     <- Renterjerusalem2007     %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterAshdod2007        <- RenterAshdod2007        %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterHaifa2007         <- RenterHaifa2007         %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterHerzliyya2007     <- RenterHerzliyya2007     %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterHolon2007         <- RenterHolon2007         %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterLod2007           <- RenterLod2007           %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterHadera2007        <- RenterHadera2007        %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterAshqelon2007      <- RenterAshqelon2007      %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterNettanya2007      <- RenterNettanya2007      %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterRehovot2007       <- RenterRehovot2007       %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterRamla2007         <- RenterRamla2007         %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterRaannana2007      <- RenterRaannana2007      %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRentertlv2007           <- Rentertlv2007           %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterbene2007          <- Renterbene2007          %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterbat2007           <- Renterbat2007           %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterkefar2007         <- Renterkefar2007         %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterpetah2007         <- Renterpetah2007         %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterrishon2007        <- Renterrishon2007        %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterramat2007         <- Renterramat2007         %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterbeer2007          <- Renterbeer2007          %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRentermodiin2007        <- Rentermodiin2007        %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterbetshemesh2007    <- Renterbetshemesh2007    %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenternahariyya2007     <- Renternahariyya2007     %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRenterqiryatatta2007    <- Renterqiryatatta2007    %>%  dplyr::filter( INCGROSS <= medianInc2007)
 poorRentergivatayim2007     <- Rentergivatayim2007     %>%  dplyr::filter( INCGROSS <= medianInc2007)

 poorRenterjerusalem2008     <- Renterjerusalem2008     %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterAshdod2008        <- RenterAshdod2008        %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterHaifa2008         <- RenterHaifa2008         %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterHerzliyya2008     <- RenterHerzliyya2008     %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterHolon2008         <- RenterHolon2008         %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterLod2008           <- RenterLod2008           %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterHadera2008        <- RenterHadera2008        %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterAshqelon2008      <- RenterAshqelon2008      %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterNettanya2008      <- RenterNettanya2008      %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterRehovot2008       <- RenterRehovot2008       %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterRamla2008         <- RenterRamla2008         %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterRaannana2008      <- RenterRaannana2008      %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRentertlv2008           <- Rentertlv2008           %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterbene2008          <- Renterbene2008          %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterbat2008           <- Renterbat2008           %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterkefar2008         <- Renterkefar2008         %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterpetah2008         <- Renterpetah2008         %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterrishon2008        <- Renterrishon2008        %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterramat2008         <- Renterramat2008         %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterbeer2008          <- Renterbeer2008          %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRentermodiin2008        <- Rentermodiin2008        %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterbetshemesh2008    <- Renterbetshemesh2008    %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenternahariyya2008     <- Renternahariyya2008     %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRenterqiryatatta2008    <- Renterqiryatatta2008    %>%  dplyr::filter( INCGROSS <= medianInc2008)
 poorRentergivatayim2008     <- Rentergivatayim2008     %>%  dplyr::filter( INCGROSS <= medianInc2008)

 poorRenterjerusalem2009     <- Renterjerusalem2009     %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterAshdod2009        <- RenterAshdod2009        %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterHaifa2009         <- RenterHaifa2009         %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterHerzliyya2009     <- RenterHerzliyya2009     %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterHolon2009         <- RenterHolon2009         %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterLod2009           <- RenterLod2009           %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterHadera2009        <- RenterHadera2009        %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterAshqelon2009      <- RenterAshqelon2009      %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterNettanya2009      <- RenterNettanya2009      %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterRehovot2009       <- RenterRehovot2009       %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterRamla2009         <- RenterRamla2009         %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterRaannana2009      <- RenterRaannana2009      %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRentertlv2009           <- Rentertlv2009           %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterbene2009          <- Renterbene2009          %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterbat2009           <- Renterbat2009           %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterkefar2009         <- Renterkefar2009         %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterpetah2009         <- Renterpetah2009         %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterrishon2009        <- Renterrishon2009        %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterramat2009         <- Renterramat2009         %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterbeer2009          <- Renterbeer2009          %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRentermodiin2009        <- Rentermodiin2009        %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterbetshemesh2009    <- Renterbetshemesh2009    %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenternahariyya2009     <- Renternahariyya2009     %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRenterqiryatatta2009    <- Renterqiryatatta2009    %>%  dplyr::filter( INCGROSS <= medianInc2009)
 poorRentergivatayim2009     <- Rentergivatayim2009     %>%  dplyr::filter( INCGROSS <= medianInc2009)

 poorRenterjerusalem2010     <- Renterjerusalem2010     %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterAshdod2010        <- RenterAshdod2010        %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterHaifa2010         <- RenterHaifa2010         %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterHerzliyya2010     <- RenterHerzliyya2010     %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterHolon2010         <- RenterHolon2010         %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterLod2010           <- RenterLod2010           %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterHadera2010        <- RenterHadera2010        %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterAshqelon2010      <- RenterAshqelon2010      %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterNettanya2010      <- RenterNettanya2010      %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterRehovot2010       <- RenterRehovot2010       %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterRamla2010         <- RenterRamla2010         %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterRaannana2010      <- RenterRaannana2010      %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRentertlv2010           <- Rentertlv2010           %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterbene2010          <- Renterbene2010          %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterbat2010           <- Renterbat2010           %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterkefar2010         <- Renterkefar2010         %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterpetah2010         <- Renterpetah2010         %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterrishon2010        <- Renterrishon2010        %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterramat2010         <- Renterramat2010         %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterbeer2010          <- Renterbeer2010          %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRentermodiin2010        <- Rentermodiin2010        %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterbetshemesh2010    <- Renterbetshemesh2010    %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenternahariyya2010     <- Renternahariyya2010     %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRenterqiryatatta2010    <- Renterqiryatatta2010    %>%  dplyr::filter( INCGROSS <= medianInc2010)
 poorRentergivatayim2010     <- Rentergivatayim2010     %>%  dplyr::filter( INCGROSS <= medianInc2010)

 poorRenterjerusalem2011     <- Renterjerusalem2011     %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterAshdod2011        <- RenterAshdod2011        %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterHaifa2011         <- RenterHaifa2011         %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterHerzliyya2011     <- RenterHerzliyya2011     %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterHolon2011         <- RenterHolon2011         %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterLod2011           <- RenterLod2011           %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterHadera2011        <- RenterHadera2011        %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterAshqelon2011      <- RenterAshqelon2011      %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterNettanya2011      <- RenterNettanya2011      %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterRehovot2011       <- RenterRehovot2011       %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterRamla2011         <- RenterRamla2011         %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterRaannana2011      <- RenterRaannana2011      %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRentertlv2011           <- Rentertlv2011           %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterbene2011          <- Renterbene2011          %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterbat2011           <- Renterbat2011           %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterkefar2011         <- Renterkefar2011         %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterpetah2011         <- Renterpetah2011         %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterrishon2011        <- Renterrishon2011        %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterramat2011         <- Renterramat2011         %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterbeer2011          <- Renterbeer2011          %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRentermodiin2011        <- Rentermodiin2011        %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterbetshemesh2011    <- Renterbetshemesh2011    %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenternahariyya2011     <- Renternahariyya2011     %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRenterqiryatatta2011    <- Renterqiryatatta2011    %>%  dplyr::filter( INCGROSS <= medianInc2011)
 poorRentergivatayim2011     <- Rentergivatayim2011     %>%  dplyr::filter( INCGROSS <= medianInc2011)

 poorRenterjerusalem2012     <- Renterjerusalem2012     %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterAshdod2012        <- RenterAshdod2012        %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterHaifa2012         <- RenterHaifa2012         %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterHerzliyya2012     <- RenterHerzliyya2012     %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterHolon2012         <- RenterHolon2012         %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterLod2012           <- RenterLod2012           %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterHadera2012        <- RenterHadera2012        %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterAshqelon2012      <- RenterAshqelon2012      %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterNettanya2012      <- RenterNettanya2012      %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterRehovot2012       <- RenterRehovot2012       %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterRamla2012         <- RenterRamla2012         %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterRaannana2012      <- RenterRaannana2012      %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRentertlv2012           <- Rentertlv2012           %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterbene2012          <- Renterbene2012          %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterbat2012           <- Renterbat2012           %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterkefar2012         <- Renterkefar2012         %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterpetah2012         <- Renterpetah2012         %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterrishon2012        <- Renterrishon2012        %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterramat2012         <- Renterramat2012         %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterbeer2012          <- Renterbeer2012          %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRentermodiin2012        <- Rentermodiin2012        %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterbetshemesh2012    <- Renterbetshemesh2012    %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenternahariyya2012     <- Renternahariyya2012     %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRenterqiryatatta2012    <- Renterqiryatatta2012    %>%  dplyr::filter( INCGROSS <= medianInc2012)
 poorRentergivatayim2012     <- Rentergivatayim2012     %>%  dplyr::filter( INCGROSS <= medianInc2012)

 poorRenterjerusalem2013     <- Renterjerusalem2013     %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterAshdod2013        <- RenterAshdod2013        %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterHaifa2013         <- RenterHaifa2013         %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterHerzliyya2013     <- RenterHerzliyya2013     %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterHolon2013         <- RenterHolon2013         %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterLod2013           <- RenterLod2013           %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterHadera2013        <- RenterHadera2013        %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterAshqelon2013      <- RenterAshqelon2013      %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterNettanya2013      <- RenterNettanya2013      %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterRehovot2013       <- RenterRehovot2013       %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterRamla2013         <- RenterRamla2013         %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterRaannana2013      <- RenterRaannana2013      %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRentertlv2013           <- Rentertlv2013           %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterbene2013          <- Renterbene2013          %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterbat2013           <- Renterbat2013           %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterkefar2013         <- Renterkefar2013         %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterpetah2013         <- Renterpetah2013         %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterrishon2013        <- Renterrishon2013        %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterramat2013         <- Renterramat2013         %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterbeer2013          <- Renterbeer2013          %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRentermodiin2013        <- Rentermodiin2013        %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterbetshemesh2013    <- Renterbetshemesh2013    %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenternahariyya2013     <- Renternahariyya2013     %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRenterqiryatatta2013    <- Renterqiryatatta2013    %>%  dplyr::filter( INCGROSS <= medianInc2013)
 poorRentergivatayim2013     <- Rentergivatayim2013     %>%  dplyr::filter( INCGROSS <= medianInc2013)

 poorRenterjerusalem2014     <- Renterjerusalem2014     %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterAshdod2014        <- RenterAshdod2014        %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterHaifa2014         <- RenterHaifa2014         %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterHerzliyya2014     <- RenterHerzliyya2014     %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterHolon2014         <- RenterHolon2014         %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterLod2014           <- RenterLod2014           %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterHadera2014        <- RenterHadera2014        %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterAshqelon2014      <- RenterAshqelon2014      %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterNettanya2014      <- RenterNettanya2014      %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterRehovot2014       <- RenterRehovot2014       %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterRamla2014         <- RenterRamla2014         %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterRaannana2014      <- RenterRaannana2014      %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRentertlv2014           <- Rentertlv2014           %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterbene2014          <- Renterbene2014          %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterbat2014           <- Renterbat2014           %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterkefar2014         <- Renterkefar2014         %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterpetah2014         <- Renterpetah2014         %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterrishon2014        <- Renterrishon2014        %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterramat2014         <- Renterramat2014         %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterbeer2014          <- Renterbeer2014          %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRentermodiin2014        <- Rentermodiin2014        %>%  dplyr::filter( INCGROSS <= medianInc2014)
 poorRenterbetshemesh2014    <- Renterbetshemesh2014    %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenternahariyya2014     <- Renternahariyya2014     %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRenterqiryatatta2014    <- Renterqiryatatta2014    %>%  dplyr::filter( INCGROSS <= medianInc2004)
 poorRentergivatayim2014     <- Rentergivatayim2014     %>%  dplyr::filter( INCGROSS <= medianInc2004)


############   renter ### above median income      ###############
countRichRenters2004 <- c(
 richRenterjerusalem2004         %>% nrow,
 richRenterAshdod2004            %>% nrow,
 richRenterHaifa2004             %>% nrow,
 richRenterHerzliyya2004         %>% nrow,
 richRenterHolon2004             %>% nrow,
 richRenterLod2004               %>% nrow,
 richRenterHadera2004            %>% nrow,
 richRenterAshqelon2004          %>% nrow,
 richRenterNettanya2004          %>% nrow,
 richRenterRehovot2004           %>% nrow,
 richRenterRamla2004             %>% nrow,
 richRenterRaannana2004          %>% nrow,
 richRentertlv2004               %>% nrow,
 richRenterbene2004              %>% nrow,
 richRenterbat2004               %>% nrow,
 richRenterkefar2004             %>% nrow,
 richRenterpetah2004             %>% nrow,
 richRenterrishon2004            %>% nrow,
 richRenterramat2004             %>% nrow,
 richRenterbeer2004              %>% nrow,
 richRentermodiin2004            %>% nrow,
 richRenterbetshemesh2004        %>% nrow,
 richRenternahariyya2004         %>% nrow,
 richRenterqiryatatta2004        %>% nrow,
 richRentergivatayim2004         %>% nrow )

countRichRenters2005 <- c(
 richRenterjerusalem2005         %>% nrow,
 richRenterAshdod2005            %>% nrow,
 richRenterHaifa2005             %>% nrow,
 richRenterHerzliyya2005         %>% nrow,
 richRenterHolon2005             %>% nrow,
 richRenterLod2005               %>% nrow,
 richRenterHadera2005            %>% nrow,
 richRenterAshqelon2005          %>% nrow,
 richRenterNettanya2005          %>% nrow,
 richRenterRehovot2005           %>% nrow,
 richRenterRamla2005             %>% nrow,
 richRenterRaannana2005          %>% nrow,
 richRentertlv2005               %>% nrow,
 richRenterbene2005              %>% nrow,
 richRenterbat2005               %>% nrow,
 richRenterkefar2005             %>% nrow,
 richRenterpetah2005             %>% nrow,
 richRenterrishon2005            %>% nrow,
 richRenterramat2005             %>% nrow,
 richRenterbeer2005              %>% nrow,
 richRentermodiin2005            %>% nrow,
 richRenterbetshemesh2005        %>% nrow,
 richRenternahariyya2005         %>% nrow,
 richRenterqiryatatta2005        %>% nrow,
 richRentergivatayim2005         %>% nrow )

countRichRenters2006 <- c(
 richRenterjerusalem2006         %>% nrow,
 richRenterAshdod2006            %>% nrow,
 richRenterHaifa2006             %>% nrow,
 richRenterHerzliyya2006         %>% nrow,
 richRenterHolon2006             %>% nrow,
 richRenterLod2006               %>% nrow,
 richRenterHadera2006            %>% nrow,
 richRenterAshqelon2006          %>% nrow,
 richRenterNettanya2006          %>% nrow,
 richRenterRehovot2006           %>% nrow,
 richRenterRamla2006             %>% nrow,
 richRenterRaannana2006          %>% nrow,
 richRentertlv2006               %>% nrow,
 richRenterbene2006              %>% nrow,
 richRenterbat2006               %>% nrow,
 richRenterkefar2006             %>% nrow,
 richRenterpetah2006             %>% nrow,
 richRenterrishon2006            %>% nrow,
 richRenterramat2006             %>% nrow,
 richRenterbeer2006              %>% nrow,
 richRentermodiin2006            %>% nrow,
 richRenterbetshemesh2006        %>% nrow,
 richRenternahariyya2006         %>% nrow,
 richRenterqiryatatta2006        %>% nrow,
 richRentergivatayim2006         %>% nrow )


countRichRenters2007 <- c(
 richRenterjerusalem2007         %>% nrow,
 richRenterAshdod2007            %>% nrow,
 richRenterHaifa2007             %>% nrow,
 richRenterHerzliyya2007         %>% nrow,
 richRenterHolon2007             %>% nrow,
 richRenterLod2007               %>% nrow,
 richRenterHadera2007            %>% nrow,
 richRenterAshqelon2007          %>% nrow,
 richRenterNettanya2007          %>% nrow,
 richRenterRehovot2007           %>% nrow,
 richRenterRamla2007             %>% nrow,
 richRenterRaannana2007          %>% nrow,
 richRentertlv2007               %>% nrow,
 richRenterbene2007              %>% nrow,
 richRenterbat2007               %>% nrow,
 richRenterkefar2007             %>% nrow,
 richRenterpetah2007             %>% nrow,
 richRenterrishon2007            %>% nrow,
 richRenterramat2007             %>% nrow,
 richRenterbeer2007              %>% nrow,
 richRentermodiin2007            %>% nrow,
 richRenterbetshemesh2007        %>% nrow,
 richRenternahariyya2007         %>% nrow,
 richRenterqiryatatta2007        %>% nrow,
 richRentergivatayim2007         %>% nrow )


countRichRenters2008 <- c(
 richRenterjerusalem2008         %>% nrow,
 richRenterAshdod2008            %>% nrow,
 richRenterHaifa2008             %>% nrow,
 richRenterHerzliyya2008         %>% nrow,
 richRenterHolon2008             %>% nrow,
 richRenterLod2008               %>% nrow,
 richRenterHadera2008            %>% nrow,
 richRenterAshqelon2008          %>% nrow,
 richRenterNettanya2008          %>% nrow,
 richRenterRehovot2008           %>% nrow,
 richRenterRamla2008             %>% nrow,
 richRenterRaannana2008          %>% nrow,
 richRentertlv2008               %>% nrow,
 richRenterbene2008              %>% nrow,
 richRenterbat2008               %>% nrow,
 richRenterkefar2008             %>% nrow,
 richRenterpetah2008             %>% nrow,
 richRenterrishon2008            %>% nrow,
 richRenterramat2008             %>% nrow,
 richRenterbeer2008              %>% nrow,
 richRentermodiin2008            %>% nrow,
 richRenterbetshemesh2008        %>% nrow,
 richRenternahariyya2008         %>% nrow,
 richRenterqiryatatta2008        %>% nrow,
 richRentergivatayim2008         %>% nrow )


countRichRenters2009 <- c(
 richRenterjerusalem2009         %>% nrow,
 richRenterAshdod2009            %>% nrow,
 richRenterHaifa2009             %>% nrow,
 richRenterHerzliyya2009         %>% nrow,
 richRenterHolon2009             %>% nrow,
 richRenterLod2009               %>% nrow,
 richRenterHadera2009            %>% nrow,
 richRenterAshqelon2009          %>% nrow,
 richRenterNettanya2009          %>% nrow,
 richRenterRehovot2009           %>% nrow,
 richRenterRamla2009             %>% nrow,
 richRenterRaannana2009          %>% nrow,
 richRentertlv2009               %>% nrow,
 richRenterbene2009              %>% nrow,
 richRenterbat2009               %>% nrow,
 richRenterkefar2009             %>% nrow,
 richRenterpetah2009             %>% nrow,
 richRenterrishon2009            %>% nrow,
 richRenterramat2009             %>% nrow,
 richRenterbeer2009              %>% nrow,
 richRentermodiin2009            %>% nrow,
 richRenterbetshemesh2009        %>% nrow,
 richRenternahariyya2009         %>% nrow,
 richRenterqiryatatta2009        %>% nrow,
 richRentergivatayim2009         %>% nrow )


countRichRenters2010 <- c(
 richRenterjerusalem2010         %>% nrow,
 richRenterAshdod2010            %>% nrow,
 richRenterHaifa2010             %>% nrow,
 richRenterHerzliyya2010         %>% nrow,
 richRenterHolon2010             %>% nrow,
 richRenterLod2010               %>% nrow,
 richRenterHadera2010            %>% nrow,
 richRenterAshqelon2010          %>% nrow,
 richRenterNettanya2010          %>% nrow,
 richRenterRehovot2010           %>% nrow,
 richRenterRamla2010             %>% nrow,
 richRenterRaannana2010          %>% nrow,
 richRentertlv2010               %>% nrow,
 richRenterbene2010              %>% nrow,
 richRenterbat2010               %>% nrow,
 richRenterkefar2010             %>% nrow,
 richRenterpetah2010             %>% nrow,
 richRenterrishon2010            %>% nrow,
 richRenterramat2010             %>% nrow,
 richRenterbeer2010              %>% nrow,
 richRentermodiin2010            %>% nrow,
 richRenterbetshemesh2010        %>% nrow,
 richRenternahariyya2010         %>% nrow,
 richRenterqiryatatta2010        %>% nrow,
 richRentergivatayim2010         %>% nrow )


countRichRenters2011 <- c(
 richRenterjerusalem2011         %>% nrow,
 richRenterAshdod2011            %>% nrow,
 richRenterHaifa2011             %>% nrow,
 richRenterHerzliyya2011         %>% nrow,
 richRenterHolon2011             %>% nrow,
 richRenterLod2011               %>% nrow,
 richRenterHadera2011            %>% nrow,
 richRenterAshqelon2011          %>% nrow,
 richRenterNettanya2011          %>% nrow,
 richRenterRehovot2011           %>% nrow,
 richRenterRamla2011             %>% nrow,
 richRenterRaannana2011          %>% nrow,
 richRentertlv2011               %>% nrow,
 richRenterbene2011              %>% nrow,
 richRenterbat2011               %>% nrow,
 richRenterkefar2011             %>% nrow,
 richRenterpetah2011             %>% nrow,
 richRenterrishon2011            %>% nrow,
 richRenterramat2011             %>% nrow,
 richRenterbeer2011              %>% nrow,
 richRentermodiin2011            %>% nrow,
 richRenterbetshemesh2011        %>% nrow,
 richRenternahariyya2011         %>% nrow,
 richRenterqiryatatta2011        %>% nrow,
 richRentergivatayim2011         %>% nrow )


countRichRenters2012 <- c(
 richRenterjerusalem2012         %>% nrow,
 richRenterAshdod2012            %>% nrow,
 richRenterHaifa2012             %>% nrow,
 richRenterHerzliyya2012         %>% nrow,
 richRenterHolon2012             %>% nrow,
 richRenterLod2012               %>% nrow,
 richRenterHadera2012            %>% nrow,
 richRenterAshqelon2012          %>% nrow,
 richRenterNettanya2012          %>% nrow,
 richRenterRehovot2012           %>% nrow,
 richRenterRamla2012             %>% nrow,
 richRenterRaannana2012          %>% nrow,
 richRentertlv2012               %>% nrow,
 richRenterbene2012              %>% nrow,
 richRenterbat2012               %>% nrow,
 richRenterkefar2012             %>% nrow,
 richRenterpetah2012             %>% nrow,
 richRenterrishon2012            %>% nrow,
 richRenterramat2012             %>% nrow,
 richRenterbeer2012              %>% nrow,
 richRentermodiin2012            %>% nrow,
 richRenterbetshemesh2012        %>% nrow,
 richRenternahariyya2012         %>% nrow,
 richRenterqiryatatta2012        %>% nrow,
 richRentergivatayim2012         %>% nrow )


countRichRenters2013 <- c(
 richRenterjerusalem2013         %>% nrow,
 richRenterAshdod2013            %>% nrow,
 richRenterHaifa2013             %>% nrow,
 richRenterHerzliyya2013         %>% nrow,
 richRenterHolon2013             %>% nrow,
 richRenterLod2013               %>% nrow,
 richRenterHadera2013            %>% nrow,
 richRenterAshqelon2013          %>% nrow,
 richRenterNettanya2013          %>% nrow,
 richRenterRehovot2013           %>% nrow,
 richRenterRamla2013             %>% nrow,
 richRenterRaannana2013          %>% nrow,
 richRentertlv2013               %>% nrow,
 richRenterbene2013              %>% nrow,
 richRenterbat2013               %>% nrow,
 richRenterkefar2013             %>% nrow,
 richRenterpetah2013             %>% nrow,
 richRenterrishon2013            %>% nrow,
 richRenterramat2013             %>% nrow,
 richRenterbeer2013              %>% nrow,
 richRentermodiin2013            %>% nrow,
 richRenterbetshemesh2013        %>% nrow,
 richRenternahariyya2013         %>% nrow,
 richRenterqiryatatta2013        %>% nrow,
 richRentergivatayim2013         %>% nrow )


countRichRenters2014 <- c(
 richRenterjerusalem2014         %>% nrow,
 richRenterAshdod2014            %>% nrow,
 richRenterHaifa2014             %>% nrow,
 richRenterHerzliyya2014         %>% nrow,
 richRenterHolon2014             %>% nrow,
 richRenterLod2014               %>% nrow,
 richRenterHadera2014            %>% nrow,
 richRenterAshqelon2014          %>% nrow,
 richRenterNettanya2014          %>% nrow,
 richRenterRehovot2014           %>% nrow,
 richRenterRamla2014             %>% nrow,
 richRenterRaannana2014          %>% nrow,
 richRentertlv2014               %>% nrow,
 richRenterbene2014              %>% nrow,
 richRenterbat2014               %>% nrow,
 richRenterkefar2014             %>% nrow,
 richRenterpetah2014             %>% nrow,
 richRenterrishon2014            %>% nrow,
 richRenterramat2014             %>% nrow,
 richRenterbeer2014              %>% nrow,
 richRentermodiin2014            %>% nrow,
 richRenterbetshemesh2014        %>% nrow,
 richRenternahariyya2014         %>% nrow,
 richRenterqiryatatta2014        %>% nrow,
 richRentergivatayim2014         %>% nrow )



############   renter ##### below median income  ###############
countPoorRenters2004 <- c(
 poorRenterjerusalem2004         %>% nrow,
 poorRenterAshdod2004            %>% nrow,
 poorRenterHaifa2004             %>% nrow,
 poorRenterHerzliyya2004         %>% nrow,
 poorRenterHolon2004             %>% nrow,
 poorRenterLod2004               %>% nrow,
 poorRenterHadera2004            %>% nrow,
 poorRenterAshqelon2004          %>% nrow,
 poorRenterNettanya2004          %>% nrow,
 poorRenterRehovot2004           %>% nrow,
 poorRenterRamla2004             %>% nrow,
 poorRenterRaannana2004          %>% nrow,
 poorRentertlv2004               %>% nrow,
 poorRenterbene2004              %>% nrow,
 poorRenterbat2004               %>% nrow,
 poorRenterkefar2004             %>% nrow,
 poorRenterpetah2004             %>% nrow,
 poorRenterrishon2004            %>% nrow,
 poorRenterramat2004             %>% nrow,
 poorRenterbeer2004              %>% nrow,
 poorRentermodiin2004            %>% nrow,
 poorRenterbetshemesh2004        %>% nrow,
 poorRenternahariyya2004         %>% nrow,
 poorRenterqiryatatta2004        %>% nrow,
 poorRentergivatayim2004         %>% nrow )

countPoorRenters2005 <- c(
 poorRenterjerusalem2005         %>% nrow,
 poorRenterAshdod2005            %>% nrow,
 poorRenterHaifa2005             %>% nrow,
 poorRenterHerzliyya2005         %>% nrow,
 poorRenterHolon2005             %>% nrow,
 poorRenterLod2005               %>% nrow,
 poorRenterHadera2005            %>% nrow,
 poorRenterAshqelon2005          %>% nrow,
 poorRenterNettanya2005          %>% nrow,
 poorRenterRehovot2005           %>% nrow,
 poorRenterRamla2005             %>% nrow,
 poorRenterRaannana2005          %>% nrow,
 poorRentertlv2005               %>% nrow,
 poorRenterbene2005              %>% nrow,
 poorRenterbat2005               %>% nrow,
 poorRenterkefar2005             %>% nrow,
 poorRenterpetah2005             %>% nrow,
 poorRenterrishon2005            %>% nrow,
 poorRenterramat2005             %>% nrow,
 poorRenterbeer2005              %>% nrow,
 poorRentermodiin2005            %>% nrow,
 poorRenterbetshemesh2005        %>% nrow,
 poorRenternahariyya2005         %>% nrow,
 poorRenterqiryatatta2005        %>% nrow,
 poorRentergivatayim2005         %>% nrow )

countPoorRenters2006 <- c(
 poorRenterjerusalem2006         %>% nrow,
 poorRenterAshdod2006            %>% nrow,
 poorRenterHaifa2006             %>% nrow,
 poorRenterHerzliyya2006         %>% nrow,
 poorRenterHolon2006             %>% nrow,
 poorRenterLod2006               %>% nrow,
 poorRenterHadera2006            %>% nrow,
 poorRenterAshqelon2006          %>% nrow,
 poorRenterNettanya2006          %>% nrow,
 poorRenterRehovot2006           %>% nrow,
 poorRenterRamla2006             %>% nrow,
 poorRenterRaannana2006          %>% nrow,
 poorRentertlv2006               %>% nrow,
 poorRenterbene2006              %>% nrow,
 poorRenterbat2006               %>% nrow,
 poorRenterkefar2006             %>% nrow,
 poorRenterpetah2006             %>% nrow,
 poorRenterrishon2006            %>% nrow,
 poorRenterramat2006             %>% nrow,
 poorRenterbeer2006              %>% nrow,
 poorRentermodiin2006            %>% nrow,
 poorRenterbetshemesh2006        %>% nrow,
 poorRenternahariyya2006         %>% nrow,
 poorRenterqiryatatta2006        %>% nrow,
 poorRentergivatayim2006         %>% nrow )


countPoorRenters2007 <- c(
 poorRenterjerusalem2007         %>% nrow,
 poorRenterAshdod2007            %>% nrow,
 poorRenterHaifa2007             %>% nrow,
 poorRenterHerzliyya2007         %>% nrow,
 poorRenterHolon2007             %>% nrow,
 poorRenterLod2007               %>% nrow,
 poorRenterHadera2007            %>% nrow,
 poorRenterAshqelon2007          %>% nrow,
 poorRenterNettanya2007          %>% nrow,
 poorRenterRehovot2007           %>% nrow,
 poorRenterRamla2007             %>% nrow,
 poorRenterRaannana2007          %>% nrow,
 poorRentertlv2007               %>% nrow,
 poorRenterbene2007              %>% nrow,
 poorRenterbat2007               %>% nrow,
 poorRenterkefar2007             %>% nrow,
 poorRenterpetah2007             %>% nrow,
 poorRenterrishon2007            %>% nrow,
 poorRenterramat2007             %>% nrow,
 poorRenterbeer2007              %>% nrow,
 poorRentermodiin2007            %>% nrow,
 poorRenterbetshemesh2007        %>% nrow,
 poorRenternahariyya2007         %>% nrow,
 poorRenterqiryatatta2007        %>% nrow,
 poorRentergivatayim2007         %>% nrow )


countPoorRenters2008 <- c(
 poorRenterjerusalem2008         %>% nrow,
 poorRenterAshdod2008            %>% nrow,
 poorRenterHaifa2008             %>% nrow,
 poorRenterHerzliyya2008         %>% nrow,
 poorRenterHolon2008             %>% nrow,
 poorRenterLod2008               %>% nrow,
 poorRenterHadera2008            %>% nrow,
 poorRenterAshqelon2008          %>% nrow,
 poorRenterNettanya2008          %>% nrow,
 poorRenterRehovot2008           %>% nrow,
 poorRenterRamla2008             %>% nrow,
 poorRenterRaannana2008          %>% nrow,
 poorRentertlv2008               %>% nrow,
 poorRenterbene2008              %>% nrow,
 poorRenterbat2008               %>% nrow,
 poorRenterkefar2008             %>% nrow,
 poorRenterpetah2008             %>% nrow,
 poorRenterrishon2008            %>% nrow,
 poorRenterramat2008             %>% nrow,
 poorRenterbeer2008              %>% nrow,
 poorRentermodiin2008            %>% nrow,
 poorRenterbetshemesh2008        %>% nrow,
 poorRenternahariyya2008         %>% nrow,
 poorRenterqiryatatta2008        %>% nrow,
 poorRentergivatayim2008         %>% nrow )


countPoorRenters2009 <- c(
 poorRenterjerusalem2009         %>% nrow,
 poorRenterAshdod2009            %>% nrow,
 poorRenterHaifa2009             %>% nrow,
 poorRenterHerzliyya2009         %>% nrow,
 poorRenterHolon2009             %>% nrow,
 poorRenterLod2009               %>% nrow,
 poorRenterHadera2009            %>% nrow,
 poorRenterAshqelon2009          %>% nrow,
 poorRenterNettanya2009          %>% nrow,
 poorRenterRehovot2009           %>% nrow,
 poorRenterRamla2009             %>% nrow,
 poorRenterRaannana2009          %>% nrow,
 poorRentertlv2009               %>% nrow,
 poorRenterbene2009              %>% nrow,
 poorRenterbat2009               %>% nrow,
 poorRenterkefar2009             %>% nrow,
 poorRenterpetah2009             %>% nrow,
 poorRenterrishon2009            %>% nrow,
 poorRenterramat2009             %>% nrow,
 poorRenterbeer2009              %>% nrow,
 poorRentermodiin2009            %>% nrow,
 poorRenterbetshemesh2009        %>% nrow,
 poorRenternahariyya2009         %>% nrow,
 poorRenterqiryatatta2009        %>% nrow,
 poorRentergivatayim2009         %>% nrow )


countPoorRenters2010 <- c(
 poorRenterjerusalem2010         %>% nrow,
 poorRenterAshdod2010            %>% nrow,
 poorRenterHaifa2010             %>% nrow,
 poorRenterHerzliyya2010         %>% nrow,
 poorRenterHolon2010             %>% nrow,
 poorRenterLod2010               %>% nrow,
 poorRenterHadera2010            %>% nrow,
 poorRenterAshqelon2010          %>% nrow,
 poorRenterNettanya2010          %>% nrow,
 poorRenterRehovot2010           %>% nrow,
 poorRenterRamla2010             %>% nrow,
 poorRenterRaannana2010          %>% nrow,
 poorRentertlv2010               %>% nrow,
 poorRenterbene2010              %>% nrow,
 poorRenterbat2010               %>% nrow,
 poorRenterkefar2010             %>% nrow,
 poorRenterpetah2010             %>% nrow,
 poorRenterrishon2010            %>% nrow,
 poorRenterramat2010             %>% nrow,
 poorRenterbeer2010              %>% nrow,
 poorRentermodiin2010            %>% nrow,
 poorRenterbetshemesh2010        %>% nrow,
 poorRenternahariyya2010         %>% nrow,
 poorRenterqiryatatta2010        %>% nrow,
 poorRentergivatayim2010         %>% nrow )


countPoorRenters2011 <- c(
 poorRenterjerusalem2011         %>% nrow,
 poorRenterAshdod2011            %>% nrow,
 poorRenterHaifa2011             %>% nrow,
 poorRenterHerzliyya2011         %>% nrow,
 poorRenterHolon2011             %>% nrow,
 poorRenterLod2011               %>% nrow,
 poorRenterHadera2011            %>% nrow,
 poorRenterAshqelon2011          %>% nrow,
 poorRenterNettanya2011          %>% nrow,
 poorRenterRehovot2011           %>% nrow,
 poorRenterRamla2011             %>% nrow,
 poorRenterRaannana2011          %>% nrow,
 poorRentertlv2011               %>% nrow,
 poorRenterbene2011              %>% nrow,
 poorRenterbat2011               %>% nrow,
 poorRenterkefar2011             %>% nrow,
 poorRenterpetah2011             %>% nrow,
 poorRenterrishon2011            %>% nrow,
 poorRenterramat2011             %>% nrow,
 poorRenterbeer2011              %>% nrow,
 poorRentermodiin2011            %>% nrow,
 poorRenterbetshemesh2011        %>% nrow,
 poorRenternahariyya2011         %>% nrow,
 poorRenterqiryatatta2011        %>% nrow,
 poorRentergivatayim2011         %>% nrow )


countPoorRenters2012 <- c(
 poorRenterjerusalem2012         %>% nrow,
 poorRenterAshdod2012            %>% nrow,
 poorRenterHaifa2012             %>% nrow,
 poorRenterHerzliyya2012         %>% nrow,
 poorRenterHolon2012             %>% nrow,
 poorRenterLod2012               %>% nrow,
 poorRenterHadera2012            %>% nrow,
 poorRenterAshqelon2012          %>% nrow,
 poorRenterNettanya2012          %>% nrow,
 poorRenterRehovot2012           %>% nrow,
 poorRenterRamla2012             %>% nrow,
 poorRenterRaannana2012          %>% nrow,
 poorRentertlv2012               %>% nrow,
 poorRenterbene2012              %>% nrow,
 poorRenterbat2012               %>% nrow,
 poorRenterkefar2012             %>% nrow,
 poorRenterpetah2012             %>% nrow,
 poorRenterrishon2012            %>% nrow,
 poorRenterramat2012             %>% nrow,
 poorRenterbeer2012              %>% nrow,
 poorRentermodiin2012            %>% nrow,
 poorRenterbetshemesh2012        %>% nrow,
 poorRenternahariyya2012         %>% nrow,
 poorRenterqiryatatta2012        %>% nrow,
 poorRentergivatayim2012         %>% nrow )


countPoorRenters2013 <- c(
 poorRenterjerusalem2013         %>% nrow,
 poorRenterAshdod2013            %>% nrow,
 poorRenterHaifa2013             %>% nrow,
 poorRenterHerzliyya2013         %>% nrow,
 poorRenterHolon2013             %>% nrow,
 poorRenterLod2013               %>% nrow,
 poorRenterHadera2013            %>% nrow,
 poorRenterAshqelon2013          %>% nrow,
 poorRenterNettanya2013          %>% nrow,
 poorRenterRehovot2013           %>% nrow,
 poorRenterRamla2013             %>% nrow,
 poorRenterRaannana2013          %>% nrow,
 poorRentertlv2013               %>% nrow,
 poorRenterbene2013              %>% nrow,
 poorRenterbat2013               %>% nrow,
 poorRenterkefar2013             %>% nrow,
 poorRenterpetah2013             %>% nrow,
 poorRenterrishon2013            %>% nrow,
 poorRenterramat2013             %>% nrow,
 poorRenterbeer2013              %>% nrow,
 poorRentermodiin2013            %>% nrow,
 poorRenterbetshemesh2013        %>% nrow,
 poorRenternahariyya2013         %>% nrow,
 poorRenterqiryatatta2013        %>% nrow,
 poorRentergivatayim2013         %>% nrow )

countPoorRenters2014 <- c(
 poorRenterjerusalem2014         %>% nrow,
 poorRenterAshdod2014            %>% nrow,
 poorRenterHaifa2014             %>% nrow,
 poorRenterHerzliyya2014         %>% nrow,
 poorRenterHolon2014             %>% nrow,
 poorRenterLod2014               %>% nrow,
 poorRenterHadera2014            %>% nrow,
 poorRenterAshqelon2014          %>% nrow,
 poorRenterNettanya2014          %>% nrow,
 poorRenterRehovot2014           %>% nrow,
 poorRenterRamla2014             %>% nrow,
 poorRenterRaannana2014          %>% nrow,
 poorRentertlv2014               %>% nrow,
 poorRenterbene2014              %>% nrow,
 poorRenterbat2014               %>% nrow,
 poorRenterkefar2014             %>% nrow,
 poorRenterpetah2014             %>% nrow,
 poorRenterrishon2014            %>% nrow,
 poorRenterramat2014             %>% nrow,
 poorRenterbeer2014              %>% nrow,
 poorRentermodiin2014            %>% nrow,
 poorRenterbetshemesh2014        %>% nrow,
 poorRenternahariyya2014         %>% nrow,
 poorRenterqiryatatta2014        %>% nrow,
 poorRentergivatayim2014         %>% nrow )


poorRenterList  <<-as.data.frame(cbind(list   = cityOrder,
                                "2004"  = countPoorRenters2004,
                                "2005"  = countPoorRenters2005,
                                "2006"  = countPoorRenters2006,
                                "2007"  = countPoorRenters2007,
                                "2008"  = countPoorRenters2008,
                                "2009"  = countPoorRenters2009,
                                "2010"  = countPoorRenters2010,
                                "2011"  = countPoorRenters2011,
                                "2012"  = countPoorRenters2012,
                                "2013"  = countPoorRenters2013,
                                "2014"  = countPoorRenters2014))


richRenterList  <<-as.data.frame(cbind(list   = cityOrder,
                                "2004"  = countRichRenters2004,
                                "2005"  = countRichRenters2005,
                                "2006"  = countRichRenters2006,
                                "2007"  = countRichRenters2007,
                                "2008"  = countRichRenters2008,
                                "2009"  = countRichRenters2009,
                                "2010"  = countRichRenters2010,
                                "2011"  = countRichRenters2011,
                                "2012"  = countRichRenters2012,
                                "2013"  = countRichRenters2013,
                                "2014"  = countRichRenters2014))



        return("split cityYYYY  by owner/renter")
    } else {
        return("not implemented, error: 990ra9sldoar")
    }
}

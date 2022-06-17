library(dplyr)
library(lubridate)

#종관관측소 지점코드 포함
me.index <-read.csv("D:\\EUMC\\질병관리청\\폭염연구\\세미나자료\\TS_seminar\\data\\기상자료\\기상자료_종관관측지점.csv",fileEncoding = "euc-kr")
me.index2<-subset(me.index,포함유무=="TRUE") %>% dplyr:: select(시도,지점코드,geocode)

setwd("D:\\EUMC\\데이터관리\\기상청\\기상자료\\시간별자료")

list.files()

wh2001<-read.csv("wea_hourly_2001.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2002<-read.csv("wea_hourly_2002.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2003<-read.csv("wea_hourly_2003.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2004<-read.csv("wea_hourly_2004.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2005<-read.csv("wea_hourly_2005.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2006<-read.csv("wea_hourly_2006.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2007<-read.csv("wea_hourly_2007.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2008<-read.csv("wea_hourly_2008.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2009<-read.csv("wea_hourly_2009.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2010<-read.csv("wea_hourly_2010.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2011<-read.csv("wea_hourly_2011.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2012<-read.csv("wea_hourly_2012.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2013<-read.csv("wea_hourly_2013.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2014<-read.csv("wea_hourly_2014.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2015<-read.csv("wea_hourly_2015.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2016<-read.csv("wea_hourly_2016.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2017<-read.csv("wea_hourly_2017.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2018<-read.csv("wea_hourly_2018.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2019<-read.csv("wea_hourly_2019.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2020<-read.csv("wea_hourly_2020.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)
wh2021<-read.csv("wea_hourly_2021.csv",fileEncoding="euc-kr")%>% dplyr::select(지점,지점명,일시,기온..C.) %>% filter(지점 %in% me.index2$지점코드)

wh<-rbind(wh2001,wh2002,wh2003,wh2004,wh2005,
          wh2006,wh2007,wh2008,wh2009,wh2010,
          wh2011,wh2012,wh2013,wh2014,wh2015,
          wh2016,wh2017,wh2018,wh2019,wh2020,wh2021)

names(wh)=c("code","codename","datetime","temp")
head(wh)

wh$dateT=as.numeric(substr(wh$datetime,12,13))
head(wh,25)

ind01<-subset(me.index2,시도=="서울")$지점코드
ind02<-subset(me.index2,시도=="부산")$지점코드
ind03<-subset(me.index2,시도=="대구")$지점코드
ind04<-subset(me.index2,시도=="인천")$지점코드
ind05<-subset(me.index2,시도=="광주")$지점코드
ind06<-subset(me.index2,시도=="대전")$지점코드
ind07<-subset(me.index2,시도=="울산")$지점코드
ind08<-subset(me.index2,시도=="세종")$지점코드
ind09<-subset(me.index2,시도=="경기도")$지점코드
ind10<-subset(me.index2,시도=="강원도")$지점코드
ind11<-subset(me.index2,시도=="충청북도")$지점코드
ind12<-subset(me.index2,시도=="충청남도")$지점코드
ind13<-subset(me.index2,시도=="전라북도")$지점코드
ind14<-subset(me.index2,시도=="전라남도")$지점코드
ind15<-subset(me.index2,시도=="경상북도")$지점코드
ind16<-subset(me.index2,시도=="경상남도")$지점코드
ind17<-subset(me.index2,시도=="제주도")$지점코드

#지점별 기온 -> 시도별 기온
wh_s01<-wh %>% filter(code %in% c(ind01)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s02<-wh %>% filter(code %in% c(ind02)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s03<-wh %>% filter(code %in% c(ind03)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s04<-wh %>% filter(code %in% c(ind04)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s05<-wh %>% filter(code %in% c(ind05)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s06<-wh %>% filter(code %in% c(ind06)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s07<-wh %>% filter(code %in% c(ind07)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s08<-wh %>% filter(code %in% c(ind08)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s09<-wh %>% filter(code %in% c(ind09)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s10<-wh %>% filter(code %in% c(ind10)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s11<-wh %>% filter(code %in% c(ind11)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s12<-wh %>% filter(code %in% c(ind12)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s13<-wh %>% filter(code %in% c(ind13)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s14<-wh %>% filter(code %in% c(ind14)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s15<-wh %>% filter(code %in% c(ind15)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s16<-wh %>% filter(code %in% c(ind16)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))
wh_s17<-wh %>% filter(code %in% c(ind17)) %>% mutate(area=substr(codename,1,2)) %>% group_by(datetime,dateT) %>% dplyr::summarise(temp=mean(temp))

#시간별 자료 열대야 정의 (당일 18시 ~ 익일 9시까지 )
#Tropical index -> TF
#Tropical nights-> TN
wh_s01$TF=with(wh_s01,ifelse(dateT>=18 | dateT<=9,1,0));wh_s01$TN=with(wh_s01,ifelse(TF==1 & temp>=25,1,0))
wh_s02$TF=with(wh_s02,ifelse(dateT>=18 | dateT<=9,1,0));wh_s02$TN=with(wh_s02,ifelse(TF==1 & temp>=25,1,0))
wh_s03$TF=with(wh_s03,ifelse(dateT>=18 | dateT<=9,1,0));wh_s03$TN=with(wh_s03,ifelse(TF==1 & temp>=25,1,0))
wh_s04$TF=with(wh_s04,ifelse(dateT>=18 | dateT<=9,1,0));wh_s04$TN=with(wh_s04,ifelse(TF==1 & temp>=25,1,0))
wh_s05$TF=with(wh_s05,ifelse(dateT>=18 | dateT<=9,1,0));wh_s05$TN=with(wh_s05,ifelse(TF==1 & temp>=25,1,0))
wh_s06$TF=with(wh_s06,ifelse(dateT>=18 | dateT<=9,1,0));wh_s06$TN=with(wh_s06,ifelse(TF==1 & temp>=25,1,0))
wh_s07$TF=with(wh_s07,ifelse(dateT>=18 | dateT<=9,1,0));wh_s07$TN=with(wh_s07,ifelse(TF==1 & temp>=25,1,0))
wh_s08$TF=with(wh_s08,ifelse(dateT>=18 | dateT<=9,1,0));wh_s08$TN=with(wh_s08,ifelse(TF==1 & temp>=25,1,0))
wh_s09$TF=with(wh_s09,ifelse(dateT>=18 | dateT<=9,1,0));wh_s09$TN=with(wh_s09,ifelse(TF==1 & temp>=25,1,0))
wh_s10$TF=with(wh_s10,ifelse(dateT>=18 | dateT<=9,1,0));wh_s10$TN=with(wh_s10,ifelse(TF==1 & temp>=25,1,0))
wh_s11$TF=with(wh_s11,ifelse(dateT>=18 | dateT<=9,1,0));wh_s11$TN=with(wh_s11,ifelse(TF==1 & temp>=25,1,0))
wh_s12$TF=with(wh_s12,ifelse(dateT>=18 | dateT<=9,1,0));wh_s12$TN=with(wh_s12,ifelse(TF==1 & temp>=25,1,0))
wh_s13$TF=with(wh_s13,ifelse(dateT>=18 | dateT<=9,1,0));wh_s13$TN=with(wh_s13,ifelse(TF==1 & temp>=25,1,0))
wh_s14$TF=with(wh_s14,ifelse(dateT>=18 | dateT<=9,1,0));wh_s14$TN=with(wh_s14,ifelse(TF==1 & temp>=25,1,0))
wh_s15$TF=with(wh_s15,ifelse(dateT>=18 | dateT<=9,1,0));wh_s15$TN=with(wh_s15,ifelse(TF==1 & temp>=25,1,0))
wh_s16$TF=with(wh_s16,ifelse(dateT>=18 | dateT<=9,1,0));wh_s16$TN=with(wh_s16,ifelse(TF==1 & temp>=25,1,0))
wh_s17$TF=with(wh_s17,ifelse(dateT>=18 | dateT<=9,1,0));wh_s17$TN=with(wh_s17,ifelse(TF==1 & temp>=25,1,0))

wh_s01$date=as.Date(substr(wh_s01$datetime,1,10))
wh_s02$date=as.Date(substr(wh_s02$datetime,1,10))
wh_s03$date=as.Date(substr(wh_s03$datetime,1,10))
wh_s04$date=as.Date(substr(wh_s04$datetime,1,10))
wh_s05$date=as.Date(substr(wh_s05$datetime,1,10))
wh_s06$date=as.Date(substr(wh_s06$datetime,1,10))
wh_s07$date=as.Date(substr(wh_s07$datetime,1,10))
wh_s08$date=as.Date(substr(wh_s08$datetime,1,10))
wh_s09$date=as.Date(substr(wh_s09$datetime,1,10))
wh_s10$date=as.Date(substr(wh_s10$datetime,1,10))
wh_s11$date=as.Date(substr(wh_s11$datetime,1,10))
wh_s12$date=as.Date(substr(wh_s12$datetime,1,10))
wh_s13$date=as.Date(substr(wh_s13$datetime,1,10))
wh_s14$date=as.Date(substr(wh_s14$datetime,1,10))
wh_s15$date=as.Date(substr(wh_s15$datetime,1,10))
wh_s16$date=as.Date(substr(wh_s16$datetime,1,10))
wh_s17$date=as.Date(substr(wh_s17$datetime,1,10))

#열대야 시점의 날짜들 ->date2 (밤 18시 부터 익일 9시까지 )
wh_s01$date2=with(wh_s01,ifelse(dateT<=9,date-1,as.Date(date)));wh_s01$date2=as.Date(wh_s01$date2)
wh_s02$date2=with(wh_s02,ifelse(dateT<=9,date-1,as.Date(date)));wh_s02$date2=as.Date(wh_s02$date2)
wh_s03$date2=with(wh_s03,ifelse(dateT<=9,date-1,as.Date(date)));wh_s03$date2=as.Date(wh_s03$date2)
wh_s04$date2=with(wh_s04,ifelse(dateT<=9,date-1,as.Date(date)));wh_s04$date2=as.Date(wh_s04$date2)
wh_s05$date2=with(wh_s05,ifelse(dateT<=9,date-1,as.Date(date)));wh_s05$date2=as.Date(wh_s05$date2)
wh_s06$date2=with(wh_s06,ifelse(dateT<=9,date-1,as.Date(date)));wh_s06$date2=as.Date(wh_s06$date2)
wh_s07$date2=with(wh_s07,ifelse(dateT<=9,date-1,as.Date(date)));wh_s07$date2=as.Date(wh_s07$date2)
wh_s08$date2=with(wh_s08,ifelse(dateT<=9,date-1,as.Date(date)));wh_s08$date2=as.Date(wh_s08$date2)
wh_s09$date2=with(wh_s09,ifelse(dateT<=9,date-1,as.Date(date)));wh_s09$date2=as.Date(wh_s09$date2)
wh_s10$date2=with(wh_s10,ifelse(dateT<=9,date-1,as.Date(date)));wh_s10$date2=as.Date(wh_s10$date2)
wh_s11$date2=with(wh_s11,ifelse(dateT<=9,date-1,as.Date(date)));wh_s11$date2=as.Date(wh_s11$date2)
wh_s12$date2=with(wh_s12,ifelse(dateT<=9,date-1,as.Date(date)));wh_s12$date2=as.Date(wh_s12$date2)
wh_s13$date2=with(wh_s13,ifelse(dateT<=9,date-1,as.Date(date)));wh_s13$date2=as.Date(wh_s13$date2)
wh_s14$date2=with(wh_s14,ifelse(dateT<=9,date-1,as.Date(date)));wh_s14$date2=as.Date(wh_s14$date2)
wh_s15$date2=with(wh_s15,ifelse(dateT<=9,date-1,as.Date(date)));wh_s15$date2=as.Date(wh_s15$date2)
wh_s16$date2=with(wh_s16,ifelse(dateT<=9,date-1,as.Date(date)));wh_s16$date2=as.Date(wh_s16$date2)
wh_s17$date2=with(wh_s17,ifelse(dateT<=9,date-1,as.Date(date)));wh_s17$date2=as.Date(wh_s17$date2)

#열대야, 일별 기온 편차 
sido_tn_sd<-function(data,area){
  r1<-data %>% filter(TF==1) %>% group_by(date2) %>% 
    dplyr:: summarise(TN_cnt=sum(TN),
                      TN    =ifelse(TN_cnt>=1,1,0),
                      TN_len=length(date2)) %>% mutate(area=area) %>% 
    filter(date2>="2001-01-01")
  
  #일별 기온 표준편차 
  r2<-data %>% group_by(date) %>% dplyr::summarise(temp_SD=sd(temp))
  
  cbind(r1,temp_SD=r2$temp_SD) %>% dplyr::select(area,date2,TN_cnt:TN_len,temp_SD)
  
}
tn_sd_s01<-sido_tn_sd(wh_s01,"서울")
tn_sd_s02<-sido_tn_sd(wh_s02,"부산")
tn_sd_s03<-sido_tn_sd(wh_s03,"대구")
tn_sd_s04<-sido_tn_sd(wh_s04,"인천")
tn_sd_s05<-sido_tn_sd(wh_s05,"광주")
tn_sd_s06<-sido_tn_sd(wh_s06,"대전")
tn_sd_s07<-sido_tn_sd(wh_s07,"울산")
tn_sd_s08<-sido_tn_sd(wh_s08,"세종")
tn_sd_s09<-sido_tn_sd(wh_s09,"경기")
tn_sd_s10<-sido_tn_sd(wh_s10,"강원")
tn_sd_s11<-sido_tn_sd(wh_s11,"충북")
tn_sd_s12<-sido_tn_sd(wh_s12,"충남")
tn_sd_s13<-sido_tn_sd(wh_s13,"전북")
tn_sd_s14<-sido_tn_sd(wh_s14,"전남")
tn_sd_s15<-sido_tn_sd(wh_s15,"경북")
tn_sd_s16<-sido_tn_sd(wh_s16,"경남")
tn_sd_s17<-sido_tn_sd(wh_s17,"제주")

tn_sd<-rbind(tn_sd_s01,tn_sd_s02,tn_sd_s03,tn_sd_s04,
      tn_sd_s05,tn_sd_s06,tn_sd_s07,tn_sd_s08,
      tn_sd_s09,tn_sd_s10,tn_sd_s11,tn_sd_s12,
      tn_sd_s13,tn_sd_s14,tn_sd_s15,tn_sd_s16,tn_sd_s17)

#기상청 시간 자료 이용, 시도별 일별 열대야, 일별 기온 편차 자료
setwd("D:\\EUMC\\데이터관리\\기상청")
write.csv(tn_sd,file="sido_tropical_night_dailyT_SD.csv",row.names=F,na="")

library(dplyr)
library(lubridate)
library(readxl)
library(metafor)
library(mgcv)
#종관관측소 지점코드 포함

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
setwd("D:\\EUMC\\질병관리청\\폭염후속연구\\data\\정리자료")
mdis_dat<-read.csv("통계청_5세미만사망_2015_2020_revise0620.csv",fileEncoding = "euc-kr")


mdis_dat$ddate=as.Date(mdis_dat$ddate)
mdis_dat$area=factor(mdis_dat$area,levels=unique(mdis_dat$area))

table(mdis_dat$area)
#지역별 폭염 인덱스 만들기 

z<-subset(mdis_dat,KOR_SIDO=="서울")

summary(z$maxAT)
quantile(z$maxAT,p=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9),na.rm=T)
quantile(z$maxAT,p=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95),na.rm=T)

#절대적 기준: 폭염여부는 일최고체감온도 33℃ 이상인 경우(기상청 기준)
#여름철 체감온도 산출식 이용

#지역별로 일최고체감기온 자료 이용, 3일 이전까지 단일 지연자료 만들어주기 
#뒤에 월은 5~9월, 6~9월, 6~8월, 등 몇으로 할지, 한번 통일해야할듯 

#폭염, 일최고체감기온으로 산출, 분위수로 산출 
sub_func<-function(region,smonth,emonth){
  d<-subset(mdis_dat,KOR_SIDO==region) %>% mutate(
    
    #일최고기온
    maxT    =maxtemp,       
    maxT_s0 =lag(maxtemp,0),maxT_s1 =lag(maxtemp,1),maxT_s2 =lag(maxtemp,2),maxT_s3 =lag(maxtemp,3),
    maxT_s4 =lag(maxtemp,4),maxT_s5 =lag(maxtemp,5),maxT_s6 =lag(maxtemp,6),maxT_s7 =lag(maxtemp,7),
    
    #일최고체감기온
    maxAT_s0 =lag(maxAT,0),maxAT_s1 =lag(maxAT,1),maxAT_s2 =lag(maxAT,2),maxAT_s3 =lag(maxAT,3),
    maxAT_s4 =lag(maxAT,4),maxAT_s5 =lag(maxAT,5),maxAT_s6 =lag(maxAT,6),maxAT_s7 =lag(maxAT,7),
    
    #열지수
    HI_c_s0 =lag(HI_c,0),HI_c_s1 =lag(HI_c,1),HI_c_s2 =lag(HI_c,2),HI_c_s3 =lag(HI_c,3),
    HI_c_s4 =lag(HI_c,4),HI_c_s5 =lag(HI_c,5),HI_c_s6 =lag(HI_c,6),HI_c_s7 =lag(HI_c,7),
    
    #열대야
    TN_s0 =lag(TN,0),TN_s1 =lag(TN,1),TN_s2 =lag(TN,2),TN_s3 =lag(TN,3),
    TN_s4 =lag(TN,4),TN_s5 =lag(TN,5),TN_s6 =lag(TN,6),TN_s7 =lag(TN,7),
    
    #일교차
    DT_s0 =lag(DT,0),DT_s1 =lag(DT,1),DT_s2 =lag(DT,2),DT_s3 =lag(DT,3),
    DT_s4 =lag(DT,4),DT_s5 =lag(DT,5),DT_s6 =lag(DT,6),DT_s7 =lag(DT,7),
    
    #일일 기온편차
    temp_SD_s0 =lag(temp_SD,0),temp_SD_s1 =lag(temp_SD,1),temp_SD_s2 =lag(temp_SD,2),temp_SD_s3 =lag(temp_SD,3),
    temp_SD_s4 =lag(temp_SD,4),temp_SD_s5 =lag(temp_SD,5),temp_SD_s6 =lag(temp_SD,6),temp_SD_s7 =lag(temp_SD,7),
    
    #일 평균 습도
    meanhumi_s0 =lag(meanhumi,0),meanhumi_s1 =lag(meanhumi,1),meanhumi_s2 =lag(meanhumi,2),meanhumi_s3 =lag(meanhumi,3),
    meanhumi_s4 =lag(meanhumi,4),meanhumi_s5 =lag(meanhumi,5),meanhumi_s6 =lag(meanhumi,6),meanhumi_s7 =lag(meanhumi,7),
    
    #일 평균 이슬점
    meandew_s0 =lag(meandew,0),meandew_s1 =lag(meandew,1),meandew_s2 =lag(meandew,2),meandew_s3 =lag(meandew,3),
    meandew_s4 =lag(meandew,4),meandew_s5 =lag(meandew,5),meandew_s6 =lag(meandew,6),meandew_s7 =lag(meandew,7))
  
  #Moving average - maximum temperature
  d$maxT_m1=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxT,maxT_s1)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxT,maxT_s1),1,mean))
  d$maxT_m2=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxT,maxT_s2)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxT,maxT_s2),1,mean))
  d$maxT_m3=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxT,maxT_s3)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxT,maxT_s3),1,mean))
  d$maxT_m4=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxT,maxT_s4)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxT,maxT_s4),1,mean))
  d$maxT_m5=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxT,maxT_s5)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxT,maxT_s5),1,mean))
  d$maxT_m6=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxT,maxT_s6)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxT,maxT_s6),1,mean))
  d$maxT_m7=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxT,maxT_s7)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxT,maxT_s7),1,mean))
  
  #Moving average - maximum apparent temperature
  d$maxAT_m1=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxAT,maxAT_s1)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxAT,maxAT_s1),1,mean))
  d$maxAT_m2=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxAT,maxAT_s2)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxAT,maxAT_s2),1,mean))
  d$maxAT_m3=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxAT,maxAT_s3)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxAT,maxAT_s3),1,mean))
  d$maxAT_m4=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxAT,maxAT_s4)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxAT,maxAT_s4),1,mean))
  d$maxAT_m5=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxAT,maxAT_s5)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxAT,maxAT_s5),1,mean))
  d$maxAT_m6=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxAT,maxAT_s6)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxAT,maxAT_s6),1,mean))
  d$maxAT_m7=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(maxAT,maxAT_s7)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(maxAT,maxAT_s7),1,mean))
  
  #Moving average - heat index
  d$HI_c_m1=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(HI_c,HI_c_s1)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(HI_c,HI_c_s1),1,mean))
  d$HI_c_m2=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(HI_c,HI_c_s2)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(HI_c,HI_c_s2),1,mean))
  d$HI_c_m3=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(HI_c,HI_c_s3)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(HI_c,HI_c_s3),1,mean))
  d$HI_c_m4=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(HI_c,HI_c_s4)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(HI_c,HI_c_s4),1,mean))
  d$HI_c_m5=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(HI_c,HI_c_s5)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(HI_c,HI_c_s5),1,mean))
  d$HI_c_m6=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(HI_c,HI_c_s6)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(HI_c,HI_c_s6),1,mean))
  d$HI_c_m7=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(HI_c,HI_c_s7)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(HI_c,HI_c_s7),1,mean))
  
  #Moving average - diurnal temperature
  d$DT_m1=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(DT,DT_s1)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(DT,DT_s1),1,mean))
  d$DT_m2=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(DT,DT_s2)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(DT,DT_s2),1,mean))
  d$DT_m3=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(DT,DT_s3)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(DT,DT_s3),1,mean))
  d$DT_m4=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(DT,DT_s4)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(DT,DT_s4),1,mean))
  d$DT_m5=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(DT,DT_s5)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(DT,DT_s5),1,mean))
  d$DT_m6=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(DT,DT_s6)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(DT,DT_s6),1,mean))
  d$DT_m7=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(DT,DT_s7)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(DT,DT_s7),1,mean))
  
  #Moving average - temperautre SD
  d$temp_SD_m1=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(temp_SD,temp_SD_s1)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(temp_SD,temp_SD_s1),1,mean))
  d$temp_SD_m2=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(temp_SD,temp_SD_s2)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(temp_SD,temp_SD_s2),1,mean))
  d$temp_SD_m3=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(temp_SD,temp_SD_s3)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(temp_SD,temp_SD_s3),1,mean))
  d$temp_SD_m4=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(temp_SD,temp_SD_s4)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(temp_SD,temp_SD_s4),1,mean))
  d$temp_SD_m5=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(temp_SD,temp_SD_s5)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(temp_SD,temp_SD_s5),1,mean))
  d$temp_SD_m6=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(temp_SD,temp_SD_s6)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(temp_SD,temp_SD_s6),1,mean))
  d$temp_SD_m7=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(temp_SD,temp_SD_s7)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(temp_SD,temp_SD_s7),1,mean))
  
  #Moving average - humidity
  d$meanhumi_m1=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meanhumi,meanhumi_s1)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meanhumi,meanhumi_s1),1,mean))
  d$meanhumi_m2=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meanhumi,meanhumi_s2)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meanhumi,meanhumi_s2),1,mean))
  d$meanhumi_m3=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meanhumi,meanhumi_s3)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meanhumi,meanhumi_s3),1,mean))
  d$meanhumi_m4=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meanhumi,meanhumi_s4)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meanhumi,meanhumi_s4),1,mean))
  d$meanhumi_m5=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meanhumi,meanhumi_s5)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meanhumi,meanhumi_s5),1,mean))
  d$meanhumi_m6=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meanhumi,meanhumi_s6)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meanhumi,meanhumi_s6),1,mean))
  d$meanhumi_m7=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meanhumi,meanhumi_s7)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meanhumi,meanhumi_s7),1,mean))
  
  #Moving average - dew point temperautre
  d$meandew_m1=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meandew,meandew_s1)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meandew,meandew_s1),1,mean))
  d$meandew_m2=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meandew,meandew_s2)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meandew,meandew_s2),1,mean))
  d$meandew_m3=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meandew,meandew_s3)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meandew,meandew_s3),1,mean))
  d$meandew_m4=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meandew,meandew_s4)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meandew,meandew_s4),1,mean))
  d$meandew_m5=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meandew,meandew_s5)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meandew,meandew_s5),1,mean))
  d$meandew_m6=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meandew,meandew_s6)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meandew,meandew_s6),1,mean))
  d$meandew_m7=ifelse(apply(ifelse(is.na(d %>% dplyr ::select(meandew,meandew_s7)),0,1),1,sum)==0,NA,apply(d %>% dplyr ::select(meandew,meandew_s7),1,mean))
  
  #폭염, 일최고기온, 33도 기준, 2일 지속 
  d$hwD2_maxT_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=33,1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_s1=lag(d$hwD2_maxT_s0,1);d$hwD2_maxT_s2=lag(d$hwD2_maxT_s0,2)
  d$hwD2_maxT_s3=lag(d$hwD2_maxT_s0,3);d$hwD2_maxT_s4=lag(d$hwD2_maxT_s0,4)
  d$hwD2_maxT_s5=lag(d$hwD2_maxT_s0,5);d$hwD2_maxT_s6=lag(d$hwD2_maxT_s0,6);d$hwD2_maxT_s7=lag(d$hwD2_maxT_s0,7)
  
  #폭염, 일최고기온, 33도 기준, 3일 지속 
  d$hwD3_maxT_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=33,1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_s1=lag(d$hwD3_maxT_s0,1);d$hwD3_maxT_s2=lag(d$hwD3_maxT_s0,2)
  d$hwD3_maxT_s3=lag(d$hwD3_maxT_s0,3);d$hwD3_maxT_s4=lag(d$hwD3_maxT_s0,4)
  d$hwD3_maxT_s5=lag(d$hwD3_maxT_s0,5);d$hwD3_maxT_s6=lag(d$hwD3_maxT_s0,6);d$hwD3_maxT_s7=lag(d$hwD3_maxT_s0,7)
  
  #폭염, 일최고체감기온, 33도 기준, 2일 지속
  d$hwD2_maxAT_s0=ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=33,1,0),1,sum)>=2,1,0)
  d$hwD2_maxAT_s1=lag(d$hwD2_maxAT_s0,1);d$hwD2_maxAT_s2=lag(d$hwD2_maxAT_s0,2)
  d$hwD2_maxAT_s3=lag(d$hwD2_maxAT_s0,3);d$hwD2_maxAT_s4=lag(d$hwD2_maxAT_s0,4)
  d$hwD2_maxAT_s5=lag(d$hwD2_maxAT_s0,5);d$hwD2_maxAT_s6=lag(d$hwD2_maxAT_s0,6);d$hwD2_maxAT_s7=lag(d$hwD2_maxAT_s0,7)
  
  #폭염, 일최고체감기온, 33도 기준, 3일 지속 
  d$hwD3_maxAT_s0=ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=33,1,0),1,sum)>=3,1,0)
  d$hwD3_maxAT_s1=lag(d$hwD3_maxAT_s0,1);d$hwD3_maxAT_s2=lag(d$hwD3_maxAT_s0,2)
  d$hwD3_maxAT_s3=lag(d$hwD3_maxAT_s0,3);d$hwD3_maxAT_s4=lag(d$hwD3_maxAT_s0,4)
  d$hwD3_maxAT_s5=lag(d$hwD3_maxAT_s0,5);d$hwD3_maxAT_s6=lag(d$hwD3_maxAT_s0,6);d$hwD3_maxAT_s7=lag(d$hwD3_maxAT_s0,7)
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------------------------------------------------------------#
  
  h<-subset(d,month %in% c(smonth:emonth))
  
  #폭염, 일최고기온, 70th percentile 산출 2일 지속 
  d$hwD2_maxT_p70_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.70,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p70_s1=lag(d$hwD2_maxT_p70_s0,1);d$hwD2_maxT_p70_s2=lag(d$hwD2_maxT_p70_s0,2)
  d$hwD2_maxT_p70_s3=lag(d$hwD2_maxT_p70_s0,3);d$hwD2_maxT_p70_s4=lag(d$hwD2_maxT_p70_s0,4)
  d$hwD2_maxT_p70_s5=lag(d$hwD2_maxT_p70_s0,5);d$hwD2_maxT_p70_s6=lag(d$hwD2_maxT_p70_s0,6);d$hwD2_maxT_p70_s7=lag(d$hwD2_maxT_p70_s0,7)
  
  #폭염, 일최고기온, 75th percentile 산출 2일 지속 
  d$hwD2_maxT_p75_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.75,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p75_s1=lag(d$hwD2_maxT_p75_s0,1);d$hwD2_maxT_p75_s2=lag(d$hwD2_maxT_p75_s0,2)
  d$hwD2_maxT_p75_s3=lag(d$hwD2_maxT_p75_s0,3);d$hwD2_maxT_p75_s4=lag(d$hwD2_maxT_p75_s0,4)
  d$hwD2_maxT_p75_s5=lag(d$hwD2_maxT_p75_s0,5);d$hwD2_maxT_p75_s6=lag(d$hwD2_maxT_p75_s0,6);d$hwD2_maxT_p75_s7=lag(d$hwD2_maxT_p75_s0,7)
  
  #폭염, 일최고기온, 80th percentile 산출 2일 지속 
  d$hwD2_maxT_p80_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.80,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p80_s1=lag(d$hwD2_maxT_p80_s0,1);d$hwD2_maxT_p80_s2=lag(d$hwD2_maxT_p80_s0,2)
  d$hwD2_maxT_p80_s3=lag(d$hwD2_maxT_p80_s0,3);d$hwD2_maxT_p80_s4=lag(d$hwD2_maxT_p80_s0,4)
  d$hwD2_maxT_p80_s5=lag(d$hwD2_maxT_p80_s0,5);d$hwD2_maxT_p80_s6=lag(d$hwD2_maxT_p80_s0,6);d$hwD2_maxT_p80_s7=lag(d$hwD2_maxT_p80_s0,7)
  
  #폭염, 일최고기온, 85th percentile 산출 2일 지속 
  d$hwD2_maxT_p85_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.85,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p85_s1=lag(d$hwD2_maxT_p85_s0,1);d$hwD2_maxT_p85_s2=lag(d$hwD2_maxT_p85_s0,2)
  d$hwD2_maxT_p85_s3=lag(d$hwD2_maxT_p85_s0,3);d$hwD2_maxT_p85_s4=lag(d$hwD2_maxT_p85_s0,4)
  d$hwD2_maxT_p85_s5=lag(d$hwD2_maxT_p85_s0,5);d$hwD2_maxT_p85_s6=lag(d$hwD2_maxT_p85_s0,6);d$hwD2_maxT_p85_s7=lag(d$hwD2_maxT_p85_s0,7)
  
  #폭염, 일최고기온, 90th percentile 산출 2일 지속 
  d$hwD2_maxT_p90_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.90,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p90_s1=lag(d$hwD2_maxT_p90_s0,1);d$hwD2_maxT_p90_s2=lag(d$hwD2_maxT_p90_s0,2)
  d$hwD2_maxT_p90_s3=lag(d$hwD2_maxT_p90_s0,3);d$hwD2_maxT_p90_s4=lag(d$hwD2_maxT_p90_s0,4)
  d$hwD2_maxT_p90_s5=lag(d$hwD2_maxT_p90_s0,5);d$hwD2_maxT_p90_s6=lag(d$hwD2_maxT_p90_s0,6);d$hwD2_maxT_p90_s7=lag(d$hwD2_maxT_p90_s0,7)
  
  #폭염, 일최고기온, 91th percentile 산출 2일 지속
  d$hwD2_maxT_p91_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.91,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p91_s1=lag(d$hwD2_maxT_p91_s0,1);d$hwD2_maxT_p91_s2=lag(d$hwD2_maxT_p91_s0,2)
  d$hwD2_maxT_p91_s3=lag(d$hwD2_maxT_p91_s0,3);d$hwD2_maxT_p91_s4=lag(d$hwD2_maxT_p91_s0,4)
  d$hwD2_maxT_p91_s5=lag(d$hwD2_maxT_p91_s0,5);d$hwD2_maxT_p91_s6=lag(d$hwD2_maxT_p91_s0,6);d$hwD2_maxT_p91_s7=lag(d$hwD2_maxT_p91_s0,7)
  
  #폭염, 일최고기온, 92th percentile 산출 2일 지속 
  d$hwD2_maxT_p92_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.92,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p92_s1=lag(d$hwD2_maxT_p92_s0,1);d$hwD2_maxT_p92_s2=lag(d$hwD2_maxT_p92_s0,2)
  d$hwD2_maxT_p92_s3=lag(d$hwD2_maxT_p92_s0,3);d$hwD2_maxT_p92_s4=lag(d$hwD2_maxT_p92_s0,4)
  d$hwD2_maxT_p92_s5=lag(d$hwD2_maxT_p92_s0,5);d$hwD2_maxT_p92_s6=lag(d$hwD2_maxT_p92_s0,6);d$hwD2_maxT_p92_s7=lag(d$hwD2_maxT_p92_s0,7)
  
  #폭염, 일최고기온, 93th percentile 산출 2일 지속  
  d$hwD2_maxT_p93_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.93,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p93_s1=lag(d$hwD2_maxT_p93_s0,1);d$hwD2_maxT_p93_s2=lag(d$hwD2_maxT_p93_s0,2)
  d$hwD2_maxT_p93_s3=lag(d$hwD2_maxT_p93_s0,3);d$hwD2_maxT_p93_s4=lag(d$hwD2_maxT_p93_s0,4)
  d$hwD2_maxT_p93_s5=lag(d$hwD2_maxT_p93_s0,5);d$hwD2_maxT_p93_s6=lag(d$hwD2_maxT_p93_s0,6);d$hwD2_maxT_p93_s7=lag(d$hwD2_maxT_p93_s0,7)
  
  #폭염, 일최고기온, 94th percentile 산출 2일 지속 
  d$hwD2_maxT_p94_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.94,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p94_s1=lag(d$hwD2_maxT_p94_s0,1);d$hwD2_maxT_p94_s2=lag(d$hwD2_maxT_p94_s0,2)
  d$hwD2_maxT_p94_s3=lag(d$hwD2_maxT_p94_s0,3);d$hwD2_maxT_p94_s4=lag(d$hwD2_maxT_p94_s0,4)
  d$hwD2_maxT_p94_s5=lag(d$hwD2_maxT_p94_s0,5);d$hwD2_maxT_p94_s6=lag(d$hwD2_maxT_p94_s0,6);d$hwD2_maxT_p94_s7=lag(d$hwD2_maxT_p94_s0,7)
  
  #폭염, 일최고기온, 95th percentile 산출 2일 지속 
  d$hwD2_maxT_p95_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1)>=quantile(h$maxT,0.95,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxT_p95_s1=lag(d$hwD2_maxT_p95_s0,1);d$hwD2_maxT_p95_s2=lag(d$hwD2_maxT_p95_s0,2)
  d$hwD2_maxT_p95_s3=lag(d$hwD2_maxT_p95_s0,3);d$hwD2_maxT_p95_s4=lag(d$hwD2_maxT_p95_s0,4)
  d$hwD2_maxT_p95_s5=lag(d$hwD2_maxT_p95_s0,5);d$hwD2_maxT_p95_s6=lag(d$hwD2_maxT_p95_s0,6);d$hwD2_maxT_p95_s7=lag(d$hwD2_maxT_p95_s0,7)
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------------------------------------------------------------#
  #폭염, 일최고기온, 70th percentile 산출 3일 지속 
  d$hwD3_maxT_p70_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.70,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p70_s1=lag(d$hwD3_maxT_p70_s0,1);d$hwD3_maxT_p70_s2=lag(d$hwD3_maxT_p70_s0,2)
  d$hwD3_maxT_p70_s3=lag(d$hwD3_maxT_p70_s0,3);d$hwD3_maxT_p70_s4=lag(d$hwD3_maxT_p70_s0,4)
  d$hwD3_maxT_p70_s5=lag(d$hwD3_maxT_p70_s0,5);d$hwD3_maxT_p70_s6=lag(d$hwD3_maxT_p70_s0,6);d$hwD3_maxT_p70_s7=lag(d$hwD3_maxT_p70_s0,7)
  
  #폭염, 일최고기온, 75th percentile 산출 3일 지속 
  d$hwD3_maxT_p75_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.75,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p75_s1=lag(d$hwD3_maxT_p75_s0,1);d$hwD3_maxT_p75_s2=lag(d$hwD3_maxT_p75_s0,2)
  d$hwD3_maxT_p75_s3=lag(d$hwD3_maxT_p75_s0,3);d$hwD3_maxT_p75_s4=lag(d$hwD3_maxT_p75_s0,4)
  d$hwD3_maxT_p75_s5=lag(d$hwD3_maxT_p75_s0,5);d$hwD3_maxT_p75_s6=lag(d$hwD3_maxT_p75_s0,6);d$hwD3_maxT_p75_s7=lag(d$hwD3_maxT_p75_s0,7)
  
  #폭염, 일최고기온, 80th percentile 산출 3일 지속 
  d$hwD3_maxT_p80_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.80,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p80_s1=lag(d$hwD3_maxT_p80_s0,1);d$hwD3_maxT_p80_s2=lag(d$hwD3_maxT_p80_s0,2)
  d$hwD3_maxT_p80_s3=lag(d$hwD3_maxT_p80_s0,3);d$hwD3_maxT_p80_s4=lag(d$hwD3_maxT_p80_s0,4)
  d$hwD3_maxT_p80_s5=lag(d$hwD3_maxT_p80_s0,5);d$hwD3_maxT_p80_s6=lag(d$hwD3_maxT_p80_s0,6);d$hwD3_maxT_p80_s7=lag(d$hwD3_maxT_p80_s0,7)
  
  #폭염, 일최고기온, 85th percentile 산출 3일 지속 
  d$hwD3_maxT_p85_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.85,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p85_s1=lag(d$hwD3_maxT_p85_s0,1);d$hwD3_maxT_p85_s2=lag(d$hwD3_maxT_p85_s0,2)
  d$hwD3_maxT_p85_s3=lag(d$hwD3_maxT_p85_s0,3);d$hwD3_maxT_p85_s4=lag(d$hwD3_maxT_p85_s0,4)
  d$hwD3_maxT_p85_s5=lag(d$hwD3_maxT_p85_s0,5);d$hwD3_maxT_p85_s6=lag(d$hwD3_maxT_p85_s0,6);d$hwD3_maxT_p85_s7=lag(d$hwD3_maxT_p85_s0,7)
  
  #폭염, 일최고기온, 90th percentile 산출 3일 지속 
  d$hwD3_maxT_p90_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.90,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p90_s1=lag(d$hwD3_maxT_p90_s0,1);d$hwD3_maxT_p90_s2=lag(d$hwD3_maxT_p90_s0,2)
  d$hwD3_maxT_p90_s3=lag(d$hwD3_maxT_p90_s0,3);d$hwD3_maxT_p90_s4=lag(d$hwD3_maxT_p90_s0,4)
  d$hwD3_maxT_p90_s5=lag(d$hwD3_maxT_p90_s0,5);d$hwD3_maxT_p90_s6=lag(d$hwD3_maxT_p90_s0,6);d$hwD3_maxT_p90_s7=lag(d$hwD3_maxT_p90_s0,7)
  
  #폭염, 일최고기온, 91th percentile 산출 3일 지속
  d$hwD3_maxT_p91_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.91,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p91_s1=lag(d$hwD3_maxT_p91_s0,1);d$hwD3_maxT_p91_s2=lag(d$hwD3_maxT_p91_s0,2)
  d$hwD3_maxT_p91_s3=lag(d$hwD3_maxT_p91_s0,3);d$hwD3_maxT_p91_s4=lag(d$hwD3_maxT_p91_s0,4)
  d$hwD3_maxT_p91_s5=lag(d$hwD3_maxT_p91_s0,5);d$hwD3_maxT_p91_s6=lag(d$hwD3_maxT_p91_s0,6);d$hwD3_maxT_p91_s7=lag(d$hwD3_maxT_p91_s0,7)
  
  #폭염, 일최고기온, 92th percentile 산출 3일 지속 
  d$hwD3_maxT_p92_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.92,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p92_s1=lag(d$hwD3_maxT_p92_s0,1);d$hwD3_maxT_p92_s2=lag(d$hwD3_maxT_p92_s0,2)
  d$hwD3_maxT_p92_s3=lag(d$hwD3_maxT_p92_s0,3);d$hwD3_maxT_p92_s4=lag(d$hwD3_maxT_p92_s0,4)
  d$hwD3_maxT_p92_s5=lag(d$hwD3_maxT_p92_s0,5);d$hwD3_maxT_p92_s6=lag(d$hwD3_maxT_p92_s0,6);d$hwD3_maxT_p92_s7=lag(d$hwD3_maxT_p92_s0,7)
  
  #폭염, 일최고기온, 93th percentile 산출 3일 지속  
  d$hwD3_maxT_p93_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.93,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p93_s1=lag(d$hwD3_maxT_p93_s0,1);d$hwD3_maxT_p93_s2=lag(d$hwD3_maxT_p93_s0,2)
  d$hwD3_maxT_p93_s3=lag(d$hwD3_maxT_p93_s0,3);d$hwD3_maxT_p93_s4=lag(d$hwD3_maxT_p93_s0,4)
  d$hwD3_maxT_p93_s5=lag(d$hwD3_maxT_p93_s0,5);d$hwD3_maxT_p93_s6=lag(d$hwD3_maxT_p93_s0,6);d$hwD3_maxT_p93_s7=lag(d$hwD3_maxT_p93_s0,7)
  
  #폭염, 일최고기온, 94th percentile 산출 3일 지속 
  d$hwD3_maxT_p94_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.94,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p94_s1=lag(d$hwD3_maxT_p94_s0,1);d$hwD3_maxT_p94_s2=lag(d$hwD3_maxT_p94_s0,2)
  d$hwD3_maxT_p94_s3=lag(d$hwD3_maxT_p94_s0,3);d$hwD3_maxT_p94_s4=lag(d$hwD3_maxT_p94_s0,4)
  d$hwD3_maxT_p94_s5=lag(d$hwD3_maxT_p94_s0,5);d$hwD3_maxT_p94_s6=lag(d$hwD3_maxT_p94_s0,6);d$hwD3_maxT_p94_s7=lag(d$hwD3_maxT_p94_s0,7)
  
  #폭염, 일최고기온, 95th percentile 산출 3일 지속 
  d$hwD3_maxT_p95_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxT,maxT_s1,maxT_s2)>=quantile(h$maxT,0.95,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxT_p95_s1=lag(d$hwD3_maxT_p95_s0,1);d$hwD3_maxT_p95_s2=lag(d$hwD3_maxT_p95_s0,2)
  d$hwD3_maxT_p95_s3=lag(d$hwD3_maxT_p95_s0,3);d$hwD3_maxT_p95_s4=lag(d$hwD3_maxT_p95_s0,4)
  d$hwD3_maxT_p95_s5=lag(d$hwD3_maxT_p95_s0,5);d$hwD3_maxT_p95_s6=lag(d$hwD3_maxT_p95_s0,6);d$hwD3_maxT_p95_s7=lag(d$hwD3_maxT_p95_s0,7)
  
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------------------------------------------------------------#
  
  #폭염, 일최고체감기온, 70th percentile 산출 2일 지속 
  d$hwD2_maxAT_p70_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.70,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p70_s1=lag(d$hwD2_maxAT_p70_s0,1);d$hwD2_maxAT_p70_s2=lag(d$hwD2_maxAT_p70_s0,2)
  d$hwD2_maxAT_p70_s3=lag(d$hwD2_maxAT_p70_s0,3);d$hwD2_maxAT_p70_s4=lag(d$hwD2_maxAT_p70_s0,4)
  d$hwD2_maxAT_p70_s5=lag(d$hwD2_maxAT_p70_s0,5);d$hwD2_maxAT_p70_s6=lag(d$hwD2_maxAT_p70_s0,6);d$hwD2_maxAT_p70_s7=lag(d$hwD2_maxAT_p70_s0,7)
  
  #폭염, 일최고체감기온, 75th percentile 산출 2일 지속 
  d$hwD2_maxAT_p75_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.75,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p75_s1=lag(d$hwD2_maxAT_p75_s0,1);d$hwD2_maxAT_p75_s2=lag(d$hwD2_maxAT_p75_s0,2)
  d$hwD2_maxAT_p75_s3=lag(d$hwD2_maxAT_p75_s0,3);d$hwD2_maxAT_p75_s4=lag(d$hwD2_maxAT_p75_s0,4)
  d$hwD2_maxAT_p75_s5=lag(d$hwD2_maxAT_p75_s0,5);d$hwD2_maxAT_p75_s6=lag(d$hwD2_maxAT_p75_s0,6);d$hwD2_maxAT_p75_s7=lag(d$hwD2_maxAT_p75_s0,7)
  
  #폭염, 일최고체감기온, 80th percentile 산출 2일 지속 
  d$hwD2_maxAT_p80_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.80,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p80_s1=lag(d$hwD2_maxAT_p80_s0,1);d$hwD2_maxAT_p80_s2=lag(d$hwD2_maxAT_p80_s0,2)
  d$hwD2_maxAT_p80_s3=lag(d$hwD2_maxAT_p80_s0,3);d$hwD2_maxAT_p80_s4=lag(d$hwD2_maxAT_p80_s0,4)
  d$hwD2_maxAT_p80_s5=lag(d$hwD2_maxAT_p80_s0,5);d$hwD2_maxAT_p80_s6=lag(d$hwD2_maxAT_p80_s0,6);d$hwD2_maxAT_p80_s7=lag(d$hwD2_maxAT_p80_s0,7)
  
  #폭염, 일최고체감기온, 85th percentile 산출 2일 지속 
  d$hwD2_maxAT_p85_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.85,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p85_s1=lag(d$hwD2_maxAT_p85_s0,1);d$hwD2_maxAT_p85_s2=lag(d$hwD2_maxAT_p85_s0,2)
  d$hwD2_maxAT_p85_s3=lag(d$hwD2_maxAT_p85_s0,3);d$hwD2_maxAT_p85_s4=lag(d$hwD2_maxAT_p85_s0,4)
  d$hwD2_maxAT_p85_s5=lag(d$hwD2_maxAT_p85_s0,5);d$hwD2_maxAT_p85_s6=lag(d$hwD2_maxAT_p85_s0,6);d$hwD2_maxAT_p85_s7=lag(d$hwD2_maxAT_p85_s0,7)
  
  #폭염, 일최고체감기온, 90th percentile 산출 2일 지속 
  d$hwD2_maxAT_p90_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.90,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p90_s1=lag(d$hwD2_maxAT_p90_s0,1);d$hwD2_maxAT_p90_s2=lag(d$hwD2_maxAT_p90_s0,2)
  d$hwD2_maxAT_p90_s3=lag(d$hwD2_maxAT_p90_s0,3);d$hwD2_maxAT_p90_s4=lag(d$hwD2_maxAT_p90_s0,4)
  d$hwD2_maxAT_p90_s5=lag(d$hwD2_maxAT_p90_s0,5);d$hwD2_maxAT_p90_s6=lag(d$hwD2_maxAT_p90_s0,6);d$hwD2_maxAT_p90_s7=lag(d$hwD2_maxAT_p90_s0,7)
  
  #폭염, 일최고체감기온, 91th percentile 산출 2일 지속
  d$hwD2_maxAT_p91_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.91,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p91_s1=lag(d$hwD2_maxAT_p91_s0,1);d$hwD2_maxAT_p91_s2=lag(d$hwD2_maxAT_p91_s0,2)
  d$hwD2_maxAT_p91_s3=lag(d$hwD2_maxAT_p91_s0,3);d$hwD2_maxAT_p91_s4=lag(d$hwD2_maxAT_p91_s0,4)
  d$hwD2_maxAT_p91_s5=lag(d$hwD2_maxAT_p91_s0,5);d$hwD2_maxAT_p91_s6=lag(d$hwD2_maxAT_p91_s0,6);d$hwD2_maxAT_p91_s7=lag(d$hwD2_maxAT_p91_s0,7)
  
  #폭염, 일최고체감기온, 92th percentile 산출 2일 지속 
  d$hwD2_maxAT_p92_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.92,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p92_s1=lag(d$hwD2_maxAT_p92_s0,1);d$hwD2_maxAT_p92_s2=lag(d$hwD2_maxAT_p92_s0,2)
  d$hwD2_maxAT_p92_s3=lag(d$hwD2_maxAT_p92_s0,3);d$hwD2_maxAT_p92_s4=lag(d$hwD2_maxAT_p92_s0,4)
  d$hwD2_maxAT_p92_s5=lag(d$hwD2_maxAT_p92_s0,5);d$hwD2_maxAT_p92_s6=lag(d$hwD2_maxAT_p92_s0,6);d$hwD2_maxAT_p92_s7=lag(d$hwD2_maxAT_p92_s0,7)
  
  #폭염, 일최고체감기온, 93th percentile 산출 2일 지속  
  d$hwD2_maxAT_p93_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.93,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p93_s1=lag(d$hwD2_maxAT_p93_s0,1);d$hwD2_maxAT_p93_s2=lag(d$hwD2_maxAT_p93_s0,2)
  d$hwD2_maxAT_p93_s3=lag(d$hwD2_maxAT_p93_s0,3);d$hwD2_maxAT_p93_s4=lag(d$hwD2_maxAT_p93_s0,4)
  d$hwD2_maxAT_p93_s5=lag(d$hwD2_maxAT_p93_s0,5);d$hwD2_maxAT_p93_s6=lag(d$hwD2_maxAT_p93_s0,6);d$hwD2_maxAT_p93_s7=lag(d$hwD2_maxAT_p93_s0,7)
  
  #폭염, 일최고체감기온, 94th percentile 산출 2일 지속 
  d$hwD2_maxAT_p94_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.94,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p94_s1=lag(d$hwD2_maxAT_p94_s0,1);d$hwD2_maxAT_p94_s2=lag(d$hwD2_maxAT_p94_s0,2)
  d$hwD2_maxAT_p94_s3=lag(d$hwD2_maxAT_p94_s0,3);d$hwD2_maxAT_p94_s4=lag(d$hwD2_maxAT_p94_s0,4)
  d$hwD2_maxAT_p94_s5=lag(d$hwD2_maxAT_p94_s0,5);d$hwD2_maxAT_p94_s6=lag(d$hwD2_maxAT_p94_s0,6);d$hwD2_maxAT_p94_s7=lag(d$hwD2_maxAT_p94_s0,7)
  
  #폭염, 일최고체감기온, 95th percentile 산출 2일 지속 
  d$hwD2_maxAT_p95_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1)>=quantile(h$maxAT,0.95,na.rm=T),1,0),1,sum)>=2,1,0))
  d$hwD2_maxAT_p95_s1=lag(d$hwD2_maxAT_p95_s0,1);d$hwD2_maxAT_p95_s2=lag(d$hwD2_maxAT_p95_s0,2)
  d$hwD2_maxAT_p95_s3=lag(d$hwD2_maxAT_p95_s0,3);d$hwD2_maxAT_p95_s4=lag(d$hwD2_maxAT_p95_s0,4)
  d$hwD2_maxAT_p95_s5=lag(d$hwD2_maxAT_p95_s0,5);d$hwD2_maxAT_p95_s6=lag(d$hwD2_maxAT_p95_s0,6);d$hwD2_maxAT_p95_s7=lag(d$hwD2_maxAT_p95_s0,7)
  
  #---------------------------------------------------------------------------------------------------------------------------------------#
  #---------------------------------------------------------------------------------------------------------------------------------------#
  #폭염, 일최고체감기온, 70th percentile 산출 3일 지속 
  d$hwD3_maxAT_p70_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.70,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p70_s1=lag(d$hwD3_maxAT_p70_s0,1);d$hwD3_maxAT_p70_s2=lag(d$hwD3_maxAT_p70_s0,2)
  d$hwD3_maxAT_p70_s3=lag(d$hwD3_maxAT_p70_s0,3);d$hwD3_maxAT_p70_s4=lag(d$hwD3_maxAT_p70_s0,4)
  d$hwD3_maxAT_p70_s5=lag(d$hwD3_maxAT_p70_s0,5);d$hwD3_maxAT_p70_s6=lag(d$hwD3_maxAT_p70_s0,6);d$hwD3_maxAT_p70_s7=lag(d$hwD3_maxAT_p70_s0,7)
  
  #폭염, 일최고체감기온, 75th percentile 산출 3일 지속 
  d$hwD3_maxAT_p75_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.75,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p75_s1=lag(d$hwD3_maxAT_p75_s0,1);d$hwD3_maxAT_p75_s2=lag(d$hwD3_maxAT_p75_s0,2)
  d$hwD3_maxAT_p75_s3=lag(d$hwD3_maxAT_p75_s0,3);d$hwD3_maxAT_p75_s4=lag(d$hwD3_maxAT_p75_s0,4)
  d$hwD3_maxAT_p75_s5=lag(d$hwD3_maxAT_p75_s0,5);d$hwD3_maxAT_p75_s6=lag(d$hwD3_maxAT_p75_s0,6);d$hwD3_maxAT_p75_s7=lag(d$hwD3_maxAT_p75_s0,7)
  
  #폭염, 일최고체감기온, 80th percentile 산출 3일 지속 
  d$hwD3_maxAT_p80_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.80,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p80_s1=lag(d$hwD3_maxAT_p80_s0,1);d$hwD3_maxAT_p80_s2=lag(d$hwD3_maxAT_p80_s0,2)
  d$hwD3_maxAT_p80_s3=lag(d$hwD3_maxAT_p80_s0,3);d$hwD3_maxAT_p80_s4=lag(d$hwD3_maxAT_p80_s0,4)
  d$hwD3_maxAT_p80_s5=lag(d$hwD3_maxAT_p80_s0,5);d$hwD3_maxAT_p80_s6=lag(d$hwD3_maxAT_p80_s0,6);d$hwD3_maxAT_p80_s7=lag(d$hwD3_maxAT_p80_s0,7)
  
  #폭염, 일최고체감기온, 85th percentile 산출 3일 지속 
  d$hwD3_maxAT_p85_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.85,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p85_s1=lag(d$hwD3_maxAT_p85_s0,1);d$hwD3_maxAT_p85_s2=lag(d$hwD3_maxAT_p85_s0,2)
  d$hwD3_maxAT_p85_s3=lag(d$hwD3_maxAT_p85_s0,3);d$hwD3_maxAT_p85_s4=lag(d$hwD3_maxAT_p85_s0,4)
  d$hwD3_maxAT_p85_s5=lag(d$hwD3_maxAT_p85_s0,5);d$hwD3_maxAT_p85_s6=lag(d$hwD3_maxAT_p85_s0,6);d$hwD3_maxAT_p85_s7=lag(d$hwD3_maxAT_p85_s0,7)
  
  #폭염, 일최고체감기온, 90th percentile 산출 3일 지속 
  d$hwD3_maxAT_p90_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.90,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p90_s1=lag(d$hwD3_maxAT_p90_s0,1);d$hwD3_maxAT_p90_s2=lag(d$hwD3_maxAT_p90_s0,2)
  d$hwD3_maxAT_p90_s3=lag(d$hwD3_maxAT_p90_s0,3);d$hwD3_maxAT_p90_s4=lag(d$hwD3_maxAT_p90_s0,4)
  d$hwD3_maxAT_p90_s5=lag(d$hwD3_maxAT_p90_s0,5);d$hwD3_maxAT_p90_s6=lag(d$hwD3_maxAT_p90_s0,6);d$hwD3_maxAT_p90_s7=lag(d$hwD3_maxAT_p90_s0,7)
  
  #폭염, 일최고체감기온, 91th percentile 산출 3일 지속
  d$hwD3_maxAT_p91_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.91,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p91_s1=lag(d$hwD3_maxAT_p91_s0,1);d$hwD3_maxAT_p91_s2=lag(d$hwD3_maxAT_p91_s0,2)
  d$hwD3_maxAT_p91_s3=lag(d$hwD3_maxAT_p91_s0,3);d$hwD3_maxAT_p91_s4=lag(d$hwD3_maxAT_p91_s0,4)
  d$hwD3_maxAT_p91_s5=lag(d$hwD3_maxAT_p91_s0,5);d$hwD3_maxAT_p91_s6=lag(d$hwD3_maxAT_p91_s0,6);d$hwD3_maxAT_p91_s7=lag(d$hwD3_maxAT_p91_s0,7)
  
  #폭염, 일최고체감기온, 92th percentile 산출 3일 지속 
  d$hwD3_maxAT_p92_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.92,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p92_s1=lag(d$hwD3_maxAT_p92_s0,1);d$hwD3_maxAT_p92_s2=lag(d$hwD3_maxAT_p92_s0,2)
  d$hwD3_maxAT_p92_s3=lag(d$hwD3_maxAT_p92_s0,3);d$hwD3_maxAT_p92_s4=lag(d$hwD3_maxAT_p92_s0,4)
  d$hwD3_maxAT_p92_s5=lag(d$hwD3_maxAT_p92_s0,5);d$hwD3_maxAT_p92_s6=lag(d$hwD3_maxAT_p92_s0,6);d$hwD3_maxAT_p92_s7=lag(d$hwD3_maxAT_p92_s0,7)
  
  #폭염, 일최고체감기온, 93th percentile 산출 3일 지속  
  d$hwD3_maxAT_p93_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.93,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p93_s1=lag(d$hwD3_maxAT_p93_s0,1);d$hwD3_maxAT_p93_s2=lag(d$hwD3_maxAT_p93_s0,2)
  d$hwD3_maxAT_p93_s3=lag(d$hwD3_maxAT_p93_s0,3);d$hwD3_maxAT_p93_s4=lag(d$hwD3_maxAT_p93_s0,4)
  d$hwD3_maxAT_p93_s5=lag(d$hwD3_maxAT_p93_s0,5);d$hwD3_maxAT_p93_s6=lag(d$hwD3_maxAT_p93_s0,6);d$hwD3_maxAT_p93_s7=lag(d$hwD3_maxAT_p93_s0,7)
  
  #폭염, 일최고체감기온, 94th percentile 산출 3일 지속 
  d$hwD3_maxAT_p94_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.94,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p94_s1=lag(d$hwD3_maxAT_p94_s0,1);d$hwD3_maxAT_p94_s2=lag(d$hwD3_maxAT_p94_s0,2)
  d$hwD3_maxAT_p94_s3=lag(d$hwD3_maxAT_p94_s0,3);d$hwD3_maxAT_p94_s4=lag(d$hwD3_maxAT_p94_s0,4)
  d$hwD3_maxAT_p94_s5=lag(d$hwD3_maxAT_p94_s0,5);d$hwD3_maxAT_p94_s6=lag(d$hwD3_maxAT_p94_s0,6);d$hwD3_maxAT_p94_s7=lag(d$hwD3_maxAT_p94_s0,7)
  
  #폭염, 일최고체감기온, 95th percentile 산출 3일 지속 
  d$hwD3_maxAT_p95_s0=with(d,ifelse(apply(ifelse(d %>% dplyr::select(maxAT,maxAT_s1,maxAT_s2)>=quantile(h$maxAT,0.95,na.rm=T),1,0),1,sum)>=3,1,0))
  d$hwD3_maxAT_p95_s1=lag(d$hwD3_maxAT_p95_s0,1);d$hwD3_maxAT_p95_s2=lag(d$hwD3_maxAT_p95_s0,2)
  d$hwD3_maxAT_p95_s3=lag(d$hwD3_maxAT_p95_s0,3);d$hwD3_maxAT_p95_s4=lag(d$hwD3_maxAT_p95_s0,4)
  d$hwD3_maxAT_p95_s5=lag(d$hwD3_maxAT_p95_s0,5);d$hwD3_maxAT_p95_s6=lag(d$hwD3_maxAT_p95_s0,6);d$hwD3_maxAT_p95_s7=lag(d$hwD3_maxAT_p95_s0,7)
  
  #전체 자료에서 관찰하고자 하는 폭염 노출 기간만 
  d2<-d %>% filter(month %in% c(smonth:emonth))
  d2}

mdis_s01<-sub_func("서울",6,9)
mdis_s02<-sub_func("부산",6,9)
mdis_s03<-sub_func("대구",6,9)
mdis_s04<-sub_func("인천",6,9)
mdis_s05<-sub_func("광주",6,9)
mdis_s06<-sub_func("대전",6,9)
mdis_s07<-sub_func("울산",6,9)
mdis_s08<-sub_func("세종",6,9)
mdis_s09<-sub_func("경기",6,9)
mdis_s10<-sub_func("강원",6,9)
mdis_s11<-sub_func("충북",6,9)
mdis_s12<-sub_func("충남",6,9)
mdis_s13<-sub_func("전북",6,9)
mdis_s14<-sub_func("전남",6,9)
mdis_s15<-sub_func("경북",6,9)
mdis_s16<-sub_func("경남",6,9)
mdis_s17<-sub_func("제주",6,9)

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#도시별 산출자료 취합
mdis_dat2<-rbind(mdis_s01,mdis_s02,mdis_s03,mdis_s04,
                 mdis_s05,mdis_s06,mdis_s07,mdis_s08,
                 mdis_s09,mdis_s10,mdis_s11,mdis_s12,
                 mdis_s13,mdis_s14,mdis_s15,mdis_s16,mdis_s17)

with(mdis_dat2,table(area,TN)) 

#일최고기온 절대온도(33), 상대온도, 이틀 이상 지속으로 정의한 폭염 지역별 빈도
hwD2_maxT_city_tb<-cbind(with(mdis_dat2,table(area,hwD2_maxT_s0))    ,with(mdis_dat2,table(area,hwD2_maxT_p70_s0)),
                         with(mdis_dat2,table(area,hwD2_maxT_p75_s0)),with(mdis_dat2,table(area,hwD2_maxT_p80_s0)),
                         with(mdis_dat2,table(area,hwD2_maxT_p85_s0)),with(mdis_dat2,table(area,hwD2_maxT_p90_s0)),
                         with(mdis_dat2,table(area,hwD2_maxT_p91_s0)),with(mdis_dat2,table(area,hwD2_maxT_p92_s0)),
                         with(mdis_dat2,table(area,hwD2_maxT_p93_s0)),with(mdis_dat2,table(area,hwD2_maxT_p94_s0)),
                         with(mdis_dat2,table(area,hwD2_maxT_p95_s0))) %>% as.data.frame

#일최고기온 절대온도(33), 상대온도, 삼일 이상 지속으로 정의한 폭염 지역별 빈도
hwD3_maxT_city_tb<-cbind(with(mdis_dat2,table(area,hwD3_maxT_s0))    ,with(mdis_dat2,table(area,hwD3_maxT_p70_s0)),
                         with(mdis_dat2,table(area,hwD3_maxT_p75_s0)),with(mdis_dat2,table(area,hwD3_maxT_p80_s0)),
                         with(mdis_dat2,table(area,hwD3_maxT_p85_s0)),with(mdis_dat2,table(area,hwD3_maxT_p90_s0)),
                         with(mdis_dat2,table(area,hwD3_maxT_p91_s0)),with(mdis_dat2,table(area,hwD3_maxT_p92_s0)),
                         with(mdis_dat2,table(area,hwD3_maxT_p93_s0)),with(mdis_dat2,table(area,hwD3_maxT_p94_s0)),
                         with(mdis_dat2,table(area,hwD3_maxT_p95_s0))) %>% as.data.frame

#일최고체감기온 절대온도(33), 상대온도, 이틀 이상 지속으로 정의한 폭염 지역별 빈도
hwD2_maxAT_city_tb<-cbind(with(mdis_dat2,table(area,hwD2_maxAT_s0))   ,with(mdis_dat2,table(area,hwD2_maxAT_p70_s0)),
                          with(mdis_dat2,table(area,hwD2_maxAT_p75_s0)),with(mdis_dat2,table(area,hwD2_maxAT_p80_s0)),
                          with(mdis_dat2,table(area,hwD2_maxAT_p85_s0)),with(mdis_dat2,table(area,hwD2_maxAT_p90_s0)),
                          with(mdis_dat2,table(area,hwD2_maxAT_p91_s0)),with(mdis_dat2,table(area,hwD2_maxAT_p92_s0)),
                          with(mdis_dat2,table(area,hwD2_maxAT_p93_s0)),with(mdis_dat2,table(area,hwD2_maxAT_p94_s0)),
                          with(mdis_dat2,table(area,hwD2_maxAT_p95_s0))) %>% as.data.frame

#일최고체감기온 절대온도(33), 상대온도, 삼일 이상 지속으로 정의한 폭염 지역별 빈도
hwD3_maxAT_city_tb<-cbind(with(mdis_dat2,table(area,hwD3_maxAT_s0))   ,with(mdis_dat2,table(area,hwD3_maxAT_p70_s0)),
                          with(mdis_dat2,table(area,hwD3_maxAT_p75_s0)),with(mdis_dat2,table(area,hwD3_maxAT_p80_s0)),
                          with(mdis_dat2,table(area,hwD3_maxAT_p85_s0)),with(mdis_dat2,table(area,hwD3_maxAT_p90_s0)),
                          with(mdis_dat2,table(area,hwD3_maxAT_p91_s0)),with(mdis_dat2,table(area,hwD3_maxAT_p92_s0)),
                          with(mdis_dat2,table(area,hwD3_maxAT_p93_s0)),with(mdis_dat2,table(area,hwD3_maxAT_p94_s0)),
                          with(mdis_dat2,table(area,hwD3_maxAT_p95_s0))) %>% as.data.frame

setwd("D:\\EUMC\\질병관리청\\폭염후속연구\\분석\\5세미만사망")
write.csv(hwD2_maxT_city_tb ,file="hwD2_maxT_city_tb.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_city_tb ,file="hwD3_maxT_city_tb.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_city_tb,file="hwD2_maxAT_city_tb.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_city_tb,file="hwD3_maxAT_city_tb.csv",row.names=F,na="",fileEncoding = "euc-kr")

d01<-subset(mdis_dat,KOR_SIDO=="서울");d02<-subset(mdis_dat,KOR_SIDO=="부산")
d03<-subset(mdis_dat,KOR_SIDO=="대구");d04<-subset(mdis_dat,KOR_SIDO=="인천")
d05<-subset(mdis_dat,KOR_SIDO=="광주");d06<-subset(mdis_dat,KOR_SIDO=="대전")
d07<-subset(mdis_dat,KOR_SIDO=="울산");d08<-subset(mdis_dat,KOR_SIDO=="세종")
d09<-subset(mdis_dat,KOR_SIDO=="경기");d10<-subset(mdis_dat,KOR_SIDO=="강원")
d11<-subset(mdis_dat,KOR_SIDO=="충북");d12<-subset(mdis_dat,KOR_SIDO=="충남")
d13<-subset(mdis_dat,KOR_SIDO=="전북");d14<-subset(mdis_dat,KOR_SIDO=="전남")
d15<-subset(mdis_dat,KOR_SIDO=="경북");d16<-subset(mdis_dat,KOR_SIDO=="경남")
d17<-subset(mdis_dat,KOR_SIDO=="제주")

#도시별 연구기간별 온도 분위수 (6~9월)
warm_maxT_tb<-rbind(quantile(mdis_s01$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s02$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s03$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s04$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s05$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s06$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s07$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s08$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s09$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s10$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s11$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s12$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s13$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s14$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s15$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s16$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                    quantile(mdis_s17$maxT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T)) %>% as.data.frame

warm_maxAT_tb<-rbind(quantile(mdis_s01$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s02$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s03$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s04$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s05$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s06$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s07$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s08$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s09$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s10$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s11$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s12$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s13$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s14$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s15$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),quantile(mdis_s16$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
                     quantile(mdis_s17$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T)) %>% as.data.frame


write.csv(warm_maxT_tb ,file="warm_maxT_tb.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(warm_maxAT_tb,file="warm_maxAT_tb.csv",row.names=F,na="",fileEncoding = "euc-kr")

#도시별, 노출 요약 
z1<-do.call(rbind,psych::describeBy(mdis_dat2$maxtemp ,mdis_dat2$area));z1$exposure="maxT"
z2<-do.call(rbind,psych::describeBy(mdis_dat2$maxAT   ,mdis_dat2$area));z2$exposure="maxAT"
z3<-do.call(rbind,psych::describeBy(mdis_dat2$HI_c    ,mdis_dat2$area));z3$exposure="HI"
z4<-do.call(rbind,psych::describeBy(mdis_dat2$DT      ,mdis_dat2$area));z4$exposure="DT"
z5<-do.call(rbind,psych::describeBy(mdis_dat2$temp_SD ,mdis_dat2$area));z5$exposure="temp_SD"
z6<-do.call(rbind,psych::describeBy(mdis_dat2$meanhumi,mdis_dat2$area));z6$exposure="meanhumi"
z7<-do.call(rbind,psych::describeBy(mdis_dat2$meandew ,mdis_dat2$area));z7$exposure="meandew"

zz<-rbind(z1,z2,z3,z4,z5,z6,z7)
write.csv(zz,file="zz.csv",row.names=F,na="",fileEncoding = "euc-kr")


#전체, 노출 요약 
t1<-aggregate(maxtemp ~ddate  ,data=mdis_dat2,mean,na.rm=T)
t2<-aggregate(maxAT   ~ddate  ,data=mdis_dat2,mean,na.rm=T)
t3<-aggregate(HI_c    ~ddate  ,data=mdis_dat2,mean,na.rm=T)
t4<-aggregate(DT      ~ddate  ,data=mdis_dat2,mean,na.rm=T)
t5<-aggregate(temp_SD ~ddate  ,data=mdis_dat2,mean,na.rm=T)
t6<-aggregate(meanhumi~ddate  ,data=mdis_dat2,mean,na.rm=T)
t7<-aggregate(meandew ~ddate  ,data=mdis_dat2,mean,na.rm=T)

z1<-as.data.frame(psych::describe(t1$maxtemp)) ;z1$exposure="maxT"
z2<-as.data.frame(psych::describe(t2$maxAT))   ;z2$exposure="maxAT"
z3<-as.data.frame(psych::describe(t3$HI_c))    ;z3$exposure="HI_c"
z4<-as.data.frame(psych::describe(t4$DT))      ;z4$exposure="DT"
z5<-as.data.frame(psych::describe(t5$temp_SD)) ;z5$exposure="temp_SD"
z6<-as.data.frame(psych::describe(t6$meanhumi));z6$exposure="meanhumi"
z7<-as.data.frame(psych::describe(t7$meandew)) ;z7$exposure="meandew"

zz<-rbind(z1,z2,z3,z4,z5,z6,z7)
write.csv(zz,file="zz.csv",row.names=F,na="",fileEncoding = "euc-kr")

zz<-rbind(quantile(t1$maxtemp,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T),
          quantile(t2$maxAT,p=c(0.70,0.75,0.80,0.85,0.90,seq(0.75,0.99,0.01)),na.rm=T))
write.csv(zz,file="zz.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#exposure-resonse curve
head(mdis_dat)
cor(mdis_s01 %>% dplyr::select(nonacc,maxAT,maxtemp,meandew,meanhumi,meanprec,meanwindsp,meanatp,totsun,DT,temp_SD,
                               HI_c,TN),use="complete.obs")

#------------------------------------------------------------------------------------------------#
#gam plot
fit.s01<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s01,family="poisson")
fit.s02<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s02,family="poisson")
fit.s03<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s03,family="poisson")
fit.s04<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s04,family="poisson")
fit.s05<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s05,family="poisson")
fit.s06<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s06,family="poisson")
fit.s07<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s07,family="poisson")
fit.s08<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s08,family="poisson")
fit.s09<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s09,family="poisson")
fit.s10<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s10,family="poisson")
fit.s11<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s11,family="poisson")
fit.s12<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s12,family="poisson")
fit.s13<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s13,family="poisson")
fit.s14<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s14,family="poisson")
fit.s15<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s15,family="poisson")
fit.s16<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s16,family="poisson")
fit.s17<-gam(nonacc~s(maxAT_s2)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s17,family="poisson")


fit.s01<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s01,family="poisson")
fit.s02<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s02,family="poisson")
fit.s03<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s03,family="poisson")
fit.s04<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s04,family="poisson")
fit.s05<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s05,family="poisson")
fit.s06<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s06,family="poisson")
fit.s07<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s07,family="poisson")
fit.s08<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s08,family="poisson")
fit.s09<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s09,family="poisson")
fit.s10<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s10,family="poisson")
fit.s11<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s11,family="poisson")
fit.s12<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s12,family="poisson")
fit.s13<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s13,family="poisson")
fit.s14<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s14,family="poisson")
fit.s15<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s15,family="poisson")
fit.s16<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s16,family="poisson")
fit.s17<-gam(nonacc~s(HI_c)+s(as.numeric(ddate),k=6*6)+s(meanhumi)+dow,data=mdis_s17,family="poisson")

x11();par(mfrow=c(4,4))
plot(fit.s01,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="Seoul")
plot(fit.s02,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="Busan")
plot(fit.s03,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="Daegu")
plot(fit.s04,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="Incheon")
plot(fit.s05,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="Gwangju")
plot(fit.s06,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="Daejeon")
plot(fit.s07,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="Ulsan")
plot(fit.s09,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="KK")
plot(fit.s10,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="KW")
plot(fit.s11,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="JB")
plot(fit.s12,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="JN")
plot(fit.s13,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="CB")
plot(fit.s14,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="CN")
plot(fit.s15,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="KB")
plot(fit.s16,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="KN")
plot(fit.s17,select=1,shade = T,cex.lab=1.4,cex.axis=1.4,cex.main=2,main="JJ")

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
#시도별 Time-series, maxAT (일최고체감기온)

mdis_s01$mon_year=substr(mdis_s01$ddate,1,7);mdis_s02$mon_year=substr(mdis_s02$ddate,1,7)
mdis_s03$mon_year=substr(mdis_s03$ddate,1,7);mdis_s04$mon_year=substr(mdis_s04$ddate,1,7)
mdis_s05$mon_year=substr(mdis_s05$ddate,1,7);mdis_s06$mon_year=substr(mdis_s06$ddate,1,7)
mdis_s07$mon_year=substr(mdis_s07$ddate,1,7);mdis_s08$mon_year=substr(mdis_s08$ddate,1,7)
mdis_s09$mon_year=substr(mdis_s09$ddate,1,7);mdis_s10$mon_year=substr(mdis_s10$ddate,1,7)
mdis_s11$mon_year=substr(mdis_s11$ddate,1,7);mdis_s12$mon_year=substr(mdis_s12$ddate,1,7)
mdis_s13$mon_year=substr(mdis_s13$ddate,1,7);mdis_s14$mon_year=substr(mdis_s14$ddate,1,7)
mdis_s15$mon_year=substr(mdis_s15$ddate,1,7);mdis_s16$mon_year=substr(mdis_s16$ddate,1,7)
mdis_s17$mon_year=substr(mdis_s17$ddate,1,7)

datalist=NULL
datalist[[1]] <-mdis_s01;datalist[[2]] <-mdis_s02;datalist[[3]] <-mdis_s03;datalist[[4]] <-mdis_s04
datalist[[5]] <-mdis_s05;datalist[[6]] <-mdis_s06;datalist[[7]] <-mdis_s07;datalist[[8]] <-mdis_s08
datalist[[9]] <-mdis_s09;datalist[[10]]<-mdis_s10;datalist[[11]]<-mdis_s11;datalist[[12]]<-mdis_s12
datalist[[13]]<-mdis_s13;datalist[[14]]<-mdis_s14;datalist[[15]]<-mdis_s15;datalist[[16]]<-mdis_s16;datalist[[17]]<-mdis_s17

#single lag, Modeling function
mod_s<-function(data,lab){
  d<-data
  d1<-d[,grep(lab,names(d))]
  d2<-d %>% dplyr:: select(ddate:area,nonacc,meanhumi_s0:meanhumi_s7,meanhumi_m1:meanhumi_m7,meandew_s0:meandew_s7,meandew_m1:meandew_m7)
  dd<-cbind(d1,d2)
  
  exposure<-names(dd[1:8]);names(dd)[1:8]=paste0("X0",1:8-1)
  
  f1<-gam(nonacc~X00+s(as.numeric(ddate),k=4*6)+s(meanhumi_s0)+dow,data=dd,family="quasipoisson")
  f2<-gam(nonacc~X01+s(as.numeric(ddate),k=4*6)+s(meanhumi_s1)+dow,data=dd,family="quasipoisson")
  f3<-gam(nonacc~X02+s(as.numeric(ddate),k=4*6)+s(meanhumi_s2)+dow,data=dd,family="quasipoisson")
  f4<-gam(nonacc~X03+s(as.numeric(ddate),k=4*6)+s(meanhumi_s3)+dow,data=dd,family="quasipoisson")
  f5<-gam(nonacc~X04+s(as.numeric(ddate),k=4*6)+s(meanhumi_s4)+dow,data=dd,family="quasipoisson")
  f6<-gam(nonacc~X05+s(as.numeric(ddate),k=4*6)+s(meanhumi_s5)+dow,data=dd,family="quasipoisson")
  f7<-gam(nonacc~X06+s(as.numeric(ddate),k=4*6)+s(meanhumi_s6)+dow,data=dd,family="quasipoisson")
  f8<-gam(nonacc~X07+s(as.numeric(ddate),k=4*6)+s(meanhumi_s7)+dow,data=dd,family="quasipoisson")
  
  res<-as.data.frame(rbind(cbind(t(summary(f1)$p.table[2,]),GCV=f1$gcv.ubre,deviance=f1$deviance),
                           cbind(t(summary(f2)$p.table[2,]),GCV=f2$gcv.ubre,deviance=f2$deviance),
                           cbind(t(summary(f3)$p.table[2,]),GCV=f3$gcv.ubre,deviance=f3$deviance),
                           cbind(t(summary(f4)$p.table[2,]),GCV=f4$gcv.ubre,deviance=f4$deviance),
                           cbind(t(summary(f5)$p.table[2,]),GCV=f5$gcv.ubre,deviance=f5$deviance),
                           cbind(t(summary(f6)$p.table[2,]),GCV=f6$gcv.ubre,deviance=f6$deviance),
                           cbind(t(summary(f7)$p.table[2,]),GCV=f7$gcv.ubre,deviance=f7$deviance),
                           cbind(t(summary(f8)$p.table[2,]),GCV=f8$gcv.ubre,deviance=f8$deviance)))
  
  row.names(res)=NULL                    
  
  res$lag  =paste0("lag",1:8-1)
  res$gubun="single"
  res$exposure=exposure
  res$city    =unique(datalist[[i]]$KOR_SIDO)
  names(res)[2:4]=c("SE","t","Pval")
  res
}

#moving average, Modeling function
mod_m<-function(data,lab){
  d<-data
  
  d1<-d[,grep(lab,names(d))]
  d2<-d %>% dplyr:: select(ddate:area,nonacc,meanhumi_s0:meanhumi_s7,meanhumi_m1:meanhumi_m7,meandew_s0:meandew_s7,meandew_m1:meandew_m7)
  dd<-cbind(d1,d2)
  
  exposure<-names(dd[1:7]);names(dd)[1:7]=paste0("X",1:7)
  
  f1<-gam(nonacc~X1+s(as.numeric(ddate),k=4*6)+s(meanhumi_m1)+dow,data=dd,family="quasipoisson")
  f2<-gam(nonacc~X2+s(as.numeric(ddate),k=4*6)+s(meanhumi_m2)+dow,data=dd,family="quasipoisson")
  f3<-gam(nonacc~X3+s(as.numeric(ddate),k=4*6)+s(meanhumi_m3)+dow,data=dd,family="quasipoisson")
  f4<-gam(nonacc~X4+s(as.numeric(ddate),k=4*6)+s(meanhumi_m4)+dow,data=dd,family="quasipoisson")
  f5<-gam(nonacc~X5+s(as.numeric(ddate),k=4*6)+s(meanhumi_m5)+dow,data=dd,family="quasipoisson")
  f6<-gam(nonacc~X6+s(as.numeric(ddate),k=4*6)+s(meanhumi_m6)+dow,data=dd,family="quasipoisson")
  f7<-gam(nonacc~X7+s(as.numeric(ddate),k=4*6)+s(meanhumi_m7)+dow,data=dd,family="quasipoisson")
  
  res<-as.data.frame(rbind(cbind(t(summary(f1)$p.table[2,]),GCV=f1$gcv.ubre,deviance=f1$deviance),
                           cbind(t(summary(f2)$p.table[2,]),GCV=f2$gcv.ubre,deviance=f2$deviance),
                           cbind(t(summary(f3)$p.table[2,]),GCV=f3$gcv.ubre,deviance=f3$deviance),
                           cbind(t(summary(f4)$p.table[2,]),GCV=f4$gcv.ubre,deviance=f4$deviance),
                           cbind(t(summary(f5)$p.table[2,]),GCV=f5$gcv.ubre,deviance=f5$deviance),
                           cbind(t(summary(f6)$p.table[2,]),GCV=f6$gcv.ubre,deviance=f6$deviance),
                           cbind(t(summary(f7)$p.table[2,]),GCV=f7$gcv.ubre,deviance=f7$deviance)))
  
  row.names(res)=NULL                    
  
  res$lag  =paste0("lag0",1:7)
  res$gubun="MA"
  res$exposure=exposure
  res$city    =unique(datalist[[i]]$KOR_SIDO)
  names(res)[2:4]=c("SE","t","Pval")
  res
}

maxT_s.list =NULL;maxT_m.list =NULL
maxAT_s.list=NULL;maxAT_m.list=NULL
HI_c.list   =NULL;TN.list     =NULL
DT_s.list   =NULL;DT_m.list   =NULL
TSD_s.list  =NULL;TSD_m.list  =NULL

hwD2_maxT.list=NULL ;hwD3_maxT.list=NULL
hwD2_maxAT.list=NULL;hwD3_maxAT.list=NULL

hwD2_maxT_p70.list=NULL;hwD2_maxT_p75.list=NULL;hwD2_maxT_p80.list=NULL;hwD2_maxT_p85.list=NULL;hwD2_maxT_p90.list=NULL;hwD2_maxT_p91.list=NULL
hwD2_maxT_p92.list=NULL;hwD2_maxT_p93.list=NULL;hwD2_maxT_p94.list=NULL;hwD2_maxT_p95.list=NULL

hwD2_maxAT_p70.list=NULL;hwD2_maxAT_p75.list=NULL;hwD2_maxAT_p80.list=NULL;hwD2_maxAT_p85.list=NULL;hwD2_maxAT_p90.list=NULL;hwD2_maxAT_p91.list=NULL
hwD2_maxAT_p92.list=NULL;hwD2_maxAT_p93.list=NULL;hwD2_maxAT_p94.list=NULL;hwD2_maxAT_p95.list=NULL

hwD3_maxT_p70.list=NULL;hwD3_maxT_p75.list=NULL;hwD3_maxT_p80.list=NULL;hwD3_maxT_p85.list=NULL;hwD3_maxT_p90.list=NULL;hwD3_maxT_p91.list=NULL
hwD3_maxT_p92.list=NULL;hwD3_maxT_p93.list=NULL;hwD3_maxT_p94.list=NULL;hwD3_maxT_p95.list=NULL

hwD3_maxAT_p70.list=NULL;hwD3_maxAT_p75.list=NULL;hwD3_maxAT_p80.list=NULL;hwD3_maxAT_p85.list=NULL;hwD3_maxAT_p90.list=NULL;hwD3_maxAT_p91.list=NULL
hwD3_maxAT_p92.list=NULL;hwD3_maxAT_p93.list=NULL;hwD3_maxAT_p94.list=NULL;hwD3_maxAT_p95.list=NULL


#도시별 모델링 
for(i in 1:17){
  #일최고기온
  maxT_s.list[[i]] <-mod_s(datalist[[i]],"maxT_s")
  maxT_m.list[[i]] <-mod_m(datalist[[i]],"maxT_m")
  
  #일최고체감기온
  maxAT_s.list[[i]]<-mod_s(datalist[[i]],"maxAT_s")
  maxAT_m.list[[i]]<-mod_m(datalist[[i]],"maxAT_m")
  
  #열지수
  HI_c.list[[i]]   <-mod_s(datalist[[i]],"HI_c_s")
  
  #열대야
  TN.list[[i]]     <-mod_s(datalist[[i]],"TN_s")
  
  #일교차
  
  DT_s.list[[i]]   <-mod_s(datalist[[i]],"DT_s")
  DT_m.list[[i]]   <-mod_m(datalist[[i]],"DT_m")
  
  #일일 기온편차
  TSD_s.list[[i]]  <-mod_s(datalist[[i]],"temp_SD_s")
  TSD_m.list[[i]]  <-mod_m(datalist[[i]],"temp_SD_m")
  
  #폭염: 절대온도(33도), 일최고기온 
  hwD2_maxT.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_s")
  hwD3_maxT.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_s")
  
  #폭염: 절대온도(33도), 일최고체감기온 
  hwD2_maxAT.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_s")
  hwD3_maxAT.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_s")
  
  #폭염: 상대온도(분위수), 일최고기온 2일
  hwD2_maxT_p70.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p70_s");hwD2_maxT_p75.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p75_s")
  hwD2_maxT_p80.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p80_s");hwD2_maxT_p85.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p85_s")
  hwD2_maxT_p90.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p90_s");hwD2_maxT_p91.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p91_s")
  hwD2_maxT_p92.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p92_s");hwD2_maxT_p93.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p93_s")
  hwD2_maxT_p94.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p94_s");hwD2_maxT_p95.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxT_p95_s")
  
  #폭염: 상대온도(분위수), 일최고기온 3일
  hwD3_maxT_p70.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p70_s");hwD3_maxT_p75.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p75_s")
  hwD3_maxT_p80.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p80_s");hwD3_maxT_p85.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p85_s")
  hwD3_maxT_p90.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p90_s");hwD3_maxT_p91.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p91_s")
  hwD3_maxT_p92.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p92_s");hwD3_maxT_p93.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p93_s")
  hwD3_maxT_p94.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p94_s");hwD3_maxT_p95.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxT_p95_s")
  
  #폭염: 상대온도(분위수), 일최고체감기온 2일
  hwD2_maxAT_p70.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p70_s");hwD2_maxAT_p75.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p75_s")
  hwD2_maxAT_p80.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p80_s");hwD2_maxAT_p85.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p85_s")
  hwD2_maxAT_p90.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p90_s");hwD2_maxAT_p91.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p91_s")
  hwD2_maxAT_p92.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p92_s");hwD2_maxAT_p93.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p93_s")
  hwD2_maxAT_p94.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p94_s");hwD2_maxAT_p95.list[[i]]<-mod_s(datalist[[i]],"hwD2_maxAT_p95_s")
  
  #폭염: 상대온도(분위수), 일최고체감기온 3일
  hwD3_maxAT_p70.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p70_s");hwD3_maxAT_p75.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p75_s")
  hwD3_maxAT_p80.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p80_s");hwD3_maxAT_p85.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p85_s")
  hwD3_maxAT_p90.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p90_s");hwD3_maxAT_p91.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p91_s")
  hwD3_maxAT_p92.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p92_s");hwD3_maxAT_p93.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p93_s")
  hwD3_maxAT_p94.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p94_s");hwD3_maxAT_p95.list[[i]]<-mod_s(datalist[[i]],"hwD3_maxAT_p95_s")
  
  print(i)
}

maxT_s.res <-do.call(rbind,maxT_s.list) ;maxT_m.res <-do.call(rbind,maxT_m.list)
maxAT_s.res<-do.call(rbind,maxAT_s.list);maxAT_m.res<-do.call(rbind,maxAT_m.list)

HI_c.res<-do.call(rbind,HI_c.list)
TN.res  <-do.call(rbind,TN.list)

DT_s.res<-do.call(rbind,DT_s.list)  ;DT_m.res<-do.call(rbind,DT_m.list)
TSD_s.res<-do.call(rbind,TSD_s.list);TSD_m.res<-do.call(rbind,TSD_m.list)

hwD2_maxT.res <-do.call(rbind,hwD2_maxT.list) ;hwD3_maxT.res<-do.call(rbind,hwD3_maxT.list)
hwD2_maxAT.res<-do.call(rbind,hwD2_maxAT.list);hwD3_maxAT.res<-do.call(rbind,hwD3_maxAT.list)

hwD2_maxT_p70.res<-do.call(rbind,hwD2_maxT_p70.list);hwD2_maxT_p75.res<-do.call(rbind,hwD2_maxT_p75.list)
hwD2_maxT_p80.res<-do.call(rbind,hwD2_maxT_p80.list);hwD2_maxT_p85.res<-do.call(rbind,hwD2_maxT_p85.list)
hwD2_maxT_p90.res<-do.call(rbind,hwD2_maxT_p90.list);hwD2_maxT_p91.res<-do.call(rbind,hwD2_maxT_p91.list)
hwD2_maxT_p92.res<-do.call(rbind,hwD2_maxT_p92.list);hwD2_maxT_p93.res<-do.call(rbind,hwD2_maxT_p93.list)
hwD2_maxT_p94.res<-do.call(rbind,hwD2_maxT_p94.list);hwD2_maxT_p95.res<-do.call(rbind,hwD2_maxT_p95.list)

hwD3_maxT_p70.res<-do.call(rbind,hwD3_maxT_p70.list);hwD3_maxT_p75.res<-do.call(rbind,hwD3_maxT_p75.list)
hwD3_maxT_p80.res<-do.call(rbind,hwD3_maxT_p80.list);hwD3_maxT_p85.res<-do.call(rbind,hwD3_maxT_p85.list)
hwD3_maxT_p90.res<-do.call(rbind,hwD3_maxT_p90.list);hwD3_maxT_p91.res<-do.call(rbind,hwD3_maxT_p91.list)
hwD3_maxT_p92.res<-do.call(rbind,hwD3_maxT_p92.list);hwD3_maxT_p93.res<-do.call(rbind,hwD3_maxT_p93.list)
hwD3_maxT_p94.res<-do.call(rbind,hwD3_maxT_p94.list);hwD3_maxT_p95.res<-do.call(rbind,hwD3_maxT_p95.list)


hwD2_maxAT_p70.res<-do.call(rbind,hwD2_maxAT_p70.list);hwD2_maxAT_p75.res<-do.call(rbind,hwD2_maxAT_p75.list)
hwD2_maxAT_p80.res<-do.call(rbind,hwD2_maxAT_p80.list);hwD2_maxAT_p85.res<-do.call(rbind,hwD2_maxAT_p85.list)
hwD2_maxAT_p90.res<-do.call(rbind,hwD2_maxAT_p90.list);hwD2_maxAT_p91.res<-do.call(rbind,hwD2_maxAT_p91.list)
hwD2_maxAT_p92.res<-do.call(rbind,hwD2_maxAT_p92.list);hwD2_maxAT_p93.res<-do.call(rbind,hwD2_maxAT_p93.list)
hwD2_maxAT_p94.res<-do.call(rbind,hwD2_maxAT_p94.list);hwD2_maxAT_p95.res<-do.call(rbind,hwD2_maxAT_p95.list)

hwD3_maxAT_p70.res<-do.call(rbind,hwD3_maxAT_p70.list);hwD3_maxAT_p75.res<-do.call(rbind,hwD3_maxAT_p75.list)
hwD3_maxAT_p80.res<-do.call(rbind,hwD3_maxAT_p80.list);hwD3_maxAT_p85.res<-do.call(rbind,hwD3_maxAT_p85.list)
hwD3_maxAT_p90.res<-do.call(rbind,hwD3_maxAT_p90.list);hwD3_maxAT_p91.res<-do.call(rbind,hwD3_maxAT_p91.list)
hwD3_maxAT_p92.res<-do.call(rbind,hwD3_maxAT_p92.list);hwD3_maxAT_p93.res<-do.call(rbind,hwD3_maxAT_p93.list)
hwD3_maxAT_p94.res<-do.call(rbind,hwD3_maxAT_p94.list);hwD3_maxAT_p95.res<-do.call(rbind,hwD3_maxAT_p95.list)

setwd("D:\\EUMC\\질병관리청\\result")
write.csv(maxT_s.res ,file="maxT_s.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(maxT_m.res ,file="maxT_m.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(maxAT_s.res,file="maxAT_s.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(maxAT_m.res,file="maxAT_m.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

write.csv(HI_c.res,file="HI_c.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(TN.res  ,file="TN.res.csv"  ,row.names=F,na="",fileEncoding = "euc-kr")

write.csv(DT_s.res,file="DT_s.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(DT_m.res,file="DT_m.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

write.csv(TSD_s.res,file="TSD_s.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(TSD_m.res,file="TSD_m.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

write.csv(hwD2_maxT.res ,file="hwD2_maxT.res.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT.res ,file="hwD3_maxT.res.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT.res,file="hwD2_maxAT.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT.res,file="hwD3_maxAT.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

write.csv(hwD2_maxT_p70.res,file="hwD2_maxT_p70.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p75.res,file="hwD2_maxT_p75.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p80.res,file="hwD2_maxT_p80.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p85.res,file="hwD2_maxT_p85.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p90.res,file="hwD2_maxT_p90.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p91.res,file="hwD2_maxT_p91.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p92.res,file="hwD2_maxT_p92.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p93.res,file="hwD2_maxT_p93.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p94.res,file="hwD2_maxT_p94.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxT_p95.res,file="hwD2_maxT_p95.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

write.csv(hwD3_maxT_p70.res,file="hwD3_maxT_p70.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p75.res,file="hwD3_maxT_p75.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p80.res,file="hwD3_maxT_p80.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p85.res,file="hwD3_maxT_p85.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p90.res,file="hwD3_maxT_p90.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p91.res,file="hwD3_maxT_p91.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p92.res,file="hwD3_maxT_p92.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p93.res,file="hwD3_maxT_p93.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p94.res,file="hwD3_maxT_p94.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxT_p95.res,file="hwD3_maxT_p95.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

write.csv(hwD2_maxAT_p70.res,file="hwD2_maxAT_p70.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p75.res,file="hwD2_maxAT_p75.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p80.res,file="hwD2_maxAT_p80.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p85.res,file="hwD2_maxAT_p85.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p90.res,file="hwD2_maxAT_p90.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p91.res,file="hwD2_maxAT_p91.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p92.res,file="hwD2_maxAT_p92.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p93.res,file="hwD2_maxAT_p93.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p94.res,file="hwD2_maxAT_p94.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD2_maxAT_p95.res,file="hwD2_maxAT_p95.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

write.csv(hwD3_maxAT_p70.res,file="hwD3_maxAT_p70.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p75.res,file="hwD3_maxAT_p75.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p80.res,file="hwD3_maxAT_p80.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p85.res,file="hwD3_maxAT_p85.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p90.res,file="hwD3_maxAT_p90.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p91.res,file="hwD3_maxAT_p91.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p92.res,file="hwD3_maxAT_p92.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p93.res,file="hwD3_maxAT_p93.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p94.res,file="hwD3_maxAT_p94.res.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(hwD3_maxAT_p95.res,file="hwD3_maxAT_p95.res.csv",row.names=F,na="",fileEncoding = "euc-kr")

#------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------#
maxT_city<-read_excel("D:\\EUMC\\질병관리청\\폭염후속연구\\data\\폭염자료.xlsx",sheet="maxT_gam")
maxT_city.r<-maxT_city %>% filter(!city %in% c("세종"))

sub<-subset(maxT_city.r,lag=="lag0")
area_index=c("Seoul","Busan","Daegu","Incheon","Gwangju","Daejeon","Ulsan","KK","KW","CB","CN","JB","JN","KB","KN","JJ")
sub$area=area_index

meta.1<-with(sub,rma(yi=Estimate, sei=SE, slab=area, measure="RR",digits=5,method="FE"))
meta.2<-with(sub,rma(yi=Estimate, sei=SE, slab=area, measure="RR",digits=5,method="REML"))

x11();forest(meta.1) #Fixed effect
x11();forest(meta.2) #Random effect

#Results table
meta.fixed <-with(summary(meta.1),data.frame(Estimate=beta,SE=se,pval=pval,
                                             lci=ci.lb,uci=ci.ub,
                                             RR=exp(beta),RR_lci=exp(ci.lb),RR_uci=exp(ci.ub),
                                             I2    =I2,H2    =H2,tau   =sqrt(tau2),t(fit.stats[,2])))


meta.random<-with(summary(meta.2),data.frame(Estimate=beta,SE=se,pval=pval,
                                             lci=ci.lb,uci=ci.ub,
                                             RR=exp(beta),RR_lci=exp(ci.lb),RR_uci=exp(ci.ub),
                                             I2    =I2,H2    =H2,tau   =sqrt(tau2),t(fit.stats[,2])))


meta_df<-as.data.frame(rbind(meta.fixed,meta.random))
meta_df$lag="lag0"
meta_df$exposure="maxT"
meta_df$gubun=c("Fixed","Random")

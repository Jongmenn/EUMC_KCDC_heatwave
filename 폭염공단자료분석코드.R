#-------------------------------------------------------------------------#
#폭염 질병청 공단자료 
#-------------------------------------------------------------------------#
#library
pacman::p_load("dplyr","ggplot2","reshape2","sqldf","RColorBrewer","lubridate","lmtest","readxl","survival",
               "splines","data.table","stringr","tidyr","extrafont","scales","gridExtra","Epi","tsModel",
               "mgcv","gamm4","metafor","dlnm","survival","splines")

#-------------------------------------------------------------------------#
setwd("D:\\EUMC\\질병관리청\\폭염연구\\공단자료")
child_ts<-read.csv("child_ts.csv")
child_ts$ddate=as.Date(child_ts$ddate)

#dataset n; 1826*17=31,042, 2015-01-01~2019-12-31까지 기간*17개 도시


child_ts %>% group_by(year,month) %>% summarise(meanT=mean(meantemp_lag0,na.rm=T),
                                                maxT=mean(maxtemp_lag0,na.rm=T)) %>% View

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#노출 자료
temp_outcome<-read_excel("D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\heattable.xlsx",sheet=1)

to<-melt(temp_outcome %>% select(-c(note)),id.vars=c("category","month"))
to$month=ifelse(as.numeric(gsub("월","",to$month))<10,paste0(0,as.numeric(gsub("월","",to$month))),
                as.numeric(gsub("월","",to$month)))
to$yymm=ymd(paste0(substr(to$variable,2,5),"-",to$month,"-",01))

unique(to$category)

to01<-to %>% filter(category %in% c("birth"))
to02<-to %>% filter(category %in% c("ptb"))
to03<-to %>% filter(category %in% c("lbw"))
to04<-to %>% filter(category %in% c("heatrelated"))
to05<-to %>% filter(category %in% c("feb"))
to06<-to %>% filter(category %in% c("hfmd"))
to07<-to %>% filter(category %in% c("atopic"))
to08<-to %>% filter(category %in% c("om"))
to19<-to %>% filter(category %in% c("kawa"))
to10<-to %>% filter(category %in% c("intestinal"))
to11<-to %>% filter(category %in% c("camp"))
to12<-to %>% filter(category %in% c("respiratory"))
to13<-to %>% filter(category %in% c("pneumonia"))
to14<-to %>% filter(category %in% c("asthma"))
to15<-to %>% filter(category %in% c("URI"))
to16<-to %>% filter(category %in% c("ALRI"))
to17<-to %>% filter(category %in% c("bronch1"))
to18<-to %>% filter(category %in% c("bronch2"))


exp <-to %>% filter(category %in% c("meanT","maxT"))
exp$gubun=rep(c("평균기온(°C)","최고기온(°C)"),each=12)
exp$gubun=factor(exp$gubun,levels=unique(exp$gubun))

library(scales)
gexp<-ggplot(exp,aes(yymm,value,col=gubun))+geom_point(size=5)+geom_line(size=2)+labs(x="",y="기온(°C)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        legend.title = element_blank(),legend.position = "top")+
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")+
  scale_color_manual(values=c("orange","red"))

g1<-ggplot(to01,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="출생아 건수(명)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g2<-ggplot(to02,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="조산아 건수(명)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g3<-ggplot(to03,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="저체중아 건수(명)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g4<-ggplot(to04,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="온열질환관련 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g5<-ggplot(to05,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="열성경련 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g6<-ggplot(to06,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="수족구병 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g7<-ggplot(to07,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="아토피피부염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g8<-ggplot(to08,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="중이염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g9<-ggplot(to09,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="가와사키병 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g10<-ggplot(to10,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="전체 장감염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g11<-ggplot(to11,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="캄필로박터 장감염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g12<-ggplot(to12,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="전체 호흡기 질환 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g13<-ggplot(to13,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="폐렴 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g14<-ggplot(to14,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="천식 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g15<-ggplot(to15,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="상기도 감염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g16<-ggplot(to16,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="급성하기도 감염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g17<-ggplot(to17,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="기관지염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

g18<-ggplot(to18,aes(yymm,value))+geom_point(size=5)+geom_line(size=2)+labs(x="년월",y="급성세기관지염 입원발생건수(건)")+
  theme_bw(base_size=25)+scale_y_continuous(labels = scales::comma)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(breaks = date_breaks("1 months"),date_labels = "%Y %m")

x11();grid.arrange(gexp,g1,ncol=1)
x11();grid.arrange(gexp,g2,ncol=1)
x11();grid.arrange(gexp,g3,ncol=1)
x11();grid.arrange(gexp,g4,ncol=1)
x11();grid.arrange(gexp,g5,ncol=1)
x11();grid.arrange(gexp,g6,ncol=1)
x11();grid.arrange(gexp,g7,ncol=1)
x11();grid.arrange(gexp,g8,ncol=1)
x11();grid.arrange(gexp,g9,ncol=1)
x11();grid.arrange(gexp,g10,ncol=1)
x11();grid.arrange(gexp,g11,ncol=1)
x11();grid.arrange(gexp,g12,ncol=1)
x11();grid.arrange(gexp,g13,ncol=1)
x11();grid.arrange(gexp,g14,ncol=1)
x11();grid.arrange(gexp,g15,ncol=1)
x11();grid.arrange(gexp,g16,ncol=1)
x11();grid.arrange(gexp,g17,ncol=1)
x11();grid.arrange(gexp,g18,ncol=1)
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#


#질환 이름 
names(child_ts)[grep("TOT",names(child_ts))]
names(d01)

d01<-child_ts %>% dplyr::select(heatrelated_CHILD_TOT,ddate:pm25_lag07) 
d02<-child_ts %>% dplyr::select(VOL_CHILD_TOT,ddate:pm25_lag07)           
d03<-child_ts %>% dplyr::select(feb_CHILD_TOT,ddate:pm25_lag07)
d04<-child_ts %>% dplyr::select(HFMD_CHILD_TOT,ddate:pm25_lag07)
d05<-child_ts %>% dplyr::select(RES_CHILD_TOT,ddate:pm25_lag07)
d06<-child_ts %>% dplyr::select(acute_bronch1_CHILD_TOT,ddate:pm25_lag07)
d07<-child_ts %>% dplyr::select(acute_bronch2_CHILD_TOT,ddate:pm25_lag07)
d08<-child_ts %>% dplyr::select(URI_CHILD_TOT,ddate:pm25_lag07)
d09<-child_ts %>% dplyr::select(ALRI_CHILD_TOT,ddate:pm25_lag07)
d10<-child_ts %>% dplyr::select(asthma_CHILD_TOT,ddate:pm25_lag07)
d11<-child_ts %>% dplyr::select(pne_CHILD_TOT,ddate:pm25_lag07)
d12<-child_ts %>% dplyr::select(ATOPIC_CHILD_TOT,ddate:pm25_lag07)
d13<-child_ts %>% dplyr::select(RSV_CHILD_TOT,ddate:pm25_lag07)
d14<-child_ts %>% dplyr::select(CARDI_CHILD_TOT,ddate:pm25_lag07)
d15<-child_ts %>% dplyr::select(KAWA_CHILD_TOT,ddate:pm25_lag07)
d16<-child_ts %>% dplyr::select(mumps_CHILD_TOT,ddate:pm25_lag07)
d17<-child_ts %>% dplyr::select(om_CHILD_TOT,ddate:pm25_lag07)
d18<-child_ts %>% dplyr::select(intest_CHILD_TOT,ddate:pm25_lag07)
d19<-child_ts %>% dplyr::select(rota_CHILD_TOT,ddate:pm25_lag07)
d20<-child_ts %>% dplyr::select(camp_CHILD_TOT,ddate:pm25_lag07)

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#전체 일괄적으로 GAMM으로 추정하기 
d01 %>% dplyr:: group_by(area) %>% summarise(mean=mean(heatrelated_CHILD_TOT),var=var(heatrelated_CHILD_TOT))

d01 %>% dplyr::  group_by(ddate) %>% summarise(meanT=mean(meantemp_lag0,na.rm=T),
                                               maxT =mean(maxtemp_lag0,na.rm=T),
                                               pm25=mean(pm25_lag0,na.rm=T),
                                               meanhumi=mean(meanhumi_lag0,na.rm=T),
                                               ap=mean(meanpress1_lag0,na.rm=T),
                                               windspeed=mean(windspeed_lag0,na.rm=T),
                                               dewtemp=mean(dewtemp_lag0),na.rm=T) %>% dplyr::select(meanT:dewtemp) %>% 
  cor(method="spearman",use="complete.obs")

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#Exposure-repsonse curve, GAMM

tt.f<-function(dataset){
  #dataset
  #결측 제거, 세종, 제주 제외, 6~8월만
  tt<-dataset[complete.cases(dataset),] %>% filter(!area %in% c("세종","제주")) %>% filter(month %in% c(6:8))
  tt$outcome=tt[,1]
  tt$area=factor(tt$area)
  tt}

#GAMM에 적용하려는 형태로 dataset 변경
tt01<-tt.f(d01);tt02<-tt.f(d02);tt03<-tt.f(d03);tt04<-tt.f(d04);tt05<-tt.f(d05);
tt06<-tt.f(d06);tt07<-tt.f(d07);tt08<-tt.f(d08);tt09<-tt.f(d09);tt10<-tt.f(d10);
tt11<-tt.f(d11);tt12<-tt.f(d12);tt13<-tt.f(d13);tt14<-tt.f(d14);tt15<-tt.f(d15);
tt16<-tt.f(d16);tt17<-tt.f(d17);tt18<-tt.f(d18);tt19<-tt.f(d19);tt20<-tt.f(d20)

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 온열질환 관련
tt01.fig=NULL
tt01.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 온열질환 관련
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\온열질환(heatrelated).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt01.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt01.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt01.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt01.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt01.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt01.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt01.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt01.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.3,0.3));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 열성경련
tt03.fig=NULL
tt03.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt03.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt03.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt03.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt03.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt03.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt03.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt03.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt03,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 열성경련
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\열성경련(feb).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt03.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt03.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt03.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt03.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt03.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt03.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt03.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt03.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.3,0.3));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 수족구병
tt04.fig=NULL
tt04.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt04.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt04.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt04.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt04.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt04.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt04.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt04.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt04,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 수족구병
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\수족구병(HFMD).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt04.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt04.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt04.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt04.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt04.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt04.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt04.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt04.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-1.0,1.0));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 전체호흡기(Respiratory)
tt05.fig=NULL
tt05.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt05.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt05.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt05.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt05.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt05.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt05.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt05.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt05,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 전체호흡기(Respiratory)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\전체호흡기(Respiratory).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt05.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt05.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt05.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt05.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt05.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt05.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt05.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt05.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 급성기관지염(acute_bronch1)
tt06.fig=NULL
tt06.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt06.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt06.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt06.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt06.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt06.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt06.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt06.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt06,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 전체호흡기(Respiratory)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\급성기관지염(acute_bronch1).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt06.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt06.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt06.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt06.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt06.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt06.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt06.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt06.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 급성세기관지염(acute_bronch2)
tt07.fig=NULL
tt07.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt07.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt07.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt07.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt07.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt07.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt07.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt07.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt07,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 전체호흡기(Respiratory)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\급성세기관지염(acute_bronch2).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt07.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt07.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt07.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt07.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt07.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt07.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt07.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt07.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 상기도감염(URI)
tt08.fig=NULL
tt08.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt08.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt08.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt08.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt08.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt08.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt08.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt08.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt08,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 상기도감염(URI)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\상기도감염(URI).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt08.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt08.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt08.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt08.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt08.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt08.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt08.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.3,0.3));abline(h=0,col="red")
plot(tt08.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.3,0.3));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 하기도감염(ALRI)
tt09.fig=NULL
tt09.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt09.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt09.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt09.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt09.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt09.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt09.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt09.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt09,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 하기도감염(ALRI)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\하기도감염(ALRI).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt09.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt09.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt09.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt09.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt09.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt09.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt09.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt09.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 천식(asthma)
tt10.fig=NULL
tt10.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt10.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt10.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt10.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt10.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt10.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt10.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt10.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt10,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,노출 반응 그림저장, 단일 지연; 천식(asthma)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\천식(asthma).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt10.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt10.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt10.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt10.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt10.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt10.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt10.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt10.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 폐렴(pneumonia)
#폐렴 뭔가 잘 안돌아감, niterPQL 수정할 필요가 있음, 수렴을 안함 
tt11.fig=NULL

table(tt11$outcome)
tt11.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5,fx=T)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=11000))
tt11.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5,fx=T)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=11000))
tt11.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5,fx=T)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=11000))
tt11.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5,fx=T)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=11000))
tt11.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5,fx=T)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=20000))
tt11.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5,fx=T)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=11000))
tt11.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5,fx=T)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=11000))
tt11.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5,fx=T)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt11,family="quasipoisson",niterPQL=50,control=lmeControl(opt="optim",msMaxIter=11000))

tt11.fig$er4$gam

##GAMM,노출 반응 그림저장, 단일 지연; 폐렴(pneumonia)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\폐렴(pneumonia).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt11.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt11.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt11.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt11.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt11.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt11.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt11.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt11.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-1.0,1.0));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 아토피(atopic)
#폐렴 뭔가 잘 안돌아감, niterPQL 수정할 필요가 있음, 수렴을 안함 
tt12.fig=NULL
tt12.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))
tt12.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))
tt12.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))
tt12.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))
tt12.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))
tt12.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))
tt12.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))
tt12.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt12,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=11000))

##GAMM,노출 반응 그림저장, 단일 지연; 아토피(atopic)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\아토피(atopic).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt12.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt12.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt12.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt12.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt12.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt12.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt12.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt12.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 가와사키(Kawasaki)
tt15.fig=NULL
tt15.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt15.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt15.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt15.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt15.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt15.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt15.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt15.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt15,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))

##GAMM,노출 반응 그림저장, 단일 지연; 가와사키(Kawasaki)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\가와사키(Kawasaki).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt15.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt15.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt15.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt15.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt15.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt15.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt15.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt15.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 볼거리(Mumps)
tt16.fig=NULL
tt16.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt16.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt16.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt16.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt16.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt16.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt16.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))
tt16.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt16,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=15000))

##GAMM,노출 반응 그림저장, 단일 지연; 볼거리(Mumps)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\볼거리(Mumps).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt16.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt16.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt16.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt16.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt16.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt16.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt16.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt16.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-1.0,1.0));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 중이염(otitismedia)
tt17.fig=NULL
tt17.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5,fx=T)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt17.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5,fx=T)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt17.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5,fx=T)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt17.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5,fx=T)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt17.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5,fx=T)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt17.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5,fx=T)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt17.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5,fx=T)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt17.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5,fx=T)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt17,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))

##GAMM,노출 반응 그림저장, 단일 지연; 중이염(otitismedia)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\중이염(otitismedia).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt17.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt17.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt17.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt17.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt17.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt17.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt17.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.5,0.5));abline(h=0,col="red")
plot(tt17.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.5,0.5));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 장감염질환(intestinal_infectious)
tt18.fig=NULL
tt18.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt18.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt18.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt18.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt18.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt18.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt18.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt18.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt18,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))

##GAMM,노출 반응 그림저장, 단일 지연; 장감염질환(intestinal_infectious)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\장감염질환(intestinal_infectious).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt18.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(tt18.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(tt18.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(tt18.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(tt18.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(tt18.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(tt18.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-0.2,0.2));abline(h=0,col="red")
plot(tt18.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-0.2,0.2));abline(h=0,col="red")
dev.off()

#--------------------------------------------------------------------------#
##GAMM,노출 반응 그림, 단일 지연; 캄필로박터속감염(camp)
tt20.fig=NULL
tt20.fig$er0<-gamm(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt20.fig$er1<-gamm(outcome~s(maxtemp_lag1)+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt20.fig$er2<-gamm(outcome~s(maxtemp_lag2)+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt20.fig$er3<-gamm(outcome~s(maxtemp_lag3)+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt20.fig$er4<-gamm(outcome~s(maxtemp_lag4)+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt20.fig$er5<-gamm(outcome~s(maxtemp_lag5)+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt20.fig$er6<-gamm(outcome~s(maxtemp_lag6)+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))
tt20.fig$er7<-gamm(outcome~s(maxtemp_lag7)+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt20,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=17000))

##GAMM,노출 반응 그림저장, 단일 지연; 캄필로박터속감염(camp)
png(file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\캄필로박터속감염(camp).png",width=1600, height=800)
par(mfrow=c(2,4),mar=c(5,5,5,5),oma=c(3,3,2,3))
plot(tt20.fig$er0$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag0",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt20.fig$er1$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag1",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt20.fig$er2$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag2",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt20.fig$er3$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag3",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt20.fig$er4$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag4",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt20.fig$er5$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag5",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt20.fig$er6$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag6",ylim=c(-1.0,1.0));abline(h=0,col="red")
plot(tt20.fig$er7$gam,select=1,scheme=1,cex.lab=2.5,cex.axis=2.5,cex.main=2.8,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="Lag7",ylim=c(-1.0,1.0));abline(h=0,col="red")
dev.off()
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#질환별로 도시별 노출-반응 그림 그리기 
sido.gam.func<-function(dataset,text,ylimin,ylimax){
  dd<-dataset
  sido01<-dd %>% filter(area=="서울");sido02<-dd %>% filter(area=="부산")
  sido03<-dd %>% filter(area=="대구");sido04<-dd %>% filter(area=="인천")
  sido05<-dd %>% filter(area=="광주");sido06<-dd %>% filter(area=="대전")
  sido07<-dd %>% filter(area=="울산");sido08<-dd %>% filter(area=="경기")
  sido09<-dd %>% filter(area=="강원");sido10<-dd %>% filter(area=="충북")
  sido11<-dd %>% filter(area=="충남");sido12<-dd %>% filter(area=="전북")
  sido13<-dd %>% filter(area=="전남");sido14<-dd %>% filter(area=="경북")
  sido15<-dd %>% filter(area=="경남")
  
  gam01<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido01,family="quasipoisson")
  gam02<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido02,family="quasipoisson")
  gam03<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido03,family="quasipoisson")
  gam04<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido04,family="quasipoisson")
  gam05<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido05,family="quasipoisson")
  gam06<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido06,family="quasipoisson")
  gam07<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido07,family="quasipoisson")
  gam08<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido08,family="quasipoisson")
  gam09<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido09,family="quasipoisson")
  gam10<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido10,family="quasipoisson")
  gam11<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido11,family="quasipoisson")
  gam12<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido12,family="quasipoisson")
  gam13<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido13,family="quasipoisson")
  gam14<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido14,family="quasipoisson")
  gam15<-gam(outcome~s(maxtemp_lag0)+s(time,k=2*5)+s(meanhumi_lag0)+dow,data=sido15,family="quasipoisson")
  
  png.save<-paste0("D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\20210928\\figure\\",text,".png")
  
  png(file=png.save,width=1600, height=800)
  par(mfrow=c(3,5),mar=c(5,5,5,5),oma=c(3,3,2,3))
  plot(gam01,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="서울",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam02,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="부산",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam03,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="대구",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam04,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="인천",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam05,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="광주",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam06,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="대전",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam07,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="울산",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam08,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="경기",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam09,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="강원",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam10,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="충북",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam11,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="충남",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam12,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="전북",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam13,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="전남",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam14,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="경북",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  plot(gam15,select=1,scheme=1,cex.lab=2.3,cex.axis=2.3,cex.main=2.5,xlab="Maxtimum Temerature(°C)",ylab="log RR" ,main="경남",ylim=c(ylimin,ylimax));abline(h=0,col="red")
  dev.off()
}

sido.gam.func(tt01,"도시별온열질환관련",-1.0,1.0)
sido.gam.func(tt03,"도시별열성경련",-1.0,1.0)
sido.gam.func(tt04,"도시별수족구병",-1.0,1.0)
sido.gam.func(tt12,"도시별아토피피부염",-2.5,2.5)
sido.gam.func(tt17,"도시별중이염",-1.0,1.0)
sido.gam.func(tt15,"도시별가와사키",-1.5,1.5)
sido.gam.func(tt18,"도시별전체장감염질환",-0.5,0.5)
sido.gam.func(tt20,"도시별캄필로박터장감염",-3.0,3.0)
sido.gam.func(tt05,"도시별전체호흡기",-0.3,0.3)
sido.gam.func(tt11,"도시별폐렴",-0.3,0.3)
sido.gam.func(tt10,"도시별천식",-0.4,0.4)
sido.gam.func(tt08,"도시별상기도감염",-0.3,0.3)
sido.gam.func(tt09,"도시별급성하기도감염",-0.3,0.3)
sido.gam.func(tt06,"도시별기관지염",-0.3,0.3)
sido.gam.func(tt07,"도시별급성세기관지염",-0.3,0.3)

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#내용 수정좀 하기 
##GAMM,모델링, 단일 지연; 온열질환 관련
tt01.res=NULL
tt01.res$fit0<-gamm(outcome~maxtemp_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit1<-gamm(outcome~maxtemp_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit2<-gamm(outcome~maxtemp_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit3<-gamm(outcome~maxtemp_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit4<-gamm(outcome~maxtemp_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit5<-gamm(outcome~maxtemp_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit6<-gamm(outcome~maxtemp_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit7<-gamm(outcome~maxtemp_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 이동평균; 온열질환 관련
tt01.res$fit01<-gamm(outcome~maxtemp_lag01+s(time,k=2*5)+s(meanhumi_lag01)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit02<-gamm(outcome~maxtemp_lag02+s(time,k=2*5)+s(meanhumi_lag02)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit03<-gamm(outcome~maxtemp_lag03+s(time,k=2*5)+s(meanhumi_lag03)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit04<-gamm(outcome~maxtemp_lag04+s(time,k=2*5)+s(meanhumi_lag04)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit05<-gamm(outcome~maxtemp_lag05+s(time,k=2*5)+s(meanhumi_lag05)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit06<-gamm(outcome~maxtemp_lag06+s(time,k=2*5)+s(meanhumi_lag06)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$fit07<-gamm(outcome~maxtemp_lag07+s(time,k=2*5)+s(meanhumi_lag07)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 28도
tt01.res$heat28_0<-gamm(outcome~heat28_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat28_1<-gamm(outcome~heat28_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="quasipoisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat28_2<-gamm(outcome~heat28_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat28_3<-gamm(outcome~heat28_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat28_4<-gamm(outcome~heat28_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat28_5<-gamm(outcome~heat28_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat28_6<-gamm(outcome~heat28_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat28_7<-gamm(outcome~heat28_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 29도
tt01.res$heat29_0<-gamm(outcome~heat29_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat29_1<-gamm(outcome~heat29_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat29_2<-gamm(outcome~heat29_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat29_3<-gamm(outcome~heat29_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat29_4<-gamm(outcome~heat29_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat29_5<-gamm(outcome~heat29_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat29_6<-gamm(outcome~heat29_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat29_7<-gamm(outcome~heat29_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 30도
tt01.res$heat30_0<-gamm(outcome~heat30_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat30_1<-gamm(outcome~heat30_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat30_2<-gamm(outcome~heat30_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat30_3<-gamm(outcome~heat30_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat30_4<-gamm(outcome~heat30_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat30_5<-gamm(outcome~heat30_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat30_6<-gamm(outcome~heat30_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat30_7<-gamm(outcome~heat30_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 31도
tt01.res$heat31_0<-gamm(outcome~heat31_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat31_1<-gamm(outcome~heat31_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat31_2<-gamm(outcome~heat31_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat31_3<-gamm(outcome~heat31_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat31_4<-gamm(outcome~heat31_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat31_5<-gamm(outcome~heat31_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat31_6<-gamm(outcome~heat31_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat31_7<-gamm(outcome~heat31_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 32도
tt01.res$heat32_0<-gamm(outcome~heat32_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat32_1<-gamm(outcome~heat32_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat32_2<-gamm(outcome~heat32_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat32_3<-gamm(outcome~heat32_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat32_4<-gamm(outcome~heat32_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat32_5<-gamm(outcome~heat32_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat32_6<-gamm(outcome~heat32_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat32_7<-gamm(outcome~heat32_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

##GAMM,모델링, 폭염 33도
tt01.res$heat33_0<-gamm(outcome~heat33_lag0+s(time,k=2*5)+s(meanhumi_lag0)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat33_1<-gamm(outcome~heat33_lag1+s(time,k=2*5)+s(meanhumi_lag1)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat33_2<-gamm(outcome~heat33_lag2+s(time,k=2*5)+s(meanhumi_lag2)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat33_3<-gamm(outcome~heat33_lag3+s(time,k=2*5)+s(meanhumi_lag3)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat33_4<-gamm(outcome~heat33_lag4+s(time,k=2*5)+s(meanhumi_lag4)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat33_5<-gamm(outcome~heat33_lag5+s(time,k=2*5)+s(meanhumi_lag5)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat33_6<-gamm(outcome~heat33_lag6+s(time,k=2*5)+s(meanhumi_lag6)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))
tt01.res$heat33_7<-gamm(outcome~heat33_lag7+s(time,k=2*5)+s(meanhumi_lag7)+dow,random=list(area=~1),data=tt01,family="poisson",control=lmeControl(opt="optim",msMaxIter=10000))

tt01.single<-as.data.frame(rbind(summary(tt01.res$fit0$gam)$p.table[2,],summary(tt01.res$fit1$gam)$p.table[2,],
                                 summary(tt01.res$fit2$gam)$p.table[2,],summary(tt01.res$fit3$gam)$p.table[2,],
                                 summary(tt01.res$fit4$gam)$p.table[2,],summary(tt01.res$fit5$gam)$p.table[2,],
                                 summary(tt01.res$fit6$gam)$p.table[2,],summary(tt01.res$fit7$gam)$p.table[2,]))

tt01.moving<-as.data.frame(rbind(summary(tt01.res$fit01$gam)$p.table[2,],
                                 summary(tt01.res$fit02$gam)$p.table[2,],summary(tt01.res$fit03$gam)$p.table[2,],
                                 summary(tt01.res$fit04$gam)$p.table[2,],summary(tt01.res$fit05$gam)$p.table[2,],
                                 summary(tt01.res$fit06$gam)$p.table[2,],summary(tt01.res$fit07$gam)$p.table[2,]))

tt01.heat28<-as.data.frame(rbind(summary(tt01.res$heat28_0$gam)$p.table[2,],summary(tt01.res$heat28_1$gam)$p.table[2,],
                                 summary(tt01.res$heat28_2$gam)$p.table[2,],summary(tt01.res$heat28_3$gam)$p.table[2,],
                                 summary(tt01.res$heat28_4$gam)$p.table[2,],summary(tt01.res$heat28_5$gam)$p.table[2,],
                                 summary(tt01.res$heat28_6$gam)$p.table[2,],summary(tt01.res$heat28_7$gam)$p.table[2,]))

tt01.heat29<-as.data.frame(rbind(summary(tt01.res$heat29_0$gam)$p.table[2,],summary(tt01.res$heat29_1$gam)$p.table[2,],
                                 summary(tt01.res$heat29_2$gam)$p.table[2,],summary(tt01.res$heat29_3$gam)$p.table[2,],
                                 summary(tt01.res$heat29_4$gam)$p.table[2,],summary(tt01.res$heat29_5$gam)$p.table[2,],
                                 summary(tt01.res$heat29_6$gam)$p.table[2,],summary(tt01.res$heat29_7$gam)$p.table[2,]))

tt01.heat30<-as.data.frame(rbind(summary(tt01.res$heat30_0$gam)$p.table[2,],summary(tt01.res$heat30_1$gam)$p.table[2,],
                                 summary(tt01.res$heat30_2$gam)$p.table[2,],summary(tt01.res$heat30_3$gam)$p.table[2,],
                                 summary(tt01.res$heat30_4$gam)$p.table[2,],summary(tt01.res$heat30_5$gam)$p.table[2,],
                                 summary(tt01.res$heat30_6$gam)$p.table[2,],summary(tt01.res$heat30_7$gam)$p.table[2,]))

tt01.heat31<-as.data.frame(rbind(summary(tt01.res$heat31_0$gam)$p.table[2,],summary(tt01.res$heat31_1$gam)$p.table[2,],
                                 summary(tt01.res$heat31_2$gam)$p.table[2,],summary(tt01.res$heat31_3$gam)$p.table[2,],
                                 summary(tt01.res$heat31_4$gam)$p.table[2,],summary(tt01.res$heat31_5$gam)$p.table[2,],
                                 summary(tt01.res$heat31_6$gam)$p.table[2,],summary(tt01.res$heat31_7$gam)$p.table[2,]))

tt01.heat32<-as.data.frame(rbind(summary(tt01.res$heat32_0$gam)$p.table[2,],summary(tt01.res$heat32_1$gam)$p.table[2,],
                                 summary(tt01.res$heat32_2$gam)$p.table[2,],summary(tt01.res$heat32_3$gam)$p.table[2,],
                                 summary(tt01.res$heat32_4$gam)$p.table[2,],summary(tt01.res$heat32_5$gam)$p.table[2,],
                                 summary(tt01.res$heat32_6$gam)$p.table[2,],summary(tt01.res$heat32_7$gam)$p.table[2,]))

tt01.heat33<-as.data.frame(rbind(summary(tt01.res$heat33_0$gam)$p.table[2,],summary(tt01.res$heat33_1$gam)$p.table[2,],
                                 summary(tt01.res$heat33_2$gam)$p.table[2,],summary(tt01.res$heat33_3$gam)$p.table[2,],
                                 summary(tt01.res$heat33_4$gam)$p.table[2,],summary(tt01.res$heat33_5$gam)$p.table[2,],
                                 summary(tt01.res$heat33_6$gam)$p.table[2,],summary(tt01.res$heat33_7$gam)$p.table[2,]))


tt01.single$label="single";tt01.single$lag=paste0("lag" ,1:8-1)
tt01.moving$label="moving";tt01.moving$lag=paste0("lag0",1:7)

tt01.heat28$label="single";tt01.heat28$lag=paste0("lag" ,1:8-1)
tt01.heat29$label="single";tt01.heat29$lag=paste0("lag" ,1:8-1)
tt01.heat30$label="single";tt01.heat30$lag=paste0("lag" ,1:8-1)
tt01.heat31$label="single";tt01.heat31$lag=paste0("lag" ,1:8-1)
tt01.heat32$label="single";tt01.heat32$lag=paste0("lag" ,1:8-1)
tt01.heat33$label="single";tt01.heat33$lag=paste0("lag" ,1:8-1)

tt01.single$exposure="maxT"
tt01.moving$exposure="maxT"
tt01.heat28$exposure="heat28"
tt01.heat29$exposure="heat29"
tt01.heat30$exposure="heat30"
tt01.heat31$exposure="heat31"
tt01.heat32$exposure="heat32"
tt01.heat33$exposure="heat33"

#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#

#First stage, 시도별 분석
#질환별로 도시별 노출-반응 그림 그리기 

sido.gam<-function(dataset,SIDO){
  
  dd<-dataset %>% filter(area==SIDO)
  
  #일 최고기온 단일지연
  fit_lag0<-gam(outcome~maxtemp_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dd,family="quasipoisson")
  fit_lag1<-gam(outcome~maxtemp_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dd,family="quasipoisson")
  fit_lag2<-gam(outcome~maxtemp_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dd,family="quasipoisson")
  fit_lag3<-gam(outcome~maxtemp_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dd,family="quasipoisson")
  fit_lag4<-gam(outcome~maxtemp_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dd,family="quasipoisson")
  fit_lag5<-gam(outcome~maxtemp_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dd,family="quasipoisson")
  fit_lag6<-gam(outcome~maxtemp_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dd,family="quasipoisson")
  fit_lag7<-gam(outcome~maxtemp_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dd,family="quasipoisson")
  
  #일 최고기온 이동평균 
  fit_lag01<-gam(outcome~maxtemp_lag01+pm25_lag01+s(time,k=2*5)+s(meanhumi_lag01)+s(windspeed_lag01)+dow,data=dd,family="quasipoisson")
  fit_lag02<-gam(outcome~maxtemp_lag02+pm25_lag02+s(time,k=2*5)+s(meanhumi_lag02)+s(windspeed_lag02)+dow,data=dd,family="quasipoisson")
  fit_lag03<-gam(outcome~maxtemp_lag03+pm25_lag03+s(time,k=2*5)+s(meanhumi_lag03)+s(windspeed_lag03)+dow,data=dd,family="quasipoisson")
  fit_lag04<-gam(outcome~maxtemp_lag04+pm25_lag04+s(time,k=2*5)+s(meanhumi_lag04)+s(windspeed_lag04)+dow,data=dd,family="quasipoisson")
  fit_lag05<-gam(outcome~maxtemp_lag05+pm25_lag05+s(time,k=2*5)+s(meanhumi_lag05)+s(windspeed_lag05)+dow,data=dd,family="quasipoisson")
  fit_lag06<-gam(outcome~maxtemp_lag06+pm25_lag06+s(time,k=2*5)+s(meanhumi_lag06)+s(windspeed_lag06)+dow,data=dd,family="quasipoisson")
  fit_lag07<-gam(outcome~maxtemp_lag07+pm25_lag07+s(time,k=2*5)+s(meanhumi_lag07)+s(windspeed_lag07)+dow,data=dd,family="quasipoisson")
  
  #폭염 28도 단일지연
  heat28_lag0<-gam(outcome~heat28_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dd,family="quasipoisson")
  heat28_lag1<-gam(outcome~heat28_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dd,family="quasipoisson")
  heat28_lag2<-gam(outcome~heat28_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dd,family="quasipoisson")
  heat28_lag3<-gam(outcome~heat28_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dd,family="quasipoisson")
  heat28_lag4<-gam(outcome~heat28_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dd,family="quasipoisson")
  heat28_lag5<-gam(outcome~heat28_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dd,family="quasipoisson")
  heat28_lag6<-gam(outcome~heat28_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dd,family="quasipoisson")
  heat28_lag7<-gam(outcome~heat28_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dd,family="quasipoisson")
  
  #폭염 29도 단일지연
  heat29_lag0<-gam(outcome~heat29_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dd,family="quasipoisson")
  heat29_lag1<-gam(outcome~heat29_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dd,family="quasipoisson")
  heat29_lag2<-gam(outcome~heat29_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dd,family="quasipoisson")
  heat29_lag3<-gam(outcome~heat29_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dd,family="quasipoisson")
  heat29_lag4<-gam(outcome~heat29_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dd,family="quasipoisson")
  heat29_lag5<-gam(outcome~heat29_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dd,family="quasipoisson")
  heat29_lag6<-gam(outcome~heat29_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dd,family="quasipoisson")
  heat29_lag7<-gam(outcome~heat29_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dd,family="quasipoisson")
  
  #폭염 30도 단일지연
  heat30_lag0<-gam(outcome~heat30_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dd,family="quasipoisson")
  heat30_lag1<-gam(outcome~heat30_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dd,family="quasipoisson")
  heat30_lag2<-gam(outcome~heat30_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dd,family="quasipoisson")
  heat30_lag3<-gam(outcome~heat30_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dd,family="quasipoisson")
  heat30_lag4<-gam(outcome~heat30_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dd,family="quasipoisson")
  heat30_lag5<-gam(outcome~heat30_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dd,family="quasipoisson")
  heat30_lag6<-gam(outcome~heat30_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dd,family="quasipoisson")
  heat30_lag7<-gam(outcome~heat30_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dd,family="quasipoisson")
  
  #폭염 31도 단일지연
  heat31_lag0<-gam(outcome~heat31_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dd,family="quasipoisson")
  heat31_lag1<-gam(outcome~heat31_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dd,family="quasipoisson")
  heat31_lag2<-gam(outcome~heat31_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dd,family="quasipoisson")
  heat31_lag3<-gam(outcome~heat31_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dd,family="quasipoisson")
  heat31_lag4<-gam(outcome~heat31_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dd,family="quasipoisson")
  heat31_lag5<-gam(outcome~heat31_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dd,family="quasipoisson")
  heat31_lag6<-gam(outcome~heat31_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dd,family="quasipoisson")
  heat31_lag7<-gam(outcome~heat31_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dd,family="quasipoisson")
  
  #폭염 32도 단일지연
  heat32_lag0<-gam(outcome~heat32_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dd,family="quasipoisson")
  heat32_lag1<-gam(outcome~heat32_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dd,family="quasipoisson")
  heat32_lag2<-gam(outcome~heat32_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dd,family="quasipoisson")
  heat32_lag3<-gam(outcome~heat32_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dd,family="quasipoisson")
  heat32_lag4<-gam(outcome~heat32_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dd,family="quasipoisson")
  heat32_lag5<-gam(outcome~heat32_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dd,family="quasipoisson")
  heat32_lag6<-gam(outcome~heat32_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dd,family="quasipoisson")
  heat32_lag7<-gam(outcome~heat32_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dd,family="quasipoisson")
  
  #폭염 33도 단일지연
  heat33_lag0<-gam(outcome~heat33_lag0+pm25_lag0+s(time,k=2*5)+s(meanhumi_lag0)+s(windspeed_lag0)+dow,data=dd,family="quasipoisson")
  heat33_lag1<-gam(outcome~heat33_lag1+pm25_lag1+s(time,k=2*5)+s(meanhumi_lag1)+s(windspeed_lag1)+dow,data=dd,family="quasipoisson")
  heat33_lag2<-gam(outcome~heat33_lag2+pm25_lag2+s(time,k=2*5)+s(meanhumi_lag2)+s(windspeed_lag2)+dow,data=dd,family="quasipoisson")
  heat33_lag3<-gam(outcome~heat33_lag3+pm25_lag3+s(time,k=2*5)+s(meanhumi_lag3)+s(windspeed_lag3)+dow,data=dd,family="quasipoisson")
  heat33_lag4<-gam(outcome~heat33_lag4+pm25_lag4+s(time,k=2*5)+s(meanhumi_lag4)+s(windspeed_lag4)+dow,data=dd,family="quasipoisson")
  heat33_lag5<-gam(outcome~heat33_lag5+pm25_lag5+s(time,k=2*5)+s(meanhumi_lag5)+s(windspeed_lag5)+dow,data=dd,family="quasipoisson")
  heat33_lag6<-gam(outcome~heat33_lag6+pm25_lag6+s(time,k=2*5)+s(meanhumi_lag6)+s(windspeed_lag6)+dow,data=dd,family="quasipoisson")
  heat33_lag7<-gam(outcome~heat33_lag7+pm25_lag7+s(time,k=2*5)+s(meanhumi_lag7)+s(windspeed_lag7)+dow,data=dd,family="quasipoisson")
  
  
  #result table
  fit.tb0<-as.data.frame(cbind(summary(fit_lag0)$p.table[2:3,],gcv=fit_lag0$gcv.ubre,deviance=fit_lag0$deviance,r_sq=summary(fit_lag0)$r.sq))
  fit.tb1<-as.data.frame(cbind(summary(fit_lag1)$p.table[2:3,],gcv=fit_lag1$gcv.ubre,deviance=fit_lag1$deviance,r_sq=summary(fit_lag1)$r.sq))
  fit.tb2<-as.data.frame(cbind(summary(fit_lag2)$p.table[2:3,],gcv=fit_lag2$gcv.ubre,deviance=fit_lag2$deviance,r_sq=summary(fit_lag2)$r.sq))
  fit.tb3<-as.data.frame(cbind(summary(fit_lag3)$p.table[2:3,],gcv=fit_lag3$gcv.ubre,deviance=fit_lag3$deviance,r_sq=summary(fit_lag3)$r.sq))
  fit.tb4<-as.data.frame(cbind(summary(fit_lag4)$p.table[2:3,],gcv=fit_lag4$gcv.ubre,deviance=fit_lag4$deviance,r_sq=summary(fit_lag4)$r.sq))
  fit.tb5<-as.data.frame(cbind(summary(fit_lag5)$p.table[2:3,],gcv=fit_lag5$gcv.ubre,deviance=fit_lag5$deviance,r_sq=summary(fit_lag5)$r.sq))
  fit.tb6<-as.data.frame(cbind(summary(fit_lag6)$p.table[2:3,],gcv=fit_lag6$gcv.ubre,deviance=fit_lag6$deviance,r_sq=summary(fit_lag6)$r.sq))
  fit.tb7<-as.data.frame(cbind(summary(fit_lag7)$p.table[2:3,],gcv=fit_lag7$gcv.ubre,deviance=fit_lag7$deviance,r_sq=summary(fit_lag7)$r.sq))
  
  fit.tb01<-as.data.frame(cbind(summary(fit_lag01)$p.table[2:3,],gcv=fit_lag01$gcv.ubre,deviance=fit_lag01$deviance,r_sq=summary(fit_lag01)$r.sq))
  fit.tb02<-as.data.frame(cbind(summary(fit_lag02)$p.table[2:3,],gcv=fit_lag02$gcv.ubre,deviance=fit_lag02$deviance,r_sq=summary(fit_lag02)$r.sq))
  fit.tb03<-as.data.frame(cbind(summary(fit_lag03)$p.table[2:3,],gcv=fit_lag03$gcv.ubre,deviance=fit_lag03$deviance,r_sq=summary(fit_lag03)$r.sq))
  fit.tb04<-as.data.frame(cbind(summary(fit_lag04)$p.table[2:3,],gcv=fit_lag04$gcv.ubre,deviance=fit_lag04$deviance,r_sq=summary(fit_lag04)$r.sq))
  fit.tb05<-as.data.frame(cbind(summary(fit_lag05)$p.table[2:3,],gcv=fit_lag05$gcv.ubre,deviance=fit_lag05$deviance,r_sq=summary(fit_lag05)$r.sq))
  fit.tb06<-as.data.frame(cbind(summary(fit_lag06)$p.table[2:3,],gcv=fit_lag06$gcv.ubre,deviance=fit_lag06$deviance,r_sq=summary(fit_lag06)$r.sq))
  fit.tb07<-as.data.frame(cbind(summary(fit_lag07)$p.table[2:3,],gcv=fit_lag07$gcv.ubre,deviance=fit_lag07$deviance,r_sq=summary(fit_lag07)$r.sq))
  
  heat28.tb0<-as.data.frame(cbind(summary(heat28_lag0)$p.table[2:3,],gcv=heat28_lag0$gcv.ubre,deviance=heat28_lag0$deviance,r_sq=summary(heat28_lag0)$r.sq))
  heat28.tb1<-as.data.frame(cbind(summary(heat28_lag1)$p.table[2:3,],gcv=heat28_lag1$gcv.ubre,deviance=heat28_lag1$deviance,r_sq=summary(heat28_lag1)$r.sq))
  heat28.tb2<-as.data.frame(cbind(summary(heat28_lag2)$p.table[2:3,],gcv=heat28_lag2$gcv.ubre,deviance=heat28_lag2$deviance,r_sq=summary(heat28_lag2)$r.sq))
  heat28.tb3<-as.data.frame(cbind(summary(heat28_lag3)$p.table[2:3,],gcv=heat28_lag3$gcv.ubre,deviance=heat28_lag3$deviance,r_sq=summary(heat28_lag3)$r.sq))
  heat28.tb4<-as.data.frame(cbind(summary(heat28_lag4)$p.table[2:3,],gcv=heat28_lag4$gcv.ubre,deviance=heat28_lag4$deviance,r_sq=summary(heat28_lag4)$r.sq))
  heat28.tb5<-as.data.frame(cbind(summary(heat28_lag5)$p.table[2:3,],gcv=heat28_lag5$gcv.ubre,deviance=heat28_lag5$deviance,r_sq=summary(heat28_lag5)$r.sq))
  heat28.tb6<-as.data.frame(cbind(summary(heat28_lag6)$p.table[2:3,],gcv=heat28_lag6$gcv.ubre,deviance=heat28_lag6$deviance,r_sq=summary(heat28_lag6)$r.sq))
  heat28.tb7<-as.data.frame(cbind(summary(heat28_lag7)$p.table[2:3,],gcv=heat28_lag7$gcv.ubre,deviance=heat28_lag7$deviance,r_sq=summary(heat28_lag7)$r.sq))
  
  heat29.tb0<-as.data.frame(cbind(summary(heat29_lag0)$p.table[2:3,],gcv=heat29_lag0$gcv.ubre,deviance=heat29_lag0$deviance,r_sq=summary(heat29_lag0)$r.sq))
  heat29.tb1<-as.data.frame(cbind(summary(heat29_lag1)$p.table[2:3,],gcv=heat29_lag1$gcv.ubre,deviance=heat29_lag1$deviance,r_sq=summary(heat29_lag1)$r.sq))
  heat29.tb2<-as.data.frame(cbind(summary(heat29_lag2)$p.table[2:3,],gcv=heat29_lag2$gcv.ubre,deviance=heat29_lag2$deviance,r_sq=summary(heat29_lag2)$r.sq))
  heat29.tb3<-as.data.frame(cbind(summary(heat29_lag3)$p.table[2:3,],gcv=heat29_lag3$gcv.ubre,deviance=heat29_lag3$deviance,r_sq=summary(heat29_lag3)$r.sq))
  heat29.tb4<-as.data.frame(cbind(summary(heat29_lag4)$p.table[2:3,],gcv=heat29_lag4$gcv.ubre,deviance=heat29_lag4$deviance,r_sq=summary(heat29_lag4)$r.sq))
  heat29.tb5<-as.data.frame(cbind(summary(heat29_lag5)$p.table[2:3,],gcv=heat29_lag5$gcv.ubre,deviance=heat29_lag5$deviance,r_sq=summary(heat29_lag5)$r.sq))
  heat29.tb6<-as.data.frame(cbind(summary(heat29_lag6)$p.table[2:3,],gcv=heat29_lag6$gcv.ubre,deviance=heat29_lag6$deviance,r_sq=summary(heat29_lag6)$r.sq))
  heat29.tb7<-as.data.frame(cbind(summary(heat29_lag7)$p.table[2:3,],gcv=heat29_lag7$gcv.ubre,deviance=heat29_lag7$deviance,r_sq=summary(heat29_lag7)$r.sq))
  
  heat30.tb0<-as.data.frame(cbind(summary(heat30_lag0)$p.table[2:3,],gcv=heat30_lag0$gcv.ubre,deviance=heat30_lag0$deviance,r_sq=summary(heat30_lag0)$r.sq))
  heat30.tb1<-as.data.frame(cbind(summary(heat30_lag1)$p.table[2:3,],gcv=heat30_lag1$gcv.ubre,deviance=heat30_lag1$deviance,r_sq=summary(heat30_lag1)$r.sq))
  heat30.tb2<-as.data.frame(cbind(summary(heat30_lag2)$p.table[2:3,],gcv=heat30_lag2$gcv.ubre,deviance=heat30_lag2$deviance,r_sq=summary(heat30_lag2)$r.sq))
  heat30.tb3<-as.data.frame(cbind(summary(heat30_lag3)$p.table[2:3,],gcv=heat30_lag3$gcv.ubre,deviance=heat30_lag3$deviance,r_sq=summary(heat30_lag3)$r.sq))
  heat30.tb4<-as.data.frame(cbind(summary(heat30_lag4)$p.table[2:3,],gcv=heat30_lag4$gcv.ubre,deviance=heat30_lag4$deviance,r_sq=summary(heat30_lag4)$r.sq))
  heat30.tb5<-as.data.frame(cbind(summary(heat30_lag5)$p.table[2:3,],gcv=heat30_lag5$gcv.ubre,deviance=heat30_lag5$deviance,r_sq=summary(heat30_lag5)$r.sq))
  heat30.tb6<-as.data.frame(cbind(summary(heat30_lag6)$p.table[2:3,],gcv=heat30_lag6$gcv.ubre,deviance=heat30_lag6$deviance,r_sq=summary(heat30_lag6)$r.sq))
  heat30.tb7<-as.data.frame(cbind(summary(heat30_lag7)$p.table[2:3,],gcv=heat30_lag7$gcv.ubre,deviance=heat30_lag7$deviance,r_sq=summary(heat30_lag7)$r.sq))
  
  heat31.tb0<-as.data.frame(cbind(summary(heat31_lag0)$p.table[2:3,],gcv=heat31_lag0$gcv.ubre,deviance=heat31_lag0$deviance,r_sq=summary(heat31_lag0)$r.sq))
  heat31.tb1<-as.data.frame(cbind(summary(heat31_lag1)$p.table[2:3,],gcv=heat31_lag1$gcv.ubre,deviance=heat31_lag1$deviance,r_sq=summary(heat31_lag1)$r.sq))
  heat31.tb2<-as.data.frame(cbind(summary(heat31_lag2)$p.table[2:3,],gcv=heat31_lag2$gcv.ubre,deviance=heat31_lag2$deviance,r_sq=summary(heat31_lag2)$r.sq))
  heat31.tb3<-as.data.frame(cbind(summary(heat31_lag3)$p.table[2:3,],gcv=heat31_lag3$gcv.ubre,deviance=heat31_lag3$deviance,r_sq=summary(heat31_lag3)$r.sq))
  heat31.tb4<-as.data.frame(cbind(summary(heat31_lag4)$p.table[2:3,],gcv=heat31_lag4$gcv.ubre,deviance=heat31_lag4$deviance,r_sq=summary(heat31_lag4)$r.sq))
  heat31.tb5<-as.data.frame(cbind(summary(heat31_lag5)$p.table[2:3,],gcv=heat31_lag5$gcv.ubre,deviance=heat31_lag5$deviance,r_sq=summary(heat31_lag5)$r.sq))
  heat31.tb6<-as.data.frame(cbind(summary(heat31_lag6)$p.table[2:3,],gcv=heat31_lag6$gcv.ubre,deviance=heat31_lag6$deviance,r_sq=summary(heat31_lag6)$r.sq))
  heat31.tb7<-as.data.frame(cbind(summary(heat31_lag7)$p.table[2:3,],gcv=heat31_lag7$gcv.ubre,deviance=heat31_lag7$deviance,r_sq=summary(heat31_lag7)$r.sq))
  
  heat32.tb0<-as.data.frame(cbind(summary(heat32_lag0)$p.table[2:3,],gcv=heat32_lag0$gcv.ubre,deviance=heat32_lag0$deviance,r_sq=summary(heat32_lag0)$r.sq))
  heat32.tb1<-as.data.frame(cbind(summary(heat32_lag1)$p.table[2:3,],gcv=heat32_lag1$gcv.ubre,deviance=heat32_lag1$deviance,r_sq=summary(heat32_lag1)$r.sq))
  heat32.tb2<-as.data.frame(cbind(summary(heat32_lag2)$p.table[2:3,],gcv=heat32_lag2$gcv.ubre,deviance=heat32_lag2$deviance,r_sq=summary(heat32_lag2)$r.sq))
  heat32.tb3<-as.data.frame(cbind(summary(heat32_lag3)$p.table[2:3,],gcv=heat32_lag3$gcv.ubre,deviance=heat32_lag3$deviance,r_sq=summary(heat32_lag3)$r.sq))
  heat32.tb4<-as.data.frame(cbind(summary(heat32_lag4)$p.table[2:3,],gcv=heat32_lag4$gcv.ubre,deviance=heat32_lag4$deviance,r_sq=summary(heat32_lag4)$r.sq))
  heat32.tb5<-as.data.frame(cbind(summary(heat32_lag5)$p.table[2:3,],gcv=heat32_lag5$gcv.ubre,deviance=heat32_lag5$deviance,r_sq=summary(heat32_lag5)$r.sq))
  heat32.tb6<-as.data.frame(cbind(summary(heat32_lag6)$p.table[2:3,],gcv=heat32_lag6$gcv.ubre,deviance=heat32_lag6$deviance,r_sq=summary(heat32_lag6)$r.sq))
  heat32.tb7<-as.data.frame(cbind(summary(heat32_lag7)$p.table[2:3,],gcv=heat32_lag7$gcv.ubre,deviance=heat32_lag7$deviance,r_sq=summary(heat32_lag7)$r.sq))
  
  heat33.tb0<-as.data.frame(cbind(summary(heat33_lag0)$p.table[2:3,],gcv=heat33_lag0$gcv.ubre,deviance=heat33_lag0$deviance,r_sq=summary(heat33_lag0)$r.sq))
  heat33.tb1<-as.data.frame(cbind(summary(heat33_lag1)$p.table[2:3,],gcv=heat33_lag1$gcv.ubre,deviance=heat33_lag1$deviance,r_sq=summary(heat33_lag1)$r.sq))
  heat33.tb2<-as.data.frame(cbind(summary(heat33_lag2)$p.table[2:3,],gcv=heat33_lag2$gcv.ubre,deviance=heat33_lag2$deviance,r_sq=summary(heat33_lag2)$r.sq))
  heat33.tb3<-as.data.frame(cbind(summary(heat33_lag3)$p.table[2:3,],gcv=heat33_lag3$gcv.ubre,deviance=heat33_lag3$deviance,r_sq=summary(heat33_lag3)$r.sq))
  heat33.tb4<-as.data.frame(cbind(summary(heat33_lag4)$p.table[2:3,],gcv=heat33_lag4$gcv.ubre,deviance=heat33_lag4$deviance,r_sq=summary(heat33_lag4)$r.sq))
  heat33.tb5<-as.data.frame(cbind(summary(heat33_lag5)$p.table[2:3,],gcv=heat33_lag5$gcv.ubre,deviance=heat33_lag5$deviance,r_sq=summary(heat33_lag5)$r.sq))
  heat33.tb6<-as.data.frame(cbind(summary(heat33_lag6)$p.table[2:3,],gcv=heat33_lag6$gcv.ubre,deviance=heat33_lag6$deviance,r_sq=summary(heat33_lag6)$r.sq))
  heat33.tb7<-as.data.frame(cbind(summary(heat33_lag7)$p.table[2:3,],gcv=heat33_lag7$gcv.ubre,deviance=heat33_lag7$deviance,r_sq=summary(heat33_lag7)$r.sq))
  
  fit.tb0$label=row.names(fit.tb0);fit.tb1$label=row.names(fit.tb1)
  fit.tb2$label=row.names(fit.tb2);fit.tb3$label=row.names(fit.tb3)
  fit.tb4$label=row.names(fit.tb4);fit.tb5$label=row.names(fit.tb5)
  fit.tb6$label=row.names(fit.tb6);fit.tb7$label=row.names(fit.tb7)
  
  fit.tb01$label=row.names(fit.tb01);fit.tb02$label=row.names(fit.tb02)
  fit.tb03$label=row.names(fit.tb03);fit.tb04$label=row.names(fit.tb04)
  fit.tb05$label=row.names(fit.tb05);fit.tb06$label=row.names(fit.tb06)
  fit.tb07$label=row.names(fit.tb07)
  
  heat28.tb0$label=row.names(heat28.tb0);heat28.tb1$label=row.names(heat28.tb1)
  heat28.tb2$label=row.names(heat28.tb2);heat28.tb3$label=row.names(heat28.tb3)
  heat28.tb4$label=row.names(heat28.tb4);heat28.tb5$label=row.names(heat28.tb5)
  heat28.tb6$label=row.names(heat28.tb6);heat28.tb7$label=row.names(heat28.tb7)
  
  heat29.tb0$label=row.names(heat29.tb0);heat29.tb1$label=row.names(heat29.tb1)
  heat29.tb2$label=row.names(heat29.tb2);heat29.tb3$label=row.names(heat29.tb3)
  heat29.tb4$label=row.names(heat29.tb4);heat29.tb5$label=row.names(heat29.tb5)
  heat29.tb6$label=row.names(heat29.tb6);heat29.tb7$label=row.names(heat29.tb7)
  
  heat30.tb0$label=row.names(heat30.tb0);heat30.tb1$label=row.names(heat30.tb1)
  heat30.tb2$label=row.names(heat30.tb2);heat30.tb3$label=row.names(heat30.tb3)
  heat30.tb4$label=row.names(heat30.tb4);heat30.tb5$label=row.names(heat30.tb5)
  heat30.tb6$label=row.names(heat30.tb6);heat30.tb7$label=row.names(heat30.tb7)
  
  heat31.tb0$label=row.names(heat31.tb0);heat31.tb1$label=row.names(heat31.tb1)
  heat31.tb2$label=row.names(heat31.tb2);heat31.tb3$label=row.names(heat31.tb3)
  heat31.tb4$label=row.names(heat31.tb4);heat31.tb5$label=row.names(heat31.tb5)
  heat31.tb6$label=row.names(heat31.tb6);heat31.tb7$label=row.names(heat31.tb7)
  
  heat32.tb0$label=row.names(heat32.tb0);heat32.tb1$label=row.names(heat32.tb1)
  heat32.tb2$label=row.names(heat32.tb2);heat32.tb3$label=row.names(heat32.tb3)
  heat32.tb4$label=row.names(heat32.tb4);heat32.tb5$label=row.names(heat32.tb5)
  heat32.tb6$label=row.names(heat32.tb6);heat32.tb7$label=row.names(heat32.tb7)
  
  heat33.tb0$label=row.names(heat33.tb0);heat33.tb1$label=row.names(heat33.tb1)
  heat33.tb2$label=row.names(heat33.tb2);heat33.tb3$label=row.names(heat33.tb3)
  heat33.tb4$label=row.names(heat33.tb4);heat33.tb5$label=row.names(heat33.tb5)
  heat33.tb6$label=row.names(heat33.tb6);heat33.tb7$label=row.names(heat33.tb7)
  
  
  fit.tb.single<-rbind(fit.tb0,fit.tb1,fit.tb2,fit.tb3,fit.tb4,fit.tb5,fit.tb6,fit.tb7)
  fit.tb.moving<-rbind(fit.tb01,fit.tb02,fit.tb03,fit.tb04,fit.tb05,fit.tb06,fit.tb07)
  
  heat28.tb.single<-rbind(heat28.tb0,heat28.tb1,heat28.tb2,heat28.tb3,heat28.tb4,heat28.tb5,heat28.tb6,heat28.tb7)
  heat29.tb.single<-rbind(heat29.tb0,heat29.tb1,heat29.tb2,heat29.tb3,heat29.tb4,heat29.tb5,heat29.tb6,heat29.tb7)
  heat30.tb.single<-rbind(heat30.tb0,heat30.tb1,heat30.tb2,heat30.tb3,heat30.tb4,heat30.tb5,heat30.tb6,heat30.tb7)
  heat31.tb.single<-rbind(heat31.tb0,heat31.tb1,heat31.tb2,heat31.tb3,heat31.tb4,heat31.tb5,heat31.tb6,heat31.tb7)
  heat32.tb.single<-rbind(heat32.tb0,heat32.tb1,heat32.tb2,heat32.tb3,heat32.tb4,heat32.tb5,heat32.tb6,heat32.tb7)
  heat33.tb.single<-rbind(heat33.tb0,heat33.tb1,heat33.tb2,heat33.tb3,heat33.tb4,heat33.tb5,heat33.tb6,heat33.tb7)
  
  fit.tb.single$gubun="single"
  fit.tb.moving$gubun="moving"
  
  heat28.tb.single$gubun="single"
  heat29.tb.single$gubun="single"
  heat30.tb.single$gubun="single"
  heat31.tb.single$gubun="single"
  heat32.tb.single$gubun="single"
  heat33.tb.single$gubun="single"
  
  fit.tb.single$lag=paste0("lag",1:8-1)
  fit.tb.moving$lag=paste0("lag0",1:7)
  
  heat28.tb.single$lag=paste0("lag",1:8-1)
  heat29.tb.single$lag=paste0("lag",1:8-1)
  heat30.tb.single$lag=paste0("lag",1:8-1)
  heat31.tb.single$lag=paste0("lag",1:8-1)
  heat32.tb.single$lag=paste0("lag",1:8-1)
  heat33.tb.single$lag=paste0("lag",1:8-1)
  
  fit.tb.single$exposure=c("maxT","pm25")
  fit.tb.moving$exposure=c("maxT","pm25")
  heat28.tb.single$exposure=c("heat28","pm25")
  heat29.tb.single$exposure=c("heat29","pm25")
  heat30.tb.single$exposure=c("heat30","pm25")
  heat31.tb.single$exposure=c("heat31","pm25")
  heat32.tb.single$exposure=c("heat32","pm25")
  heat33.tb.single$exposure=c("heat33","pm25")
  
  res<-rbind(fit.tb.single,fit.tb.moving,heat28.tb.single,heat29.tb.single,
             heat30.tb.single,heat31.tb.single,heat32.tb.single,heat33.tb.single)
  
  res$sido=SIDO;res}

#온열질환관련 
tt01_sido01<-sido.gam(tt01,"서울");tt01_sido02<-sido.gam(tt01,"부산")
tt01_sido03<-sido.gam(tt01,"대구");tt01_sido04<-sido.gam(tt01,"인천")
tt01_sido05<-sido.gam(tt01,"광주");tt01_sido06<-sido.gam(tt01,"대전")
tt01_sido07<-sido.gam(tt01,"울산");tt01_sido08<-sido.gam(tt01,"경기")
tt01_sido09<-sido.gam(tt01,"강원");tt01_sido10<-sido.gam(tt01,"충북")
tt01_sido11<-sido.gam(tt01,"충남");tt01_sido12<-sido.gam(tt01,"전북")
tt01_sido13<-sido.gam(tt01,"전남");tt01_sido14<-sido.gam(tt01,"경북")
tt01_sido15<-sido.gam(tt01,"경남")

tt01.sido.tb<-rbind(tt01_sido01,tt01_sido02,tt01_sido03,tt01_sido04,tt01_sido05,tt01_sido06,tt01_sido07,tt01_sido08,
                    tt01_sido09,tt01_sido10,tt01_sido11,tt01_sido12,tt01_sido13,tt01_sido14,tt01_sido15)

#열성경련
tt03_sido01<-sido.gam(tt03,"서울");tt03_sido02<-sido.gam(tt03,"부산")
tt03_sido03<-sido.gam(tt03,"대구");tt03_sido04<-sido.gam(tt03,"인천")
tt03_sido05<-sido.gam(tt03,"광주");tt03_sido06<-sido.gam(tt03,"대전")
tt03_sido07<-sido.gam(tt03,"울산");tt03_sido08<-sido.gam(tt03,"경기")
tt03_sido09<-sido.gam(tt03,"강원");tt03_sido10<-sido.gam(tt03,"충북")
tt03_sido11<-sido.gam(tt03,"충남");tt03_sido12<-sido.gam(tt03,"전북")
tt03_sido13<-sido.gam(tt03,"전남");tt03_sido14<-sido.gam(tt03,"경북")
tt03_sido15<-sido.gam(tt03,"경남")

tt03.sido.tb<-rbind(tt03_sido01,tt03_sido02,tt03_sido03,tt03_sido04,tt03_sido05,tt03_sido06,tt03_sido07,tt03_sido08,
                    tt03_sido09,tt03_sido10,tt03_sido11,tt03_sido12,tt03_sido13,tt03_sido14,tt03_sido15)

#수족구병
tt04_sido01<-sido.gam(tt04,"서울");tt04_sido02<-sido.gam(tt04,"부산")
tt04_sido03<-sido.gam(tt04,"대구");tt04_sido04<-sido.gam(tt04,"인천")
tt04_sido05<-sido.gam(tt04,"광주");tt04_sido06<-sido.gam(tt04,"대전")
tt04_sido07<-sido.gam(tt04,"울산");tt04_sido08<-sido.gam(tt04,"경기")
tt04_sido09<-sido.gam(tt04,"강원");tt04_sido10<-sido.gam(tt04,"충북")
tt04_sido11<-sido.gam(tt04,"충남");tt04_sido12<-sido.gam(tt04,"전북")
tt04_sido13<-sido.gam(tt04,"전남");tt04_sido14<-sido.gam(tt04,"경북")
tt04_sido15<-sido.gam(tt04,"경남")

tt04.sido.tb<-rbind(tt04_sido01,tt04_sido02,tt04_sido03,tt04_sido04,tt04_sido05,tt04_sido06,tt04_sido07,tt04_sido08,
                    tt04_sido09,tt04_sido10,tt04_sido11,tt04_sido12,tt04_sido13,tt04_sido14,tt04_sido15)

# write.csv(tt01.sido.tb,file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\온열질환_시도별_TS결과.csv",row.names=F,na="")
# write.csv(tt03.sido.tb,file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\열성경련_시도별_TS결과.csv",row.names=F,na="")
# write.csv(tt04.sido.tb,file="D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\수족구병_시도별_TS결과.csv",row.names=F,na="")
#--------------------------------------------------------------------------#
#--------------------------------------------------------------------------#
#Second stage, 메타분석

tt01.sido.tb<-read.csv("D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\온열질환_시도별_TS결과.csv")
tt03.sido.tb<-read.csv("D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\열성경련_시도별_TS결과.csv")
tt04.sido.tb<-read.csv("D:\\EUMC\\질병관리청\\폭염연구\\공단자료\\수족구병_시도별_TS결과.csv")

tt01.sido.tb$SE=tt01.sido.tb$Std..Error
tt03.sido.tb$SE=tt03.sido.tb$Std..Error
tt04.sido.tb$SE=tt04.sido.tb$Std..Error

#도시별 결과 질병지도 그림 그리기
tt01.sido.tb  %>% filter(label=="maxtemp_lag0")
tt01.sido.tb  %>% filter(label=="maxtemp_lag1")
tt01.sido.tb  %>% filter(label=="maxtemp_lag2")
tt01.sido.tb  %>% filter(label=="maxtemp_lag3")
tt01.sido.tb  %>% filter(label=="maxtemp_lag4")
tt01.sido.tb  %>% filter(label=="maxtemp_lag5")
tt01.sido.tb  %>% filter(label=="maxtemp_lag6")
tt01.sido.tb  %>% filter(label=="maxtemp_lag7")


meta_func<-function(dataset){
  uni0 <- with(dataset %>% filter(label=="maxtemp_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni1 <- with(dataset %>% filter(label=="maxtemp_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni2 <- with(dataset %>% filter(label=="maxtemp_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni3 <- with(dataset %>% filter(label=="maxtemp_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni4 <- with(dataset %>% filter(label=="maxtemp_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni5 <- with(dataset %>% filter(label=="maxtemp_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni6 <- with(dataset %>% filter(label=="maxtemp_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni7 <- with(dataset %>% filter(label=="maxtemp_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.uni<-as.data.frame(rbind(with(uni0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.uni$lag=paste0("lag",1:8-1)
  single.uni$exposure="maxT"
  
  uni01 <- with(dataset %>% filter(label=="maxtemp_lag01"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni02 <- with(dataset %>% filter(label=="maxtemp_lag02"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni03 <- with(dataset %>% filter(label=="maxtemp_lag03"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni04 <- with(dataset %>% filter(label=="maxtemp_lag04"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni05 <- with(dataset %>% filter(label=="maxtemp_lag05"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni06 <- with(dataset %>% filter(label=="maxtemp_lag06"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  uni07 <- with(dataset %>% filter(label=="maxtemp_lag07"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  moving.uni<-as.data.frame(rbind(with(uni01,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni02,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni03,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni04,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni05,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni06,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                  with(uni07,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  moving.uni$lag=paste0("lag0",1:7)
  moving.uni$exposure="maxT"
  
  heat28_0 <- with(dataset %>% filter(label=="heat28_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_1 <- with(dataset %>% filter(label=="heat28_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_2 <- with(dataset %>% filter(label=="heat28_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_3 <- with(dataset %>% filter(label=="heat28_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_4 <- with(dataset %>% filter(label=="heat28_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_5 <- with(dataset %>% filter(label=="heat28_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_6 <- with(dataset %>% filter(label=="heat28_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat28_7 <- with(dataset %>% filter(label=="heat28_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat28<-as.data.frame(rbind(with(heat28_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat28_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat28$lag=paste0("lag",1:8-1)
  single.heat28$exposure="heat28"
  
  heat29_0 <- with(dataset %>% filter(label=="heat29_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_1 <- with(dataset %>% filter(label=="heat29_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_2 <- with(dataset %>% filter(label=="heat29_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_3 <- with(dataset %>% filter(label=="heat29_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_4 <- with(dataset %>% filter(label=="heat29_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_5 <- with(dataset %>% filter(label=="heat29_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_6 <- with(dataset %>% filter(label=="heat29_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat29_7 <- with(dataset %>% filter(label=="heat29_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat29<-as.data.frame(rbind(with(heat29_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat29_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat29$lag=paste0("lag",1:8-1)
  single.heat29$exposure="heat29"
  
  heat30_0 <- with(dataset %>% filter(label=="heat30_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_1 <- with(dataset %>% filter(label=="heat30_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_2 <- with(dataset %>% filter(label=="heat30_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_3 <- with(dataset %>% filter(label=="heat30_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_4 <- with(dataset %>% filter(label=="heat30_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_5 <- with(dataset %>% filter(label=="heat30_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_6 <- with(dataset %>% filter(label=="heat30_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat30_7 <- with(dataset %>% filter(label=="heat30_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat30<-as.data.frame(rbind(with(heat30_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat30_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat30$lag=paste0("lag",1:8-1)
  single.heat30$exposure="heat30"
  
  heat31_0 <- with(dataset %>% filter(label=="heat31_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_1 <- with(dataset %>% filter(label=="heat31_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_2 <- with(dataset %>% filter(label=="heat31_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_3 <- with(dataset %>% filter(label=="heat31_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_4 <- with(dataset %>% filter(label=="heat31_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_5 <- with(dataset %>% filter(label=="heat31_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_6 <- with(dataset %>% filter(label=="heat31_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat31_7 <- with(dataset %>% filter(label=="heat31_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat31<-as.data.frame(rbind(with(heat31_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat31_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat31$lag=paste0("lag",1:8-1)
  single.heat31$exposure="heat31"
  
  heat32_0 <- with(dataset %>% filter(label=="heat32_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_1 <- with(dataset %>% filter(label=="heat32_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_2 <- with(dataset %>% filter(label=="heat32_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_3 <- with(dataset %>% filter(label=="heat32_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_4 <- with(dataset %>% filter(label=="heat32_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_5 <- with(dataset %>% filter(label=="heat32_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_6 <- with(dataset %>% filter(label=="heat32_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat32_7 <- with(dataset %>% filter(label=="heat32_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat32<-as.data.frame(rbind(with(heat32_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat32_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat32$lag=paste0("lag",1:8-1)
  single.heat32$exposure="heat32"
  
  heat33_0 <- with(dataset %>% filter(label=="heat33_lag0"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_1 <- with(dataset %>% filter(label=="heat33_lag1"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_2 <- with(dataset %>% filter(label=="heat33_lag2"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_3 <- with(dataset %>% filter(label=="heat33_lag3"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_4 <- with(dataset %>% filter(label=="heat33_lag4"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_5 <- with(dataset %>% filter(label=="heat33_lag5"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_6 <- with(dataset %>% filter(label=="heat33_lag6"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  heat33_7 <- with(dataset %>% filter(label=="heat33_lag7"),rma(yi=Estimate, sei=SE, slab=sido, measure="RR",digits=5,method="REML"))
  
  single.heat33<-as.data.frame(rbind(with(heat33_0,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_1,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_2,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_3,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_4,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_5,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_6,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2)),
                                     with(heat33_7,data.frame(beta,se,zval,pval,ci.lb,ci.ub,I2,H2))))
  
  single.heat33$lag=paste0("lag",1:8-1)
  single.heat33$exposure="heat33"
  
  rbind(single.uni,moving.uni,
        single.heat28,single.heat29,
        single.heat30,single.heat31,
        single.heat32,single.heat33)
}

meta1<-meta_func(tt01.sido.tb) #온열질환
meta3<-meta_func(tt03.sido.tb) #열성경련
meta4<-meta_func(tt04.sido.tb) #수족구병

# write.csv(meta1,file="meta_reulst_heatrelated.csv",row.names=F,na="")
# write.csv(meta3,file="meta_reulst_feb.csv",row.names=F,na="")
# write.csv(meta4,file="meta_reulst_HFMD.csv",row.names=F,na="")

meta1.r<-subset(meta1,exposure=="maxT")
meta1.r$category=c(rep("Single lag",8),rep("Moving average",7))

meta1.r$RR =exp(meta1.r$beta)
meta1.r$lci=exp(meta1.r$beta-1.96*meta1.r$se)
meta1.r$uci=exp(meta1.r$beta+1.96*meta1.r$se)

meta1.1<-subset(meta1.r,category=="Single lag")
meta1.2<-subset(meta1.r,category=="Moving average")

gs1<-ggplot(meta1.1,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="Relative Risk (95% CI)")+
  geom_hline(yintercept=1,col="red")+coord_cartesian(ylim=c(0.99,1.04))
gm1<-ggplot(meta1.2,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="")+
  geom_hline(yintercept=1,col="red")+coord_cartesian(ylim=c(0.99,1.04))

x11();grid.arrange(gs1,gm1,ncol=2)


meta3.r<-subset(meta3,exposure=="maxT")
meta3.r$category=c(rep("Single lag",8),rep("Moving average",7))

meta3.r$RR =exp(meta3.r$beta)
meta3.r$lci=exp(meta3.r$beta-1.96*meta3.r$se)
meta3.r$uci=exp(meta3.r$beta+1.96*meta3.r$se)

meta3.1<-subset(meta3.r,category=="Single lag")
meta3.2<-subset(meta3.r,category=="Moving average")

gs3<-ggplot(meta3.1,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="Relative Risk (95% CI)")+
  geom_hline(yintercept=1,col="red")+coord_cartesian(ylim=c(0.97,1.02))
gm3<-ggplot(meta3.2,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="")+
  geom_hline(yintercept=1,col="red")+coord_cartesian(ylim=c(0.97,1.02))

x11();grid.arrange(gs3,gm3,ncol=2)

meta4.r<-subset(meta4,exposure=="maxT")
meta4.r$category=c(rep("Single lag",8),rep("Moving average",7))

meta4.r$RR =exp(meta4.r$beta)
meta4.r$lci=exp(meta4.r$beta-1.96*meta4.r$se)
meta4.r$uci=exp(meta4.r$beta+1.96*meta4.r$se)

meta4.1<-subset(meta4.r,category=="Single lag")
meta4.2<-subset(meta4.r,category=="Moving average")

gs4<-ggplot(meta4.1,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="Relative Risk (95% CI)")+
  geom_hline(yintercept=1,col="red")+coord_cartesian(ylim=c(1.00,1.105))
gm4<-ggplot(meta4.2,aes(lag,RR))+geom_point(size=6)+theme_bw(base_size=25)+
  geom_errorbar(aes(ymin=lci,ymax=uci),width=0.2,lwd=1.1)+facet_wrap(~category)+labs(x="",y="")+
  geom_hline(yintercept=1,col="red")+coord_cartesian(ylim=c(1.00,1.105))

x11();grid.arrange(gs4,gm4,ncol=2)




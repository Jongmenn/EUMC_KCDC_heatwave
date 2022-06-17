library(dplyr)
library(stringr)

setwd("D:\\EUMC\\데이터관리\\기상청\\일체감최고기온")

lf<-list.files()

sido=c("서울","부산","대구","인천","광주","대전","울산","세종","경기","강원",
       "충북","충남","전북","전남","경북","경남","제주")

ind01<-grep(sido[1],list.files())
ind02<-grep(sido[2],list.files())
ind03<-grep(sido[3],list.files())
ind04<-grep(sido[4],list.files())
ind05<-grep(sido[5],list.files())
ind06<-grep(sido[6],list.files())
ind07<-grep(sido[7],list.files())
ind08<-grep(sido[8],list.files())
ind09<-grep(sido[9],list.files())
ind10<-grep(sido[10],list.files())
ind11<-grep(sido[11],list.files())
ind12<-grep(sido[12],list.files())
ind13<-grep(sido[13],list.files())
ind14<-grep(sido[14],list.files())
ind15<-grep(sido[15],list.files())
ind16<-grep(sido[16],list.files())
ind17<-grep(sido[17],list.files())

me.sido01=NULL;me.sido02=NULL;me.sido03=NULL;me.sido04=NULL
me.sido05=NULL;me.sido06=NULL;me.sido07=NULL;me.sido08=NULL
me.sido09=NULL;me.sido10=NULL;me.sido11=NULL;me.sido12=NULL
me.sido13=NULL;me.sido14=NULL;me.sido15=NULL;me.sido16=NULL
me.sido17=NULL

me.sido.fuc<-function(me.sido,index){
  savedata<-me.sido
  index<-index
  for(i in index){
    
    dat<-read.csv(lf[i],skip=3,fileEncoding = "euc-kr")
    
    #일자, 평균기온, 일최고기온 시 습도, 일최고체감기온
    names(dat)=c('date',"maxtemp","max_humi","maxAT")
    
    dat$area=str_split(lf[i],"_")[[1]][3]
    dat$code=str_split(lf[i],"_")[[1]][4]
    
    savedata[[i]]<-dat
    
  }
  dd<-do.call(rbind,savedata)
  dd %>% group_by(date,area) %>% dplyr:: summarise(maxtemp =mean(maxtemp,na.rm=T),
                                                   max_humi=mean(max_humi,na.rm=T),
                                                   maxAT   =mean(maxAT,na.rm=T)) %>% mutate(year=year(date),
                                                                                            month=month(date),
                                                                                            day=day(date)) %>% filter(month %in% (5:9) )
  
  
}

#지점별, 2001~2021년 까지,5월~9월까지 일최고체감기온 포함된 자료 불러와서
#도시별 평균 내기: 일최고기온, 그 때의 평균 습도, 일최고체감기온 

sido01<-me.sido.fuc(me.sido01,ind01)
sido02<-me.sido.fuc(me.sido02,ind02)
sido03<-me.sido.fuc(me.sido03,ind03)
sido04<-me.sido.fuc(me.sido04,ind04)
sido05<-me.sido.fuc(me.sido05,ind05)
sido06<-me.sido.fuc(me.sido06,ind06)
sido07<-me.sido.fuc(me.sido07,ind07)
sido08<-me.sido.fuc(me.sido08,ind08)
sido09<-me.sido.fuc(me.sido09,ind09)
sido10<-me.sido.fuc(me.sido10,ind10)
sido11<-me.sido.fuc(me.sido11,ind11)
sido12<-me.sido.fuc(me.sido12,ind12)
sido13<-me.sido.fuc(me.sido13,ind13)
sido14<-me.sido.fuc(me.sido14,ind14)
sido15<-me.sido.fuc(me.sido15,ind15)
sido16<-me.sido.fuc(me.sido16,ind16)
sido17<-me.sido.fuc(me.sido17,ind17)

sido_maxAT<-rbind(sido01,sido02,sido03,sido04,sido05,sido06,sido07,sido08,
                  sido09,sido10,sido11,sido12,sido13,sido14,sido15,sido16,sido17)


sido_maxAT$area=substr(sido_maxAT$area,1,2)
sido_maxAT$key=with(sido_maxAT,paste0(area,"-",date))

setwd("D:\\EUMC\\데이터관리\\기상청")
write.csv(sido_maxAT,file="sido_maxAT.csv",row.names=F,na="",fileEncoding = "euc-kr")



####################################################
###Ko-CHENS data set 2021년 9월 , 최종보고회 자료###
####################################################

#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#라이브러리 일괄 설치
pacman::p_load("dplyr","ggplot2","reshape2","mgcv","sqldf","psych","Rmisc","lsmeans","doBy","RColorBrewer","lubridate",
               "caret","e1071","lmtest","readxl","splines","metafor","mixmeta","data.table","stringr","lme4",
               "gee","geepack","ggrepel","MuMIn","qcc","extrafont","scales","gamm4")

#폰트 변경 
windowsFonts(
  Arial   = windowsFont("Arial"),
  Calibri = windowsFont("Calibri"),
  Cambria = windowsFont("Cambria"),
  윤고딕360 =windowsFont("윤고딕360"),
  윤고딕250 =windowsFont("윤고딕250"),
  윤고딕550 =windowsFont("윤고딕550"),
  윤고딕 =windowsFont("-윤고딕360")
)

windowsFonts()
#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#대규모
setwd("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\최종보고서용 자료_지원센터_210812\\대규모")
f.index1<-grep("pwout",list.files())

main1    <-read_excel(list.files()[f.index1][1])
main2    <-read_excel(list.files()[f.index1][2])
main3    <-read_excel(list.files()[f.index1][3])
main4    <-read_excel(list.files()[f.index1][4])
main_addr<-read_excel(list.files()[f.index1][5])
main_svy <-read_excel(list.files()[f.index1][6])

#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#상세
setwd("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\최종보고서용 자료_지원센터_210812\\상세")
f.index2<-grep("pwout",list.files())

core_deliver <-read_excel(list.files()[f.index2][1])
core_svy     <-read_excel(list.files()[f.index2][2])
core_svy06   <-read_excel(list.files()[f.index2][3])
core_svy12   <-read_excel(list.files()[f.index2][4])
core_svy24   <-read_excel(list.files()[f.index2][5])
core_svy36   <-read_excel(list.files()[f.index2][6])
core_svy48   <-read_excel(list.files()[f.index2][7])
core_growth06<-read_excel(list.files()[f.index2][8])
core_growth12<-read_excel(list.files()[f.index2][9])
core_growth24<-read_excel(list.files()[f.index2][10])
core_growth36<-read_excel(list.files()[f.index2][11])
core_growth48<-read_excel(list.files()[f.index2][12])
core_addr    <-read_excel(list.files()[f.index2][13])

#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#생체시료
setwd("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\최종보고서용 자료_지원센터_210812\\생체시료")
f.index3<-grep("pwout",list.files())

bio_pre <-read_excel(list.files()[f.index3][1],sheet=2)
bio_post<-read_excel(list.files()[f.index3][1],sheet=3)
bio_cord<-read_excel(list.files()[f.index3][1],sheet=4)

names(bio_pre) [3:30]=paste0(names(bio_pre) [3:30],"_c1")
names(bio_post)[3:10]=paste0(names(bio_post)[3:10],"_c2")
names(bio_cord)[3:7] =paste0(names(bio_cord)[3:7],"_c3")

bio_pre $MOM_ID %>% unique %>% length
bio_post$MOM_ID %>% unique %>% length
bio_cord$MOM_ID %>% unique %>% length

#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#임산부 혈액 소변 merge
bio_mom<-bio_pre %>% left_join(bio_post %>% select(MOM_ID:tcs_c2),by="MOM_ID",all.x=T)

grep("u2",names(bio_mom))
names(bio_mom)[grep("u2",names(bio_mom))]="crea_u_c2"

#프탈레이트 TBD로 자료 없으니 빼고 보기 
bio_mom1<-bio_mom[,c(1:2,grep("_c1",names(bio_mom)))] %>% select(-c(mehhp_c1:mcpp_c1))
bio_mom2<-bio_mom[,c(1:2,grep("_c2",names(bio_mom)))] %>% select(-c(mehhp_c2:mcpp_c2))

bio_mom3<-bio_cord[,c(1:2,grep("_c3",names(bio_cord)))]

names(bio_mom1)[11:22]
names(bio_mom2)[11:22]

names(bio_post)[11:22]

#데이터 속성 변환 (MDL 미만 값들 전부 NA로)
for(i in 11:length(bio_mom1)){
bio_mom1[,i]=as.double(unlist(bio_mom1[,i]))
bio_mom2[,i]=as.double(unlist(bio_mom2[,i]))
bio_post[,i]=as.double(unlist(bio_post[,i]))
}

bio_mom3$pb_c3=as.double(bio_mom3$pb_c3)
bio_mom3$cd_c3=as.double(bio_mom3$cd_c3)

names(bio_mom1)
##레아티닌 보정 
#임신초기 크레아티닌 보정, 중금속 제외 (납/수은/카드뮴)
#프탈레이트 TBD라 나중에 추가해야함 
bio_mom1.crea<- bio_mom1 %>% select(cotin_c1:tcs_c1) / bio_mom1$crea_u_c1
bio_mom2.crea<- bio_mom2 %>% select(cot_c2:tcs_c2)   / bio_mom2$crea_u_c2


names(bio_mom1.crea)=paste0("c",names(bio_mom1.crea))
names(bio_mom2.crea)=paste0("c",names(bio_mom2.crea))
names(bio_mom2.crea)[1]="ccotin_c2" #임신 초기, 말기 코티닌 이름 형식이 다르니 맞춰주기 

#생체시료 코티닌 보정한값 열에 추가해주기 
bio_mom1.r<-cbind(bio_mom1,bio_mom1.crea)
bio_mom2.r<-cbind(bio_mom2,bio_mom2.crea) %>% filter(MOM_ID %in% bio_post$MOM_ID)

bio_mom1.log<-bio_mom1.r %>% select(crea_u_c1:ctcs_c1) %>% log
bio_mom2.log<-bio_mom2.r %>% select(crea_u_c2:ctcs_c2) %>% log

names(bio_mom1.log)=paste0("ln_",names(bio_mom1.log))
names(bio_mom2.log)=paste0("ln_",names(bio_mom2.log))

bio_mom1.r<-cbind(bio_mom1.r,bio_mom1.log)
bio_mom2.r<-cbind(bio_mom2.r,bio_mom2.log)

bio_mom3$ln_pb_c3=log(bio_mom3$pb_c3)
bio_mom3$ln_cd_c3=log(bio_mom3$cd_c3)
bio_mom3$ln_hg_c3=log(bio_mom3$hg_c3)

#임산부, 혈액, 소변시료 요약 통계 
fun.sum<-function(x) {
  data.frame(n=length(x),
             missing=ifelse(is.na(x)==T,1,0) %>% sum,
             obs =length(na.omit(x)),
             mean=mean(x,na.rm=T),
             sd=sd(x,na.rm=T),
             median=median(x,na.rm=T),
             min=min(x,na.rm=T),
             t(quantile(x,na.rm=T,p=c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))),
             max=max(x,na.rm=T),
             IQR=IQR(x,na.rm=T),
             geoMean=geometric.mean(x),
             geoSD  =exp(sd(log(x),na.rm=T)))
}


#환경측정변수 기술통계, MDL 미만 측정값 missing 처리 
mom_b_u.summ.list1=NULL
mom_b_u.summ.list2=NULL
for(i in 1:20){
  names(bio_mom1.r)
  x<-unlist(bio_mom1.r[,i+10])
  mom_b_u.summ.list1[[i]]<-cbind(category=names(bio_mom1.r)[i+10],fun.sum(x))
  x<-unlist(bio_mom2.r[,i+10])
  mom_b_u.summ.list2[[i]]<-cbind(category=names(bio_mom2.r)[i+10],fun.sum(x))
}

mom_b_u.summ1<-do.call(rbind,mom_b_u.summ.list1) #임신 초기 
mom_b_u.summ2<-do.call(rbind,mom_b_u.summ.list2) #임신 말기 

#제대혈 (중금속)
mom_b_u.summ3<-rbind(fun.sum(bio_mom3$pb_c3),
fun.sum(bio_mom3$cd_c3),
fun.sum(bio_mom3$hg_c3))

#산모 중복 존재 
bio_mom3$MOM_ID %>% unique %>% length

nrow(bio_mom3)-bio_mom3$MOM_ID %>% unique %>% length

#기술통계 내보내기 
# write.csv(mom_b_u.summ1,file="mom_b_u.summ1.csv",row.names=F,na="")
# write.csv(mom_b_u.summ2,file="mom_b_u.summ2.csv",row.names=F,na="")
# write.csv(mom_b_u.summ3,file="mom_b_u.summ3.csv",row.names=F,na="")

#생체시료 지표 plotting
#색깔 및 레이블 밑 작업
cbbPalette <- c("#F8766D","#00BA38", "#619CFF", "629EFB", "#F368DE") #boxplot 색깔
bio_index<-read_excel("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\KoCHENS_최종보고서용 자료_지원센터_JM.xlsx",sheet="생체시료_기준치레이블")

#크레아티닌 보정x, 원자료 그대로 플롯팅
bio_pre_long <-bio_mom1.r %>% select(MOM_ID,pb_c1:tcs_c1) %>% melt(id.ars="MOM_ID") %>%separate(variable,c("word1","word2"),sep="_") %>% mutate(label="임신초기")
bio_post_long<-bio_mom2.r %>% select(MOM_ID,pb_c2:tcs_c2) %>% melt(id.ars="MOM_ID") %>%separate(variable,c("word1","word2"),sep="_") %>% mutate(label="임신말기")
bio_cord_long<-bio_mom3   %>% select(MOM_ID,pb_c3:hg_c3) %>% melt(id.ars="MOM_ID")  %>%separate(variable,c("word1","word2"),sep="_") %>% mutate(label="제대혈")


bio_post_long$word1=ifelse(bio_post_long$word1=="cot","cotin",bio_post_long$word1)

bio_mom_long<-rbind(bio_pre_long ,bio_post_long,bio_cord_long)

bio_mom_long$label=factor(bio_mom_long$label,levels=unique(bio_mom_long$label))

ind<-unique(bio_mom_long$word1)

subset(bio_mom_long,word1==ind[3])$label %>% table

#생체시료 물질별 개별 플롯 저장 
for(i in 1:length(ind)){
ss<-subset(bio_mom_long,word1==ind[i])
k<-length(na.omit(subset(bio_index,category==unique(ss$word1))$cut_value))+1

subset(bio_index,category==unique(ss$word1)) %>% filter(value>0.0001)

na.omit(subset(bio_index,category==unique(ss$word1))$cut_value) %>% as.character()
unique(subset(bio_index,category==ind[i])$ymax)
ggplot(ss,aes(x=label,y=value,fill=label))+geom_boxplot()+coord_cartesian(ylim=c(0,unique(subset(bio_index,category==ind[i])$ymax)))+
  stat_summary(fun.y=geometric.mean, colour="red", geom="point",shape=18, size=8)+
  theme_bw()+scale_fill_manual(values=cbbPalette)+
  geom_hline(data=subset(bio_index,category==unique(ss$word1)) %>% filter(value>0.0001),
             aes(yintercept=value,col=na.omit(subset(bio_index,category==unique(ss$word1))$cut_value) %>% as.character()),
             linetype=2:k,lwd=1.2)+guides(col = guide_legend(order = 1),type = guide_legend(order = 2))+
  theme(legend.position = "top",axis.text.x = element_text(),legend.title = element_blank(),axis.text.y = element_text(),
        text=element_text(size=20, family="윤고딕360"))+labs(x="",y=unique(bio_index$label)[i])+
  labs(x=paste("\n","1. National Institute of Environmental Research (NIER), Korean National Environmental Health Survey - The Second Stage (‘12∼‘14), 2015                   ","\n",
               "2. U.S.A. Centers for Disease Control and Prevention, 4th National Report on Human Exposure to Environmental Chemicals, 2013~2014 and 2015~2016","\n",
               "3. Health Canada, Results of the Canadian Health Measures Survey cycle 2(2009~2011), cycle 3(2012~2013), and cycle 4(2014~2015)                          "),y="")
ggsave(paste0("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\figure\\생체시료\\생체시료_",ind[i],".tiff"),
       width=22,height=12,dpi=100)
}
#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#영유아 혈액 소변 자료 
bio_blood24<-read_excel(list.files()[f.index3][2],sheet=1)
bio_blood36<-read_excel(list.files()[f.index3][2],sheet=2)
bio_blood48<-read_excel(list.files()[f.index3][2],sheet=3)
bio_urine24<-read_excel(list.files()[f.index3][3],sheet=1)
bio_urine36<-read_excel(list.files()[f.index3][3],sheet=2)
bio_urine48<-read_excel(list.files()[f.index3][3],sheet=3)

#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#환경측정 
setwd("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\최종보고서용 자료_지원센터_210812\\환경측정")
f.index4<-grep("pwout",list.files())

# "." missing value 인식하고 불러오기 
env<-read_excel(list.files()[f.index4][1],na=".")

env_mdl<-read_excel("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\KoCHENS_최종보고서용 자료_지원센터_JM.xlsx",sheet="환경측정변수_MDL")

#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#환경측정 #n=2,047, variables 45

#검출한계(LOD, limit of detection): 검출 가능한 최소량, 정량 가능할 필요 없음

#검출한계는 일반적으로 두가지
# (1) 분석기기에 직접 시료를 주입하여 분석하는 경우에는 기기검출한계(IDL, instrument detection limit)
# (2) 전처리 또는 분석과정이 포함된 경우에는 방법검출한계 (MDL, method detection limit)

#검출한계 구하는 방법 
#1) 시각적 평가에 근거하는 방법,
#2) 신호(signal) 대 잡음(noise)에 근거하는 방법,
#3) 반응의 표준편차와 검정곡선의 기울기에 근거하는 방법 (표준편차를 검량선의 기울기로 나눈값에 3.3을 곱하여 산출)

#방법검출한계, MDL(method detection limit)
#시험분석 대상을 검출할 수 있는 최소한의 농도, 제시된 정량한계 부근의 농도를 포함하도록 준비한 n개의 시료를 반복측정하여
#얻은 결과의 표준편차에 99% 신뢰도에서의 t분포값을 곱한 것 


env_melt<-melt(env,id.vars=c("NO","ID","MDLYEAR"))
env_melt$key=paste0(env_melt$variable,"-",env_melt$MDLYEAR)
env_mdl$key=paste0(env_mdl$variable2,"-",env_mdl$index)

#MDL이 시기별로 다르니까 시기별로 MDL 적용 
names(env_mdl)[1]=c("name")

env_melt2<-env_melt %>% left_join(env_mdl,by="key") %>% dplyr:: select(-c(variable2,index,key,note))

subenv1<-env_melt2[is.na(env_melt2$MDL),]
subenv2<-env_melt2[!is.na(env_melt2$MDL),]

subenv1$value_r=subenv1$value
subenv1$MDL_c  =0
subenv2$value_r=with(subenv2,ifelse(value<MDL,NA,value))
subenv2$MDL_c  =with(subenv2,ifelse(value<MDL,1,0))

env_melt3<-rbind(subenv1,subenv2) %>% arrange(NO)
env_melt3$variable=as.character(env_melt3$variable)


#환경측정 자료 summary table
fun.summ<-function(xx) {
  
  sub_d<-subset(env_melt3,variable==xx)
  x<-sub_d$value_r
  mdl_c<-sum(sub_d$MDL_c,na.rm=T)
  data.frame(n=length(x),
           missing=ifelse(is.na(x)==T,1,0) %>% sum,
           MDL_n  =mdl_c,
           obs =length(na.omit(x)),
           mean=mean(x,na.rm=T),
           sd=sd(x,na.rm=T),
           median=median(x,na.rm=T),
           min=min(x,na.rm=T),
           t(quantile(x,na.rm=T,p=c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))),
           max=max(x,na.rm=T),
           IQR=IQR(x,na.rm=T),
           geoMean=geometric.mean(x),
           geoSD  =exp(sd(log(x),na.rm=T)))}


#환경측정변수 기술통계, MDL 미만 측정값 missing 처리 
env.summ.list=NULL
for(i in 1:42){
  xx<-names(env)[i+3]
  env.summ.list[[i]]<-cbind(category=xx,fun.summ(xx))
  }

env_statistics<-do.call(rbind,env.summ.list)

# write.csv(env_statistics,file="환경측정변수_MDL미만결측처리_기술통계.csv",row.names=F,na="")

#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#
#환경측정자료 분할해서 그림 그리기 

env1<-env %>% select(PM10,PM25,DUS,END,HUM,TEM)
env2<-env[,grep("Ind",names(env))]
env3<-env[,grep("Per",names(env))]

env1_label<-c("PM10 (㎍/㎥)","PM2.5 (㎍/㎥)","Dust mite (ng/g)",
"Endotoxin (EU/mg)","Humidity (%)","Temperature (℃)")

env1_g=NULL
for(i in 1:length(env1)){
  
  d<-env1[,i]; names(d)="value"
  env1_g[[i]]<-ggplot(d,aes(x=value))+geom_histogram()+labs(x=env1_label[i],
                                                     y="Frequency")+theme_gray(base_size=28)
  ggsave(paste0("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\figure\\환경측정데이터\\환경측정_",names(env1)[i],".tiff"),
         width=12,height=12,dpi=60)
  print(i)
}

env1_melt<-env_melt3 %>% filter(variable %in% c("PM10","PM25","DUS","END","HUM","TEM") )
env1_melt$label=ifelse(env1_melt$variable=="PM10","PM10 (㎍/㎥)",
                       ifelse(env1_melt$variable=="PM25","PM25 (㎍/㎥)",
                              ifelse(env1_melt$variable=="DUS","Dust mite (ng/g)",
                                     ifelse(env1_melt$variable=="END","Endotoxin (EU/mg)",
                                            ifelse(env1_melt$variable=="HUM","Humidity (%)","Temperature (℃)")))))

env1_melt$label=factor(env1_melt$label,levels=unique(env1_melt$label))
x11();ggplot(env1_melt,aes(x=value))+geom_histogram()+labs(x="",
                                             y="Frequency")+theme_gray(base_size=32)+facet_wrap(~label,scales="free")

ggsave(paste0("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\figure\\환경측정데이터\\환경측정1_병합.tiff"),
       width=32,height=24,dpi=80)

#환경측정물질 Indoor VOC
env2_g=NULL
for(i in 1:length(env2)){
  
  d<-env2[,i]; names(d)="value"
  env2_g[[i]]<-ggplot(d,aes(x=value))+geom_histogram()+labs(x=unique(env_melt3$name)[i+1],
                                                            y="Frequency")+theme_gray(base_size=28)
  ggsave(paste0("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\figure\\환경측정데이터\\환경측정_",names(env2)[i],".tiff"),
         width=12,height=12,dpi=60)
  print(i)
}

env2_melt<-env_melt3 %>% filter(variable %in% c(names(env2)) )
env2_melt$label=paste0("Indoor ",env2_melt$name)
  
env2_melt$label=factor(env2_melt$label,levels=unique(env2_melt$label))

ggplot(env2_melt,aes(x=value))+geom_histogram()+labs(x="",
                                                           y="Frequency")+theme_gray(base_size=32)+
  facet_wrap(~label,scales="free")

ggsave(paste0("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\figure\\환경측정데이터\\환경측정2_병합.tiff"),
       width=42,height=28,dpi=80)

#환경측정물질 Indoor VOC
env3_g=NULL
for(i in 1:length(env3)){
  
  d<-env3[,i]; names(d)="value"
  env3_g[[i]]<-ggplot(d,aes(x=value))+geom_histogram()+labs(x=unique(env_melt3$name)[i+1],
                                                            y="Frequency")+theme_gray(base_size=28)
  ggsave(paste0("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\figure\\환경측정데이터\\환경측정_",names(env3)[i],".tiff"),
         width=12,height=12,dpi=60)
  print(i)
}

env3_melt<-env_melt3 %>% filter(variable %in% c(names(env3)) )
env3_melt$label=paste0("Personal ",env3_melt$name)

env3_melt$label=factor(env3_melt$label,levels=unique(env3_melt$label))

ggplot(env3_melt,aes(x=value))+geom_histogram()+labs(x="",
                                                     y="Frequency")+theme_gray(base_size=32)+
  facet_wrap(~label,scales="free")

ggsave(paste0("D:\\EUMC\\Ko-CHENS\\데이터\\2021년보고서\\figure\\환경측정데이터\\환경측정3_병합.tiff"),
       width=42,height=28,dpi=80)
#----------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------#



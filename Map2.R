#------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------#
#라이브러리
pacman::p_load(ggmap,raster,rgeos,maptools,rgdal,dplyr,lubridate,ggrepel,gridExtra)

#---------------------------------------------------------------------------#
#---------------------------------------------------------------------------#
#지도
korea<-shapefile("D:\\EUMC\\지도\\CTPRVN_201602\\TL_SCCO_CTPRVN.shp")
korea<-spTransform(korea,CRS("+proj=longlat"))
korea_map<-fortify(korea)


setwd("D:\\EUMC\\질병관리청\\폭염후속연구\\분석")
effectsize<-read_excel("KCDC_EWHA_HW_정리_20221122.xlsx",sheet="도시별_소아입원") %>% filter(lag=="lag0") %>% 
  filter(exposure %in% c("meanT_s0","maxT_s0","maxAT_s0","temp_SD_s0","TN_s0","DT_s0","hwD2_maxT_s0","hwD2_maxAT_s0","HI_s0")) %>% 
  filter(Outcome=="Febrile seizure")

effectsize$Outcome %>% unique
effectsize$sido<-effectsize$city

effectsize$sido=with(effectsize,ifelse(sido=="강원",0 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="경기",1 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="경남",2 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="경북",3 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="서울",8 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="부산",7 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="대구",5 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="인천",11,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="광주",4 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="대전",6 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="세종",9 ,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="울산",10,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="전남",13,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="전북",12,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="제주",14,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="충남",15,effectsize$sido))
effectsize$sido=with(effectsize,ifelse(sido=="충북",16,effectsize$sido))
effectsize$sido=as.numeric(effectsize$sido)

#PM 농도 겹쳐서 그리기 전체 지역  (연구 지역만 표현할 때)
mapping_year_func<-function(k){
  korea_map$RR=NA
  
  korea_map$RR=ifelse(korea_map$id=="0",subset(effectsize,exposure==k & sido==0)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="1" ,subset(effectsize,exposure==k & sido==1)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="2" ,subset(effectsize,exposure==k & sido==2)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="3" ,subset(effectsize,exposure==k & sido==3)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="4" ,subset(effectsize,exposure==k & sido==4)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="5" ,subset(effectsize,exposure==k & sido==5)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="6" ,subset(effectsize,exposure==k & sido==6)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="7" ,subset(effectsize,exposure==k & sido==7)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="8" ,subset(effectsize,exposure==k & sido==8)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="9" ,subset(effectsize,exposure==k & sido==9)$RR  ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="10",subset(effectsize,exposure==k & sido==10)$RR ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="11",subset(effectsize,exposure==k & sido==11)$RR ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="12",subset(effectsize,exposure==k & sido==12)$RR ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="13",subset(effectsize,exposure==k & sido==13)$RR ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="14",NA ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="15",subset(effectsize,exposure==k & sido==15)$RR ,korea_map$RR)
  korea_map$RR=ifelse(korea_map$id=="16",subset(effectsize,exposure==k & sido==16)$RR ,korea_map$RR)
  
  korea_map$label=NA
  korea_map$label=ifelse(korea_map$id==0 ,"Gangwon-do"      ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==1 ,"Gyeonggi-do"     ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==2 ,"Gyeongsangnam-do",korea_map$label)
  korea_map$label=ifelse(korea_map$id==3 ,"Gyeongsangbuk-do",korea_map$label)
  korea_map$label=ifelse(korea_map$id==8 ,"Seoul"           ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==7 ,"Busan"           ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==5 ,"Daegu"           ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==11,"Incheon"         ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==4 ,"Gwangju"         ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==6 ,"Daejeon"         ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==10,"Ulsan"           ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==9 ,"Sejong"          ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==12,"Jeollanam-do"    ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==13,"Jeollabuk-do"    ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==14,"Jeju"            ,korea_map$label)
  korea_map$label=ifelse(korea_map$id==15,"Chungcheongnam-do",korea_map$label)
  korea_map$label=ifelse(korea_map$id==16,"Chungcheongbuk-do",korea_map$label)
  korea_map$label=factor(korea_map$label,levels=unique(korea_map$label))
  korea_map}

#연도별 노출농도 붙이는 함수 적용 
korea_map1<-mapping_year_func("hwD2_maxT_s0");
korea_map2<-mapping_year_func("hwD2_maxAT_s0");
korea_map3<-mapping_year_func("meanT_s0");
korea_map4<-mapping_year_func("maxT_s0");
korea_map5<-mapping_year_func("maxAT_s0");
korea_map6<-mapping_year_func("HI_s0");
korea_map7<-mapping_year_func("DT_s0");
korea_map8<-mapping_year_func("temp_SD_s0")
korea_map9<-mapping_year_func("TN_s0");

#단일년도 한해만 잡아서 지도 레이블 이용
mht.cent <- korea_map1 %>% group_by(label) %>% summarize(long = median(long), lat = median(lat))
mht.cent<-cbind(aggregate(korea_map1$long,list(korea_map1$label),median),
                lat=aggregate(korea_map1$lat,list(korea_map1$label),median)$x)

names(mht.cent)[1:2]=c("label","long")

k1<-subset(korea_map1,id!=4);k2<-subset(korea_map1,id==4);korea_tmap1<-rbind(k1,k2)
k1<-subset(korea_map2,id!=4);k2<-subset(korea_map2,id==4);korea_tmap2<-rbind(k1,k2)
k1<-subset(korea_map3,id!=4);k2<-subset(korea_map3,id==4);korea_tmap3<-rbind(k1,k2)
k1<-subset(korea_map4,id!=4);k2<-subset(korea_map4,id==4);korea_tmap4<-rbind(k1,k2)
k1<-subset(korea_map5,id!=4);k2<-subset(korea_map5,id==4);korea_tmap5<-rbind(k1,k2)
k1<-subset(korea_map6,id!=4);k2<-subset(korea_map6,id==4);korea_tmap6<-rbind(k1,k2)
k1<-subset(korea_map7,id!=4);k2<-subset(korea_map7,id==4);korea_tmap7<-rbind(k1,k2)
k1<-subset(korea_map8,id!=4);k2<-subset(korea_map8,id==4);korea_tmap8<-rbind(k1,k2)
k1<-subset(korea_map9,id!=4);k2<-subset(korea_map9,id==4);korea_tmap9<-rbind(k1,k2)

korea_tmap1$group=with(korea_tmap1,factor(group,levels=(unique(group))))
korea_tmap2$group=with(korea_tmap2,factor(group,levels=(unique(group))))
korea_tmap3$group=with(korea_tmap3,factor(group,levels=(unique(group))))
korea_tmap4$group=with(korea_tmap4,factor(group,levels=(unique(group))))
korea_tmap5$group=with(korea_tmap5,factor(group,levels=(unique(group))))
korea_tmap6$group=with(korea_tmap6,factor(group,levels=(unique(group))))
korea_tmap7$group=with(korea_tmap7,factor(group,levels=(unique(group))))
korea_tmap8$group=with(korea_tmap8,factor(group,levels=(unique(group))))
korea_tmap9$group=with(korea_tmap9,factor(group,levels=(unique(group))))

disease_map<-function(dataset,yl,yu,text1,text2){
  ggplot()+geom_polygon(data=dataset,aes(x=long,y=lat,group=group,fill=RR),col="black")+
    theme_minimal(base_size=15)+geom_text_repel(aes(label = label,x=long,y=lat),colour = "black", data = mht.cent, size =3.5,fontface = 'bold')+
    scale_fill_gradient(low="white",high="red",na.value="gray",limits=c(yl,yu),name=text1)+
    theme(axis.line = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),
          legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          panel.border = element_blank(),panel.background = element_blank())+
    labs(x="",y="",title = text2,plot.title = element_text(vjust = 0.5),size=20)}

mm<-c(korea_tmap1$RR,korea_tmap2$RR,korea_tmap3$RR,korea_tmap4$RR,korea_tmap5$RR,korea_tmap6$RR,
  korea_tmap7$RR,korea_tmap8$RR,korea_tmap9$RR)

summary(mm,na.rm=T)
min(mm,na.rm=T)
max(mm,na.rm=T)

g1<-disease_map(korea_tmap1,0,2,"Relative risk","폭염 (일최고기온)")
g2<-disease_map(korea_tmap2,0,2,"Relative risk","폭염 (일최고체감기온)")
g3<-disease_map(korea_tmap3,0,2,"Relative risk","평균기온")
g4<-disease_map(korea_tmap4,0,2,"Relative risk","일최고기온")
g5<-disease_map(korea_tmap5,0,2,"Relative risk","일최고체감기온")
g6<-disease_map(korea_tmap6,0,2,"Relative risk","열지수")
g7<-disease_map(korea_tmap7,0,2,"Relative risk","일교차")
g8<-disease_map(korea_tmap8,0,2,"Relative risk","일일 기온편차")
g9<-disease_map(korea_tmap9,0,2,"Relative risk","열대야")

x11();grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,ncol=3)
g8<-ggplot()+geom_polygon(data=korea_tmap1,aes(x=long,y=lat,group=group,fill=RR),col="black")+
  theme_minimal(base_size=15)+geom_text_repel(aes(label = label,x=long,y=lat),colour = "black", data = mht.cent, size =3.5,fontface = 'bold')+
  scale_fill_gradient(low="white",high="red",na.value="gray",limits=c(0,2),name="Relative Risk")+
  theme(axis.line = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())+
  labs(x="",y="",title = "text2",plot.title = element_text(vjust = 0.6),size=20)
x11();g8


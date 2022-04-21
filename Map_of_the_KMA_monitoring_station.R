library(ggmap)
library(raster)
library(ggplot2)

setwd("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\gadm40_KOR_shp")
korea<-shapefile("gadm40_KOR_0.shp")
korea_map<-fortify(korea)

zz<-read_excel("D:\\EUMC\\논문\\연구논문\\KCDC_Heatwave\\Children\\기상관측지점.xlsx")

#Location of South Korea Meteorological monitoring Station
x11();ggplot()+
  geom_polygon(data =korea_map, aes(x = long, y = lat, group = group),
               color = "black",fill="white", alpha = 0.2, linetype = 1)+
  geom_point(data=zz,aes(x=longitude,latitude),pch=15,size=2.5,col="red")+
  theme_minimal(base_size=18)+
  labs(x="",y="")+
  theme(axis.line = element_blank(),axis.text.x = element_blank(),axis.text.y = element_blank(),
        legend.position = "none",panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_blank(),panel.background = element_blank())

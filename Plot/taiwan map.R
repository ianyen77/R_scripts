library(ggplot2) 
library(rgdal)#for fortify()
library(rgeos) #for fortify()
library(maptools) #for readShapeSpatial()
library(RColorBrewer)
library(tidyverse)
#空間資料檔名 請自行下載
tw_new <- readShapeSpatial("C:/Users/USER/Desktop/mapdata202303230423/TOWN_MOI_1120317.shp") 
head(tw_new$COUNTYID)
tw_new.df <- fortify(tw_new,region ="TOWNID") 
head(tw_new.df,10)
#做一個假資料來畫
#prevalence設為亂數rnorm(需要的亂數個數)
mydata<-data.frame(NAME_2=tw_new$TOWNNAME, 
                   id=tw_new$TOWNID,
                   pre=rnorm(length(tw_new$TOWNID)),
                   stringsAsFactors = F)
head(mydata)
final.plot<-merge(tw_new.df,mydata,by="id",all.x=T)
head(final.plot,10)
x<-unique(grep("^V",final.plot$id,value=T))
x<-c("V02","V07")
final.plot1<-final.plot%>%
  filter(id==x)
twcmap<-ggplot(final.plot1) +
  geom_polygon(data = final.plot1, 
               aes(x = long, y = lat, 
                   group = group),color="black",fill="#BEBADA",alpha=0.4, 
               size = 0.25)+coord_map()+theme_bw()
twcmap
#bulk water sampling sites
library(openxlsx)
sample<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/Plot/採樣位置/採樣點地理資訊.xlsx",sheet=3)
twcmap+geom_point(data=sample,aes(x=lon, y=lat,shape=type,color=type),size=3)+geom_text(data=sample,aes(x=lon, y=lat,label=sampeltype),family="serif",size=4,vjust =1.6,hjust=-0.1)+
  theme (legend.position= "none")
#biofilm sample sampling sites
cols=c("#3288BD" ,"#F8766D","#00BFC4")
shape=c(18,16,17)
sample<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/Plot/採樣位置/採樣點地理資訊.xlsx",sheet=4)
twcmap+geom_point(data=sample,aes(x=lon, y=lat,shape=type,color=type),size=3)+geom_text(data=sample,aes(x=lon, y=lat,label=sampeltype),family="serif",size=4,vjust =1.6,hjust=-0.1)+
  theme (legend.position= "none")+scale_color_manual(values=cols)+scale_shape_manual(values = shape) 


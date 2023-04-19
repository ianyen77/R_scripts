library(ggmap)
library(ggplot2)
library(dplyr)
library(gridExtra)
library("tidyverse")
library("openxlsx")
library("stringr")
library("RColorBrewer")
sample<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/Plot/採樣位置/採樣點地理資訊.xlsx",sheet=4)
#for safety reason we need to assign google api key each time
register_google(key = "")
trek_df <- trek("23.125018652096625, 121.39324881712558", "22.875292748459103, 121.22908696386816", structure = "route")
TaitoungMap <- get_googlemap(
  center  = c(lon=121.31154590744264,lat=22.998088043256093), 
  zoom = 11, maptype = 'roadmap')
#draw a plot

cols=c("#3288BD" ,"#F8766D","#00BFC4")
shape=c(15,16,17)
TaitoungMappoint<- ggmap(TaitoungMap)+ 
  geom_point(data=sample,aes(x=lon, y=lat,shape=type,color=type),size=3)+geom_text(data=sample,label=sample$sampeltype,family="serif",size=6,vjust =1.6,hjust=-0.1)+theme_minimal()+theme (legend.position= "none")+scale_color_manual(values=cols)+scale_shape_manual(values = shape) 
  #+geom_path(aes(x = lon, y = lat),  colour = "#66a182",size = 2.3, alpha = .7,data = trek_df, lineend = "square")
TaitoungMappoint

library(scales)
hex <- hue_pal()(3)
show_col(hex)

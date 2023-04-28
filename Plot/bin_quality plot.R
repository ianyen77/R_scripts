library(openxlsx)
library(tidyverse)
library(ggExtra)
data<-read.xlsx("C:/Users/USER/Desktop/bin stats.xlsx",sheet = 1,colNames = T)
color<-c("#704D9E","#ED7C97","#F9B282")
hcl.colors(5,"Sunset")
data$quality_index<-factor(data$quality_index,levels=c("High","Midium","Low"))
p<-ggplot(data)+
  geom_point(aes(x=completeness,y=contamination,color=quality_index,fill=quality_index),alpha=0.8,shape=21,size=3)+theme_bw()+
  theme(legend.position="none")+scale_color_manual("Quality",values = color)+scale_fill_manual("Quality",values = color)+theme(legend.position="bottom")+labs(x="Completeness (%)",y="Contamination(%)")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))


x<-ggMarginal(p, type="density",fill="#704D9E",alpha=0.4)
x


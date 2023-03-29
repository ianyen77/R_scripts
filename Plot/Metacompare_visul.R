library(openxlsx)
library(scatterplot3d)
library(tidyverse)
data<-read.xlsx("C:/Users/USER/Desktop/Metacompare.xlsx",sheet=2,rowNames=F,colNames = T)
data1<-data[,c(9,12,13)]
data1<-log10(data1[,1:3])
data1$Risk.Score<-data$Risk.Score
data1$Risk.Score<-round(data$Risk.Score,1)
color1<-c("#FB8072","#80B1D3","#BEBADA","#FDB462","#B3DE69")
color<-c("#FB8072","#FB8072","#FB8072","#BEBADA","#BEBADA","#BEBADA","#80B1D3","#80B1D3","#80B1D3","#FDB462","#FDB462","#FDB462","#B3DE69","#B3DE69","#B3DE69")
#colors<-color[as.numeric(data$Location)]
#hazard space
s3d<-scatterplot3d(data1[,1:3],main="Metacompare risk Matrix Scores",color= color,pch=16,cex.symbols = 1.4,type="h",grid=T)
text(s3d$xyz.convert(data1[,1:3]),adj=0.2,pos=3,labels =data1[,4],cex= 0.66, col = "black")
legend("bottom",col=color1,legend=levels(points$location),pt.bg = colors,pch=16,inset=-0.17,xpd =T,horiz = T)

#bar plot
data_barplot<-data%>%
  group_by(Location)%>%
  summarise(mean=mean(Risk.Score),std=sd(Risk.Score))
data_barplot$Location<-factor(data_barplot$Location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))
bar_plot<-ggplot(data_barplot,aes(x=Location,y=mean))+geom_bar(alpha=0.7,stat = "identity",width = 0.8,fill="#BEBADA")+theme_bw()+geom_errorbar(aes(x=Location,ymin=mean-std, ymax=mean+std), width=.1,position=position_dodge(.9))+labs(x="Location",y="MetaCompare Risk Scores")+theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
bar_plot  

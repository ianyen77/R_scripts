library(openxlsx)
library(scatterplot3d)
library(tidyverse)
library(ggplot2)

##Indivisual assembly visulazation-----------------------------------------
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/Metacompare/Metacompare.xlsx",sheet=2,rowNames=F,colNames = T)
data1<-data[,c(9,12,13)]
data1<-log10(data1[,1:3])
data1$Risk.Score<-data$Risk.Score
data1$Risk.Score<-round(data$Risk.Score,1)
color1<-c("#FB8072","#80B1D3","#BEBADA","#FDB462","#B3DE69")
color<-rep(color1,each=3)
color2<-hcl.colors(5,"sunset")
colo2<-rep(color2,each=3)
data1$Location<-data$Location
data1$Location<-factor(data1$Location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))
#hazard space
s3d<-scatterplot3d(data1[,1:3],main="Metacompare risk Matrix Scores",color= color,pch=18,cex.symbols = 2,type="h",grid=T)
text(s3d$xyz.convert(data1[,1:3]),adj=0.2,pos=3,labels =data1[,4],cex= 0.66, col = "black")
legend("bottom",col=color1,legend=levels(data1$Location),pt.bg = color1,pch=18,inset=-0.17,xpd =T,horiz = T)

#bar plot
data_barplot<-data%>%
  group_by(Location)%>%
  summarise(mean=mean(Risk.Score),std=sd(Risk.Score))
data_barplot$Location<-factor(data_barplot$Location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))
bar_plot<-ggplot(data_barplot,aes(x=Location,y=mean))+geom_bar(alpha=0.7,stat = "identity",width = 0.8,fill="#F3E79A")+theme_bw()+geom_errorbar(aes(x=Location,ymin=mean-std, ymax=mean+std), width=.1,position=position_dodge(.9))+labs(x="Location",y="MetaCompare Risk Scores")+theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
bar_plot

#lolipop plot
data1$Sample<-data$Sample
ggplot(data1) +
  geom_segment(aes(x=Sample, xend=Sample, y=0, yend=Risk.Score), color="grey")+
  geom_point( aes(x=Sample, y=Risk.Score),color="#704D9E", size=4,alpha=0.5) +
  theme_bw() +
  xlab("Sample") +
  ylab("Metacompare Risk Score")
#location Co-assembly --------------------------------------
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/Metacompare/Metacompare.xlsx",sheet=1,rowNames=F,colNames = T)
data1<-data[,c(9,12,13)]
data1<-log10(data1[,1:3])
data1$Risk.Score<-data$Risk.Score
data1$Risk.Score<-round(data$Risk.Score,1)
color1<-c("#FB8072","#80B1D3","#BEBADA","#FDB462","#B3DE69")
color<-rep(color1,each=3)
color2<-hcl.colors(5,"sunset")
colo2<-rep(color2,each=3)
data1$Location<-data$Location
data1$Location<-factor(data1$Location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))

ggplot(data1) +
  geom_segment(aes(x=Location, xend=Location, y=0, yend=Risk.Score), color="grey")+
  geom_point( aes(x=Location, y=Risk.Score),color="#704D9E", size=4,alpha=0.5) +
  theme_bw() +
  xlab("Sample") +
  ylab("Metacompare Risk Score")

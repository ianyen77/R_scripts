library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/MGE/ARG_OAP v3.2/MGE_ARGoap_3.2out.xlsx",sheet=2,rowNames=T,colNames =T)
data$sum<-apply(data,1,sum)
data<-data%>%
  arrange(desc(sum))
data$sum<-NULL
data_other<-data[-(1:11),]
others<-apply(data_other,2,sum)
data<-data[(1:11),]
data<-rbind(data,others)
rownames(data)[nrow(data)]<-"Others"
data$`MGEs type`<-rownames(data)
data$`MGEs type`<-factor(data$`MGEs type`,levels = c(data$`MGEs type`[data$`MGEs type`!= "Others"], "Others"))
#MGEs type stacked bar plot                       
plotdata<-data%>%
  gather(key="sample",value="amount",1:15)
color<-c( "#8DD3C7","#FFFFB3" ,"#BC80BD" , "#FB8072" ,"#80B1D3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=amount,fill=`MGEs type`,color=`MGEs type`),stat="identity",alpha=0.7)+
  labs(x=NULL,y=NULL)+scale_fill_manual(values = color )+scale_color_manual(values = color)+theme_bw()+xlab("Sample")+ylab("MGEs abundance normalization against 16S rDNA")+
  theme(axis.title = element_text(size=12.5),axis.text =element_text(size=12)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
#Export width 15.76 length 7.63
#color<-c("#FFFFB3","#8DD3C7","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69","#8DD3C7")

#MGEs type relative abundance stacked bar plot
#這是用來畫ARGtype relative abundance 的script，data必須要先經過trasnform，把所有ARGtype 換成在每個樣本中的百分比
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/MGE/ARG_OAP v3.2/MGE_ARGoap_3.2out.xlsx",sheet=2,rowNames=T,colNames =T)
dbpata1<-as.data.frame(apply(dbpata[,1:15],2,function(x) x/sum(x)))
dbpata<-dbpata1
dbpata[dbpata<0.05]<-0
data_sum<-apply(dbpata,2,sum)
dd<-1-data_sum
data2<-rbind(dbpata,dd)
rownames(data2)[33]<-"Others"
data3<-data2[apply(data2, 1, function(x) !all(x==0)),]
data3$`MGEs type`<-rownames(data3)
data3$`MGEs type`<-factor(data3$`MGEs type`,levels = c(data3$`MGEs type`[data3$`MGEs type`!= "Others"], "Others"))
data4<-gather(data =data3, key ="xx",value = "yf",1:15)
data5<-data4[-which(data4$yf==0),]
#這樣設定是為了要配合前一張圖的顏色
#color<-c("#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F")
color<-c( "#FFFFB3" ,"#BC80BD" , "#FB8072" ,"#80B1D3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
ggplot(data4)+
  geom_bar(aes(x=xx,y=yf,fill=`MGEs type`,color=`MGEs type`),alpha=0.7,stat="identity")+
  labs(x=NULL,y=NULL)+scale_fill_brewer(palette = "Set3")+scale_color_brewer(palette = "Set3")+theme_bw()+labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),axis.text =element_text(size=12)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))



plotdata$location[plotdata$sample=="T1-W-1"]<-"Raw"
plotdata$location[plotdata$sample=="T1-W-2"]<-"Raw"
plotdata$location[plotdata$sample=="T1-W-3"]<-"Raw"
plotdata$location[plotdata$sample=="T2-W-1"]<-"Finished"
plotdata$location[plotdata$sample=="T2-W-2"]<-"Finished"
plotdata$location[plotdata$sample=="T2-W-3"]<-"Finished"
plotdata$location[plotdata$sample=="T3-W-1"]<-"Upstream"
plotdata$location[plotdata$sample=="T3-W-2"]<-"Upstream"
plotdata$location[plotdata$sample=="T3-W-3"]<-"Upstream"
plotdata$location[plotdata$sample=="T4-W-1"]<-"Midstream"
plotdata$location[plotdata$sample=="T4-W-2"]<-"Midstream"
plotdata$location[plotdata$sample=="T4-W-3"]<-"Midstream"
plotdata$location[plotdata$sample=="T5-W-1"]<-"Downstream"
plotdata$location[plotdata$sample=="T5-W-2"]<-"Downstream"
plotdata$location[plotdata$sample=="T5-W-3"]<-"Downstream"
plotdata$location<-factor(plotdata$location,levels=c('Raw', 'Finished', 'Upstream','Midstream','Downstream'))
plotdata%>%
  group_by(`ARGs type`,location)%>%
  summarise(mean=mean(amount),std=sd(amount))%>%
  ggplot(aes(x=location,y=mean,ymin=mean-std, ymax=mean+std,fill=`ARGs type`))+
  geom_bar(aes(y=mean,color=`ARGs type`),position="dodge",stat="identity",alpha=0.7,width = 0.9)+geom_errorbar(width=0.9,position=position_dodge())+theme_bw()+
  labs(x=NULL,y=NULL)+scale_fill_brewer(palette = "Set3")+scale_color_brewer(palette = "Set3")+theme(axis.title = element_text(size=13),axis.text = element_text(size=13),legend.title= element_text(size=12),legend.text = element_text(size=12))+xlab("Sample")+ylab("ARGs abundance normalization against 16S")
  

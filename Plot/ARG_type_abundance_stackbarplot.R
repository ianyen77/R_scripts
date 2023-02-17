library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=2,rowNames=T,colNames =T)
data$sum<-apply(data,1,sum)
data<-data%>%
  arrange(desc(sum))
data$sum<-NULL
data_other<-data[-(1:11),]
others<-apply(data_other,2,sum)
data<-data[(1:11),]
data<-rbind(data,others)
rownames(data)[nrow(data)]<-"others"
data$`ARGs type`<-rownames(data)
data$`ARGs type`<-factor(data$`ARGs type`,levels = c("aminoglycoside","bacitracin","beta-lactam","fosfomycin","macrolide-lincosamide-streptogramin","multidrug","rifamycin","sulfonamide","tetracycline","unclassified","vancomycin","others"),labels = c("Aminoglycoside","Bacitracin","Beta-lactam","Fosfomycin","MLS","Multidrug","Rifamycin","Sulfonamide","Tetracycline","Unclassified","Vancomycin","Others"))
plotdata<-data%>%
  gather(key="sample",value="amount",1:15)
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=amount,fill=`ARGs type`,color=`ARGs type`),stat="identity",alpha=0.9)+
  labs(x=NULL,y=NULL)+scale_fill_brewer(palette = "Set3")+scale_color_brewer(palette = "Set3")+theme(axis.title = element_text(size=13),axis.text = element_text(size=13),legend.title= element_text(size=12),legend.text = element_text(size=12))+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization aganist 16S")+theme_bw()

RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c("#FFFFB3","8DD3C7","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69","#8DD3C7")

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
  labs(x=NULL,y=NULL)+scale_fill_brewer(palette = "Set3")+scale_color_brewer(palette = "Set3")+theme(axis.title = element_text(size=13),axis.text = element_text(size=13),legend.title= element_text(size=12),legend.text = element_text(size=12))+xlab("Sample")+ylab("ARGs abundance normalization aganist 16S")
  

library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/plot/core ARG/CoreARGabundance.xlsx",rowNames = F)
data<-data %>% 
  separate(subtype,into=c("type","subtype"),sep="__")%>%
  gather(key = "Sample",value = "abundance",3:17)
type<-unique(data$type)
#plot---------------------
data1<-data[data$type==type[1],]
colors<-hcl.colors(2,"Sunset")
a<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[1]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[2],]
colors<-hcl.colors(1,"Sunset")
b<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[2]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[3],]
colors<-hcl.colors(3,"Sunset")
c<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[3]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[4],]
colors<-hcl.colors(2,"Sunset")
d<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[4]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[5],]
colors<-hcl.colors(15,"Sunset")
e<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[5]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.7),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[6],]
colors<-hcl.colors(1,"Sunset")
f<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[6]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[7],]
colors<-hcl.colors(1,"Sunset")
g<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[7]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[8],]
colors<-hcl.colors(1,"Sunset")
h<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[8]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[9],]
colors<-hcl.colors(2,"Sunset")
i<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[9]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.9),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[10],]
colors<-hcl.colors(5,"Sunset")
j<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[10]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.85),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

data1<-data[data$type==type[11],]
colors<-hcl.colors(2,"Sunset")
k<-ggplot(data1,aes(x=Sample,y=abundance,group=subtype,color=subtype))+geom_point(size=2)+geom_line(size=0.65)+theme_bw()+labs(x=NULL,y=NULL,title=str_to_sentence(type[11]))+
  scale_color_manual(values=colors)+theme(legend.position=c(0.075,0.85),legend.title = element_blank())+geom_vline(aes(xintercept=12.5),linetype="dashed",color="grey",size=0.8)+
  annotate("rect", xmin=12.5,xmax=16,ymin=(-Inf),ymax=Inf, alpha=0.3, fill="grey90")+geom_point(size=2)+geom_line(size=0.65)

#combine plot
#library(gridExtra)
library(cowplot)
combined_plot <- plot_grid(a,b,c,d,e,f,i,j,k,ncol = 3,labels="auto")
#24.5 13.5

#做一些配水系統下游是由上面這幾種ARGs主宰的分析-------------------------------
data2<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/plot/core ARG/CoreARGabundance_subtype overtake.xlsx",rowNames = F,sheet=3)
data3<-data2[1:8,]
data3<-data3 %>% 
  separate(subtype,into=c("type","subtype"),sep="__") %>% 
  select(-type)
data3<-data3%>%
  gather(key="sample",value="amount",2:16)
#color<-c( "#8DD3C7","#FFFFB3" ,"#BC80BD" , "#FB8072" ,"#80B1D3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
colors<-hcl.colors(8,"Sunset")
a<-ggplot(data3)+
  geom_bar(aes(x=sample,y=amount,fill=subtype,color=subtype),stat="identity",alpha=0.7)+
  scale_fill_manual(values = colors)+scale_color_manual(values = colors)+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization against 16S rDNA")+
  theme(axis.title = element_text(size=12.5),axis.text =element_text(size=11.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
#Midstream/Downstream correlation
library(ggpubr)
data4<-data2[9:10,]
rownames(data4)<-data4$subtype
rownames(data4)[2]<-"arg"
data4<-data4[,-(1:7)]
data4<-as.data.frame(t(data4))
b<-ggscatter(data4,x="sum",y="arg", add = "reg.line", conf.int = TRUE,  color= "#704D9E",alpha=0.6,size=3,
             add.params = list(fill = "lightgray"))+stat_cor(method = "pearson", label.x =0.06, label.y = 0.02)+theme_bw()+labs(x="Total abundance of selected ARGs(8) against 16S rDNA",y="Total ARGs abundance against 16S rDNA",title ="Abundance of ARGs in DWDS")+
  theme(axis.title = element_text(size=12.5),axis.text =element_text(size=11.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
#selected ARGs total ARGs percetage
data5<-data2[11:12,]
data5$subtype<-c("Selected ARGs","Other ARGs")
data5<-gather(data =data5, key ="xx",value = "yf",2:16)

color<-hcl.colors(2,"Sunset")
c<-ggplot(data5)+
  geom_bar(aes(x=xx,y=yf,fill=subtype,color=subtype),alpha=0.7,stat="identity")+scale_fill_manual(values = color)+
  scale_color_manual(values = color)+theme_bw()+labs(x="Sample",y="Relative abundance")+
  theme(axis.title = element_text(size=12.5),axis.text =element_text(size=11.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
combined_plot<- plot_grid(a,b,c,rel_widths = c(1.8,1,1.8),labels="auto")

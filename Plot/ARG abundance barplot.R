library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/ARG/argoap_out.xlsx",sheet=2,rowNames=T,colNames =T)
data<-as.data.frame(t(data))
data$sum<-apply(data,1,sum)
data$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
data$location<-factor(data$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
varaible_group_mean<-data%>%
  group_by(location)%>%
  summarise(type_mean=mean(sum),type_sd=sd(sum))
varaible_group_mean$location<-factor(varaible_group_mean$location,levels=c("Raw","Finished","Upstream","Midstream","Downstream"))
ggplot(varaible_group_mean)+geom_bar(aes(x=location, y=type_mean), stat="identity",fill="#80B1D3")+geom_errorbar(aes(x=location,ymin=type_mean-type_sd, ymax=type_mean+type_sd), width=.2,position=position_dodge(.9))+
  theme_bw()+ labs(x="Location",y="Total ARGs abundance normalization aganist 16S")+theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))+
  geom_line(data=tibble(x=c(2,5),y=c(0.215,0.215)),aes(x=x,y=y),inherit.aes = F,size=0.8)+geom_text(data=tibble(x=3.5,y=0.22),aes(x=x,y=y,label="***"),size=5,inherit.aes = F)+geom_line(data=tibble(x=c(3,5),y=c(0.2,0.2)),aes(x=x,y=y),inherit.aes = F,size=0.8)+geom_text(data=tibble(x=4,y=0.205),aes(x=x,y=y,label="*"),size=5,inherit.aes = F)+
  geom_line(data=tibble(x=c(4,5),y=c(0.185,0.185)),aes(x=x,y=y),inherit.aes = F,size=0.8)+geom_text(data=tibble(x=4.5,y=0.19),aes(x=x,y=y,label="**"),size=5,inherit.aes = F)
RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3")

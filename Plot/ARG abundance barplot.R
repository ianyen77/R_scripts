library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/活頁簿1.xlsx",sheet=2,rowNames=T,colNames =T)
rownames(data)[1]<-"amount"
data<-as.data.frame(t(data))
data$amount<-as.numeric(data$amount)
varaible_group_mean<-data%>%
  group_by(location)%>%
  summarise(type_mean=mean(amount),type_sd=sd(amount))
varaible_group_mean$location<-factor(varaible_group_mean$location,levels=c("Raw","Finished","Upstream","Midstream","Downstream"))
ggplot(varaible_group_mean)+geom_bar(aes(x=location, y=type_mean), stat="identity",fill="#80B1D3")+geom_errorbar(aes(x=location,ymin=type_mean-type_sd, ymax=type_mean+type_sd), width=.2,position=position_dodge(.9))+
  theme_bw()+ labs(x="Location",y="Total ARGs abundance normalization aganist 16S")+theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))

RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c("#8DD3C7","#FFFFB3","#BEBADA","#FB8072","#80B1D3")

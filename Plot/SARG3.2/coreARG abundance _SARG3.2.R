library(openxlsx)
library(tidyverse)
ARG_data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet=1)
core<-read.csv("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/plot/Venn/SARG v3.3.csv")%>%
  select(23)
core_ARG_list<-core[core!=""]
ARG_data_sum<-apply(ARG_data[,2:16], 2, sum)
ARG_data_core<-ARG_data[ARG_data$subtype %in% core_ARG_list,]
ARG_data_core_sum<-apply(ARG_data_core[,2:16], 2, sum)
other_ARG_sum<-ARG_data_sum-ARG_data_core_sum
argplot<-as.data.frame(rbind(ARG_data_core_sum,other_ARG_sum))
argplot<-as.data.frame(apply(argplot[,1:15],2,function(x) x/sum(x)))
argplot$type<-c("Core ARGs","Other ARGs")
data4<-gather(data =argplot, key ="xx",value = "yf",1:15)
color<-c("#BEBADA","#80B1D3")
ggplot(data4)+
  geom_bar(aes(x=xx,y=yf,fill=type,color=type),alpha=0.7,stat="identity")+
  labs(x=NULL,y=NULL)+scale_fill_manual(values = color)+scale_color_manual(values = color)+theme_bw()+labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),axis.text =element_text(size=11.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
#13.14 7.30
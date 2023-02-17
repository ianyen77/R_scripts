library(openxlsx)
library(tidyverse)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=2,rowNames = F)
data<-data[apply(data[2:16], 1, function(x) !all(x==0)),]
colnames(data)[1]<-"ARGs type"
data$`ARGs type`<-str_to_title(data$`ARGs type`) 
Sample<-colnames(data)[2:16]
data<-data%>%
  gather(key = "Sample",value = "abundance",2:16)
Location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
group<-data.frame(cbind(Sample,Location))
data_stat<-merge(data,group,by = "Sample",all.x = T)%>%
  group_by(`ARGs type`,Location)%>%
  summarise(type_mean=mean(abundance),type_std=sd(abundance))
data_stat$Location<-factor(data_stat$Location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
ggplot(data_stat,aes(x=Location,y=type_mean,ymin=type_mean-type_std,ymax=type_mean+type_std))+geom_point()+geom_line(group=1)+theme_bw()+geom_errorbar(width=0.1)+
  facet_wrap(~`ARGs type`,nrow = 5,scales = 'free')+labs(x="Location",y="ARGs abundance normalization aganist 16S")+theme(axis.title = element_text(size=13),axis.text =element_text(size=11.5),strip.text = element_text(size = 11))

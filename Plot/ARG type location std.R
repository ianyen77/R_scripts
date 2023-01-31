library(openxlsx)
library(tidyverse)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/ARG/argoap_out.xlsx",sheet=2,rowNames = T)
data<-as.data.frame(t(data))
data$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
data<-as.data.frame(t(data))
data_sta<-data%>%
  group_by(location)%>%
  summarise(aminoglycoside_mean=mean(aminoglycoside),aminoglycoside_std=sd(aminoglycoside))

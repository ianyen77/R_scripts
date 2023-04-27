library(openxlsx)
library(tidyverse)
dataARG<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,colNames = T,rowNames = T)
data16S<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/MGE/MGEblastout_ARGOAP.xlsx",sheet=3)
dataARG<-as.data.frame(t(dataARG))
x1<-data16S$`#of16Sreads`
d1<-data.frame()


for(i in seq_along(x1)){
  d<-x1[i]*dataARG[i,]
  d1<-rbind(d1,d)
}

d1<-as.data.frame(t(d1))
d1<-round(d1)
write.xlsx(d1,"C:/Users/USER/Desktop/ARGOAP_ouput_without_normlized.xlsx",rowNames=T)

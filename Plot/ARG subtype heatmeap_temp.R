library(pheatmap)
library(openxlsx)
library(tidyverse)
data<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/ARG/argoap_out.xlsx",sheet=1,rowNames = T)
data$sum<-apply(data,1,sum)
data<-data%>%
  arrange(desc(sum))
data<-data[1:30,]
data[data == 0] <- NA
data$sum<-NULL
data_log<-log10(data)
data_log<-data_log[-(1:3),]
pheatmap(data_log,cluster_rows = F,cluster_cols = F)
log
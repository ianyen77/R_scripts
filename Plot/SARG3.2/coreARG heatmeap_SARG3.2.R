library(pheatmap)
library(openxlsx)
library(tidyverse)
library(pheatmap)
data<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet=1,rowNames = F)
core_list<-read.csv("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/plot/Venn/SARG v3.3.csv")[,23]
core_list<-core_list[core_list!=""]
data<-data[data$subtype %in% core_list,]
#write.xlsx(data,"C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/plot/core ARG/CoreARGabundance.xlsx")
data<-separate(data,subtype,into = c("type","subtype"),sep="__")
rownames(data)<-data$subtype
data<-data%>%
  arrange(data$type)
data[data == 0] <- NA
#畫圖
data_plot<-data[,-(1:2)]
data_log<-log10(data_plot) 
pheatmap(data_log,cluster_rows = F,cluster_cols = F,color=colorRampPalette(c("#FCFBFD", "#BCBDDC", "#3F007D"))(50))
#RColorBrewer::brewer.pal(n=9,name="Purples")




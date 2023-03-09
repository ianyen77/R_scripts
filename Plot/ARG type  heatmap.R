library(pheatmap)
library(openxlsx)
library(tidyverse)
library(pheatmap)
data<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=2,rowNames = F)
data_argname<-data
data$Type.level.results<-str_to_title(data$Type.level.results)
data$Type.level.results[data$Type.level.results=="macrolide-lincosamide-streptogramin"]<-"MLS"
rownames(data)<-data$Type.level.results
data$sum<-apply(data[,2:16],1,sum)
data<-data%>%
  arrange(desc(sum))
data$sum<-NULL
data<-data[apply(data[,2:16], 1, function(x) !all(x==0)),]
data[data == 0] <- NA
data_argname<-data[,1:2]
#畫圖
data<-data[,-(1)]
data_log<-log10(data) 
pheatmap(data_log,cluster_rows = F,cluster_cols = F)

pheatmap(data_log,cluster_rows = F,cluster_cols = F,color=colorRampPalette(c("#FCFBFD", "#BCBDDC", "#54278F"))(50))

RColorBrewer::display.brewer.all()
RColorBrewer::display.brewer.pal(n=9,name="Purples")
RColorBrewer::brewer.pal(n=12,name="Purples")

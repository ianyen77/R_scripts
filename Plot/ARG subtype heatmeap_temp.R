library(pheatmap)
library(openxlsx)
library(tidyverse)
library(pheatmap)
data<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames = F)
data<-separate(data,ARGs.abundance.normalization.aganist.16S,into = c("type","subtype"),sep="__")
data_argname<-data
rownames(data)<-data$subtype
data$sum<-apply(data[,3:17],1,sum)
data<-data%>%
  arrange(desc(sum))
data$sum<-NULL
data<-data[1:30,]
data<-data%>%
  arrange(data$type)
data[data == 0] <- NA
data_argname<-data[,1:2]
#data_log<-data_log[-(1:3),]
#添加group資訊
name<-data_argname$subtype
data_argname<-data.frame(data_argname[,1])
rownames(data_argname)<-name
data_argname[data_argname=="macrolide-lincosamide-streptogramin"]<-"MLS"
colnames(data_argname)<-"ARGs Type"
location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
row_ano<-data.frame(cbind(colnames(data),location))
rownames(row_ano)<-row_ano$V1
row_ano$V1<-NULL
#畫圖
data<-data[,-(1:2)]
data_log<-log10(data) 
pheatmap(data_log,cluster_rows = F,cluster_cols = F,annotation_row = data_argname,annotation_col = row_ano)


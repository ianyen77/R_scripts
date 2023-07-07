library(openxlsx)
library(tidyverse)
library(pheatmap)

data<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/all_sample_bbnorm_default_coassembly/bin_quant/bin_quant.xlsx",sheet=1,rowNames = T)


#畫圖
data[data==0]<-NA
data_log<-log10(data)
data_log<-data_log[c(5,6,15,21,22,23),]
pheatmap(data_log,cluster_rows = F,cluster_cols = F)
pheatmap(data_log,cluster_rows = T,cluster_cols = F,color=colorRampPalette(c("#FCFBFD","#EFEDF5" ,"#DADAEB" ,"#BCBDDC","#9E9AC8" ,"#807DBA" ,"#6A51A3", "#54278F", "#3F007D"))(50))

RColorBrewer::display.brewer.all()
RColorBrewer::display.brewer.pal(n=9,name="Purples")
RColorBrewer::brewer.pal(n=9,name="Purples")

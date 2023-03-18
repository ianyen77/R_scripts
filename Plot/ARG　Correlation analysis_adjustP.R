library("openxlsx")
library("Hmisc")
library("tidyverse")
data_ARGsub<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=F,sep.names=" ")
#ARGdata名稱處理
data_ARGsub<-data_ARGsub%>%
  separate(`ARGs abundance normalization aganist 16S`,into=c("type","subtype"),sep="__")
rownames(data_ARGsub)<-data_ARGsub$subtype
argclass<-data_ARGsub[,1:2]
data_ARGsub$type<-NULL
data_ARGsub$subtype<-NULL
data_ARGsub<-data_ARGsub[apply(data_ARGsub, 1, function(x) !all(x==0)),]
#此部分先篩選出在所有樣品中出現超過幾次之species，input型態row="species',col='sample'
data_clean<-data_ARGsub
data_clean[data_clean!=0]<-1
data_clean$times_discover_in_all<-apply(data_clean,1,sum)
data_ARGsub$times_discover_in_all<-data_clean$times_discover_in_all
times_over7_genusdata<-filter(data_ARGsub,times_discover_in_all>=7)
times_over7_genusdata$times_discover_in_all<-NULL
transform_data<-as.data.frame(t(times_over7_genusdata))
#並將其跟環境變數data合併，注意前面的表格已轉置input型態row="sample',col='species'
envdata<-read.xlsx("C:/Users/USER/Desktop/env.xlsx",sheet=1,rowNames=T,sep.names=" ")
mixdata<-cbind(transform_data,envdata)
#以下部分開始進行相關性分析
data<-transform_data
#rcorr()他的input要是matrix
data.matrix<-as.matrix(data)
corr<-rcorr(data.matrix,type= 'spearman')
#P值修正
corr_P_adj <- p.adjust(corr$P, method = 'BH')
matrix_corr_P_adj <- matrix(corr_P_adj,nrow=(length(corr$P)**0.5))
colnames(matrix_corr_P_adj)<-colnames(data)
rownames(matrix_corr_P_adj) <- colnames(data)
matrix_corr_P_adj[matrix_corr_P_adj>= 0.05] <- -1
matrix_corr_P_adj[matrix_corr_P_adj < 0.05 & matrix_corr_P_adj >= 0] <- 1
matrix_corr_P_adj[matrix_corr_P_adj == -1] <- 0
#先輸出一次有顯著相關但相關性未必足夠的矩陣
corr_significiant<-corr$r * matrix_corr_P_adj
#write.xlsx之output要是dataframe
corr_significiant.dataframe<-as.data.frame(corr_significiant)
write.xlsx(corr_significiant.dataframe,file="c:/Users/USER/Desktop/corr_significiant_FAMILIES.xlsx",rowNames=T,colNames=T)
#將計算出來之相關性大於0.8且p小於0.05者留下
corr$r[corr$r < 0.8] <- 0
corr_final <-corr$r * matrix_corr_P_adj
#計算相關性只會有半邊的矩陣(上面是多餘的)，我們只會需要下三角矩陣，且不需要對角矩陣(都為1)
corr_final[!lower.tri(corr_final)] <- 0
#有些數據因為是0所以算不出相關性(na)，去除
corr_final[is.na(corr_final)]<-0
corr_final.dataframe<-as.data.frame(corr_final)
write.xlsx(corr_final.dataframe, 'C:/Users/USER/Desktop/FAMILES_R0.7P0.05.xlsx',rowNames=T,colNames=T)

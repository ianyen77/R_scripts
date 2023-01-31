library("openxlsx")
library("Hmisc")
library("tidyverse")
data_ARGsub<-read.xlsx("C:/Users/USER/Desktop/ARG taxon correlation.xlsx",sheet=1,rowNames=F,sep.names=" ")
data_taxa<-read.xlsx("C:/Users/USER/Desktop/ARG taxon correlation.xlsx",sheet=3,rowNames=F,sep.names=" ")
#ARGdata名稱處理
data_ARGsub<-data_ARGsub%>%
  separate(`ARGs abundance normalization aganist 16S`,into=c("type","subtype"),sep="__")
rownames(data_ARGsub)<-data_ARGsub$subtype
data_ARGsub$type<-NULL
argclass<-data_ARGsub[,1:2]

#計算出現次數
data_clean<-data
data_clean[data_clean!=0]<-1
data_clean$times_discover_in_all<-apply(data_clean,1,sum)
data$times_discover<-data_clean$times_discover_in_all
#這邊可以篩選出現超過幾次的data
times_over8<-filter(data,times_discover>=8)
times_over8$times_discover<-NULL
times_over8<-as.data.frame(t(times_over8))
#因為rcorr()他的input要是matrix
data.matrix<-as.matrix(times_over8)
corr<-rcorr(data.matrix,type= 'spearman')
#corr<-as.list(corrx)
corr$P[corr$P >= 0.05] <- -1
corr$P[corr$P < 0.05 & corr$P >= 0] <- 1
corr$P[corr$P == -1] <- 0
#我們先輸出一次有顯著相關但相關性未必足夠的矩陣
corr_significiant<-corr$r * corr$P
#因為write.xlsx之output要是dataframe
corr_significiant.dataframe<-as.data.frame(corr_significiant)
write.xlsx(corr_significiant.dataframe, 'C:/Users/USER/Desktop/小型testp0.05.xlsx',rowNames=T,colNames=T,keepNA=T)
#接著將計算出來之相關性大於0.8且p小於0.05者留下
corr$r[corr$r < 0.8] <- 0
corr_final <-corr$r * corr$P
#因為計算相關性只會有半邊的矩陣(上面是多餘的)所以我們只會需要下三角矩陣，且不需要對角矩陣(都為1)
corr_final[!lower.tri(corr_final)] <- 0
#有些數據因為是0所以算不出相關性(na)，我們把她去除
corr_final[is.na(corr_final)]<-0
corr_final.dataframe<-as.data.frame(corr_final)
write.xlsx(corr_final.dataframe, 'C:/Users/USER/Desktop/小型test.xlsx',rowNames=T,colNames=T)

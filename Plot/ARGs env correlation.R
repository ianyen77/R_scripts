library(openxlsx)
library(tidyverse)
library(Hmisc)
data_ARGsub<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=F,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/envdata.xlsx",sheet=1)
#去除all zeor row
data_ARGsub<-data_ARGsub[apply(data_ARGsub[,2:16], 1, function(x) !all(x==0)),]
#ARGdata名稱處理
colnames(data_ARGsub)[1]<-"type"
data_ARGsub<-data_ARGsub%>%
  separate(type,into=c("type","subtype"),sep="__")
rownames(data_ARGsub)<-data_ARGsub$subtype
argclass<-data_ARGsub[,1:2]
#data_ARGsub$type<-NULL
data_ARGsub_wide<-data_ARGsub%>%
  gather(key = "sample",value = "amount",3:17)
groupdata<-cbind(sample=c("T1-W-1","T1-W-2","T1-W-3","T2-W-1","T2-W-2","T2-W-3","T3-W-1","T3-W-2","T3-W-3","T4-W-1","T4-W-2","T4-W-3","T5-W-1","T5-W-2","T5-W-3"),location=c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream"))
data_ARGsub_wide<-merge(data_ARGsub_wide,groupdata,by="sample",all.x = T)
data_ARGsub_wide$location<-factor(data_ARGsub_wide$location,levels=c('Raw', 'Finished', 'Upstream','Midstream','Downstream'))   
data_forcorr_subtype<-data_ARGsub_wide%>%
  group_by(subtype,location)%>%
  summarise(mean=mean(amount))%>%
  spread(key =subtype,value=mean)
#merge two df
data<-merge(envata,data_forcorr_subtype,by="location",all = T)
rownames(data)<-data$location
data$location<-NULL
data<-as.data.frame(t(data))
write.xlsx(data,"C:/Users/USER/Desktop/env_aRG.xlsx",rowNames=T)
data.matrix<-as.matrix(data)
corr<-rcorr(data.matrix,type= 'spearman')
#P值修正
corr_P_adj <- p.adjust(corr$P, method = 'BH')
matrix_corr_P_adj <- matrix(corr_P_adj,nrow=(length(corr$P)**0.5))
colnames(matrix_corr_P_adj)<-colnames(corr$P)
rownames(matrix_corr_P_adj) <- rownames(corr$P)
#corr<-as.list(corrx)
corr$P[corr$P >= 0.01] <- -1
corr$P[corr$P < 0.01 & corr$P >= 0] <- 1
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
corr_final.dataframe<-corr_final.dataframe[apply(corr_final.dataframe[,2:16], 1, function(x) !all(x==0)),]

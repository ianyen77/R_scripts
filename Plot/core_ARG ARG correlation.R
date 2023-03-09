library(openxlsx)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=F,colNames =T)
data_core<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/plot/venn/ARG_venn.xlsx")
data_core1<-data_core$`Raw|Finished|Upstream|Midstream|Downstream`
data_core1<-data_core1[!is.na(data_core1)]
data_core1<-as.data.frame(cbind(data_core1,rep(1,26)))
colnames(data_core1)<-c("ARGs.abundance.normalization.aganist.16S","core")
data_core<-merge(data,data_core1,all.y = T,by=("ARGs.abundance.normalization.aganist.16S"))

data_core_sum<-apply(data_core[,2:16],2,sum)
data_sum<-apply(data[,2:16],2,sum)
data1<-as.data.frame(cbind(total_arg=data_sum,core_arg=data_core_sum))
data1<-data1[-(1:3),]
ggplot(data = data1,aes(x=core_arg,y=total_arg))+
  geom_point(color="#BEBADA",size=3)+geom_smooth(method="lm",color="#BEBADA")+theme_bw()+
  labs(y = "Total ARGs abundnace aganists 16S",x="Core ARGs abundnace aganists 16S") +theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))

summary(lm(total_arg ~ core_arg, data=data1))
cor.test(data1$core_arg,data1$total_arg,method="pearson")
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")

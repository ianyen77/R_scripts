library("openxlsx")
library("ggplot2")
library("vegan")
library("tidyverse")
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/台水計畫/DATA/NGS/台水計畫豐水期 amplicon sequencing_基米RAW/豐水期amplicon sequencing data analysis/relative_abundance table.xlsx/families.xlsx",rowNames = T,colNames = T)
data[data<0.05]<-0
data_sum<-apply(data,2,sum)
dd<-1-data_sum
data2<-rbind(data,dd)
#要幾個階層就放幾個
rownames(data2)[397]<-"Other;Others;Other;Others;Others"
data3<-data2[apply(data2, 1, function(x) !all(x==0)),]
data3$family<-rownames(data3)
rownames(data3)<-NULL
data4<-gather(data =data3, key ="xx",value = "yf",1:15)
data4<-separate(data4,family,into=c("domain","phyla","Class","Order","family"),sep=";")
data5<-data4[-which(data4$yf==0),]
ggplot(data5)+
  geom_bar(aes(x=xx,y=yf,fill=family),color='black',stat="identity")+
  labs(x=NULL,y=NULL)

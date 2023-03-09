library(tidyverse)
library(openxlsx)
#這是用來畫ARGtype relative abundance 的script，data必須要先經過trasnform，把所有ARGtype 換成在每個樣本中的百分比
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=2,rowNames=F,colNames=T,sep.names=" ")
dbpata1<-as.data.frame(apply(dbpata[,2:16],2,function(x) x/sum(x)))
rownames(dbpata1)<-dbpata$`Type level results`
dbpata1$sum<-apply(dbpata1[,2:15],1,sum)
dbpata1$average<-(dbpata1$sum)/15
dbpata1$type<-rownames(dbpata1)
dbpata1<-dbpata1%>%
  arrange(desc(average))
dbpata1<-dbpata1[1:10,]
other<-1-sum(dbpata1$average)
dbpata1<-dbpata1[,(17:18)]
dbpata1<-rbind(dbpata1,Others=c(other,"Others"))
dbpata1$type<-str_to_title(dbpata1$type)
dbpata1$type[dbpata1$type=="Macrolide-Lincosamide-Streptogramin"]<-"MLS"
ggplot(dbpata1, aes(x="", y=average, fill=type)) +
  geom_bar(stat="identity", width=1,alpha=0.8) +
  coord_polar("y", start=0)+theme_void()+scale_fill_manual(color = color)
color<-c("#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F")

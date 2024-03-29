library(openxlsx)
library(tidyverse)
library(RColorBrewer)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv2.2/ARGoap_out.xlsx",sheet=2,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/standard_db/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
#dbpata <-as.data.frame(t(dbpata))
#dbpata<-dbpata[,-(1:3)]
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
dbpata$sum<-apply(dbpata,1,sum)
dbpata<-dbpata[order(dbpata$sum,decreasing = T),]
dbpata<-dbpata[-(11:nrow(dbpata)),]
dbpata$sum<-NULL
dbpata$`ARGs Type`<-rownames(dbpata)
dbpata$`ARGs Type`<-str_to_title(dbpata$`ARGs Type`)
dbpata$`ARGs Type`[dbpata$`ARGs Type`=="Macrolide-Lincosamide-Streptogramin"]<-"MLS"
dbpata$`ARGs Type`[dbpata$`ARGs Type`=="Beta-Lactam"]<-"Beta-lactam"
dbpata1<-gather(data =dbpata, key ="sample",value = "value",1:(ncol(dbpata)-1))
plot<-dbpata1%>%
  mutate(type=fct_reorder(`ARGs Type`,desc(value)))%>%
  ggplot(aes(x=type,y=value))+geom_boxplot(fill="#BEBADA",alpha=0.7)+
  theme_bw()+geom_point(size=1)+labs(x="ARGs Type",y="The abundance of ARG normalization against 16S rDNA")+theme(axis.title = element_text(size=12.5),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
plot

RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-"#80B1D3"

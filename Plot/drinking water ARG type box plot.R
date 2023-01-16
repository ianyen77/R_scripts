library(openxlsx)
library(tidyverse)
library(vegan)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/ARG/argoap_out.xlsx",sheet=2,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
dbpata <-as.data.frame(t(dbpata))
dbpata<-dbpata[,-(1:3)]
dbpata$`ARGs Type`<-rownames(dbpata)
dbpata$`ARGs Type`[dbpata$`ARGs Type`=="macrolide-lincosamide-streptogramin"]<-"MLS"
dbpata1<-gather(data =dbpata, key ="sample",value = "value",1:12)
plot<-dbpata1%>%
  ggplot(aes(x=reorder(`ARGs Type`,value),y=value))+geom_boxplot()+#geom_errorbar(aes(ymin=type_mean-type_sd, ymax=type_mean+type_sd), width=.2,position=position_dodge(.9))+
  scale_fill_brewer(palette="Set3")+theme_bw()+geom_point()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),legend.title = element_text(hjust=0.5))+ylim(18, 35)+
  geom_line(data=tibble(x=c(2,3),y=c(32.3,32.3)),aes(x=x,y=y),inherit.aes = F,size=0.8)+geom_text(data=tibble(x=2.5,y=32.6),aes(x=x,y=y,label="*"),size=5,inherit.aes = F)+
  guides(fill=guide_legend(title="Plant"))+ylab("Temperature (°C)") + xlab("Plant")
#+geom_line(data=tibble(x=c(1,3),y=c(29,29)),aes(x=x,y=y),inherit.aes = F,size=0.8)+geom_text(data=tibble(x=2,y=29.3),aes(x=x,y=y,label="N.S"),size=5,inherit.aes = F)+
plot

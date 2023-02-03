library(openxlsx)
library(tidyverse)
#這是一個提
dbpata<-read.xlsx("C:/Users/USER/Desktop/枯水_ampliccon sequencing data analysis/raw_data_xlsx/genus_raw.xlsx",sheet=1,rowNames=F,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/枯水_ampliccon sequencing data analysis/group.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/枯水_ampliccon sequencing data analysis/env.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
#dbpata<-as.data.frame(t(dbpata))
output<- vector(mode = "list")
for (i in 2:length(colnames(dbpata))){
  list1<-dbpata%>%
    filter(,dbpata[,i]>=0.005)
  list1<-list1[,1]%>%
    as.data.frame()
  output<-c(output,list1)
}
x<-colnames(dbpata)[2:16]
names(output)<-x
write.xlsx(output,file="c:/Users/USER/Desktop/xx.xlsx",sep="")
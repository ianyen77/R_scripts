library(openxlsx)
library(tidyverse)
#這是一個提
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=F,colNames=T,sep.names=" ")
#dbpata<-as.data.frame(t(dbpata))
output<- vector(mode = "list")
#第一個回圈可以計算各別sample
for (i in 2:length(colnames(dbpata))){
  list1<-dbpata%>%
    filter(dbpata[,i]!=0)
  list1<-list1[,1]%>%
    as.data.frame()
  output<-c(output,list1)
}
x<-colnames(dbpata)[2:16]
names(output)<-x
output2<- vector(mode = "list")
#此回圈用於計算各組sample所有>0.005的總和(本質上在計算三重複各個sample出現所有人有誰，剔除重複)
for(i in seq(1,15,by =3)){
  list1<-as.data.frame(unique(c(output[[i]],output[[i+1]],output[[i+2]])))
  output2<-c(output2,list1)
}
names(output2)<-c("Raw","Finished","Upstream","Midstream","Downstream")

write.xlsx(output2,file="c:/Users/USER/Desktop/ARG_occurance.xlsx")

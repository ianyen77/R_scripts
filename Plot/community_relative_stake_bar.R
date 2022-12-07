library("openxlsx")
library("ggplot2")
library("vegan")
library("tidyverse")
data<-read.csv("C:/Users/USER/Desktop/rel_abundance_table/phyla.csv",header=T)
rownames(data) <- data$X
data$X<-NULL
data[data<0.05]<-0
data_sum<-apply(data,2,sum)
dd<-1-data_sum
data2<-rbind(data,dd)
rownames(data2)[35]<-"other; Others"
data3<-data2[apply(data2, 1, function(x) !all(x==0)),]
data3$phyla<-rownames(data3)
rownames(data3)<-NULL
data4<-gather(data =data3, key ="xx",value = "yf",1:15)
data4<-separate(data4,phyla,into=c("domain","phyla"),sep=";")
data5<-data4[-which(data4$yf==0),]
ggplot(data5)+
  geom_bar(aes(x=xx,y=yf,fill=phyla),color='black',stat="identity")+
  labs(x=NULL,y=NULL)

library(openxlsx)
library(UpSetR)
library(tidyverse)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",rowNames=T)
data<-data[apply(data, 1, function(x) !all(x==0)),]
data[data!=0]<-1
group<-c(rep(c("Raw","Finished","Upstrem","Midstream","Downstream"),each=3))
x<-data.frame()
for(i in seq(1,15,3)){
  if(i==1){
        x<-as.data.frame(apply(data[i:(i+2)],1,sum))
  }
  else{
    x<-cbind(x,as.data.frame(apply(data[i:(i+2)],1,sum)))
  }
}
colnames(x)<-c("Raw","Finished","Upstrem","Midstream","Downstream")
x[x!=0]<-1

upset(x, sets =c("Raw","Finished","Upstrem","Midstream","Downstream"), mb.ratio = c(0.55, 0.45), order.by = "freq", 
      nsets = 7, number.angles = 0, point.size = 6, line.size = 1, mainbar.y.label = "title",
      sets.x.label = "title", text.scale = c(2, 2, 2, 2, 2, 2))

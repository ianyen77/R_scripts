library(openxlsx)
library(UpSetR)
library(tidyverse)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",rowNames=T)
#Remove All 0 rows
data<-data[apply(data, 1, function(x) !all(x==0)),]
data[data!=0]<-1
#stat occur time of an ARGs in a location(etc. RAW, Finished)
x<-data.frame()
for(i in seq(1,15,3)){
  if(i==1){
        x<-as.data.frame(apply(data[i:(i+2)],1,sum))
  }
  else{
    x<-cbind(x,as.data.frame(apply(data[i:(i+2)],1,sum)))
  }
}
#change colnames
colnames(x)<-c("Raw","Finished","Upstrem","Midstream","Downstream")
x[x!=0]<-1

#plot
upset(x, sets =c("Raw","Finished","Upstrem","Midstream","Downstream"),nsets = 15,
      nintersects = 52,
      order.by = "freq",
      mb.ratio = c(0.6,0.4),
      text.scale = 2,
      mainbar.y.label = "Numbers of shared ARG sutypes",
      sets.x.label = "Number of ARG subtype numbers",
      main.bar.color = "#704D9E", matrix.color = "darkgrey",
      sets.bar.color = "#F9B282",
      shade.color = "#F3E79A",
      shade.alpha = 0.7, matrix.dot.alpha = 0.9,      queries = list(
        list(
          query = intersects, 
          params = list("Raw","Finished","Upstrem","Midstream","Downstream"),
          color = "#ED7C97",active = T)))
?upset

hcl.colors(5,"sunset")



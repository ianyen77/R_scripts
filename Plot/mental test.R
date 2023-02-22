library(openxlsx)
library(ggfortify)
library(hrbrthemes)
library(tidyverse)
library(ggbeeswarm)
library(lvplot)
library(hexbin)
library(vegan)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/genus_rel_table.xlsx",sheet=1,rowNames=F,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/envdata.xlsx",sheet=2,rowNames=T,colNames=T,sep.names=" ")
rownames(dbpata)<-dbpata$Genus
dbpata<-dbpata[,-(1:7)]
dbpata<-as.data.frame(t(dbpata))
dbpata<-dbpata[-(1:3),]
envata<-envata[-(1:3),]
#dbpata<-dbpata[-c(2,6,11),]
#envata<-envata[-c(2,6,11),]
dist.abund <- vegdist(dbpata, method = "bray")
dist.abund<-as.matrix(dist.abund)
each_column_dist<-envata%>%
  summarise_all(~dist(.,method = "euclidean"))%>%
  as.data.frame()
mantel.pairwise<-function(){
  df<- data.frame()
  n<-length(colnames(envata))
  for(i in 1:n){
    x<-mantel(each_column_dist[,i],dist.abund, method = "spearman", permutations = 9999, na.rm = TRUE)
    df[1,i]<-x$statistic
    df[2,i]<-x$signif
  }
  colnames(df)<-colnames(each_column_dist)
  rownames(df)<-c("Mantel statistic r","Significance")
  assign("mantel_output",df,envir = globalenv())
}
mantel.pairwise()
#對整個環境參數的矩陣跟整個物種矩陣做相關性分析
scale.allenv <- scale(envata, center = TRUE, scale = TRUE)
dist.allenv <-dist(scale.allenv, method = "euclidean")
dist.allenv.matirx<-as.matrix(dist.allenv)
mantel(dist.allenv.matirx,dist.abund,method = "spearman", permutations = 9999, na.rm = TRUE)

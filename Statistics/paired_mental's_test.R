library(openxlsx)
library(ggfortify)
library(hrbrthemes)
library(tidyverse)
library(ggbeeswarm)
library(lvplot)
library(hexbin)
library(vegan)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/台水計畫/DATA/NGS/豐水期amplicon sequencing data analysis/relative_abundance table.xlsx/genus.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/台水計畫/DATA/NGS/豐水期amplicon sequencing data analysis/group_豐.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/lab/台水計畫/DATA/NGS/豐水期amplicon sequencing data analysis/env_豐.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata<-as.data.frame(t(dbpata))
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
write.xlsx(mantel_output,"C://Users/USER/Desktop/lab/台水計畫/DATA/NGS/豐水期amplicon sequencing data analysis/Mantel's test/包含原水個別參數mantel.xlsx",colNames=T,rowNames=T)
#對整個環境參數的矩陣跟整個物種矩陣做相關性分析
scale.allenv <- scale(envata, center = TRUE, scale = TRUE)
dist.allenv <-dist(scale.allenv, method = "euclidean")
dist.allenv.matirx<-as.matrix(dist.allenv)
mantel(dist.allenv.matirx,dist.abund,method = "spearman", permutations = 9999, na.rm = TRUE)

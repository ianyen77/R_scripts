library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
data_taxa<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/phyla_rel_table.xlsx",sheet=1)
#data_taxa<-data_taxa[,-(1:5)]
#data_taxa<-data_taxa[,-2]
data_taxa<-data_taxa%>%
  separate(Phyla,into=c("n","Genus"),sep = "__")
rownames(data_taxa)<-data_taxa$Genus
data_taxa<-data_taxa[,-(1:7)]
?percent
#data_taxa<-data_taxa[apply(data_taxa, 1, function(x) !all(x==0)),]
#taxa top 20 abundance
data_taxa$sum<-apply(data_taxa,1,sum)
data_taxa<-arrange(data_taxa, desc(sum))
data_taxa<-data_taxa[1:10,]
data_taxa$sum<-NULL
data_sum<-apply(data,2,sum)
x<-nrow(data_taxa)+1
data_taxa[x,]<-(1-data_sum)
rownames(data_taxa)[x]<-"Others"
data_taxa<-data_taxa[,1:15]*100
data_taxa$Genus<-rownames(data_taxa)
#data_taxa<-data_taxa[,-(1:6)]
#data_taxa<-data_taxa[,-2]
data_taxa$Genus<-factor(data_taxa$Genus)
data_taxa$Genus<-relevel(data_taxa$Genus,ref="Others")
data4<-gather(data =data_taxa, key ="Sample",value = "Abundance",1:15)
#pheatmap(data_taxa,cluster_rows = F,cluster_cols =F)
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
Genu<-data4$Genus
ggplot(data4)+
  geom_point(aes(x=Sample,y=fct_rev(Genus),size=Abundance),color="#8DD3C7",alpha=0.7)+theme_bw()+labs(y="Family")+scale_size(range=c(0,8),breaks=c(10,30,50,70,90))


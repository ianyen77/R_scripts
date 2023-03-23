library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
data_taxa<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/genus_rel_table.xlsx",sheet=1)
#data_taxa<-data_taxa[,-(1:5)]
#data_taxa<-data_taxa[,-2]
data_taxa<-data_taxa%>%
  separate(Genus,into=c("n","Genus"),sep = "__")
rownames(data_taxa)<-data_taxa$Genus
data_taxa<-data_taxa[,-(1:8)]
?percent
#data_taxa<-data_taxa[apply(data_taxa, 1, function(x) !all(x==0)),]
#taxa top 20 abundance
data_taxa$sum<-apply(data_taxa,1,sum)
data_taxa<-arrange(data_taxa, desc(sum))
data_taxa<-data_taxa[1:10,]
data_taxa$sum<-NULL
data_sum<-apply(data_taxa,2,sum)
x<-nrow(data_taxa)+1
data_taxa[x,]<-(1-data_sum)
rownames(data_taxa)[x]<-"Others"
#data_taxa<-data_taxa[,1:15]*100
data_taxa$Genus<-rownames(data_taxa)
#data_taxa<-data_taxa[,-(1:6)]
#data_taxa<-data_taxa[,-2]
data_taxa$Genus<-factor(data_taxa$Genus)
data_taxa$Genus<-relevel(data_taxa$Genus,ref="Others")
data4<-gather(data =data_taxa, key ="Sample",value = "Abundance",1:15)
#pheatmap(data_taxa,cluster_rows = F,cluster_cols =F)
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
#color<-c("#FDB462","#80B1D3","#FB8072" ,"#BEBADA","#FFFFB3","#8DD3C7","#CCEBC5"  ,"#B3DE69" ,"#FCCDE5","#BC80BD","#D9D9D9"  ,"#FFED6F")
Genu<-data4$Genus
ggplot(data4)+
  geom_point(aes(x=Sample,y=fct_rev(Genus),size=Abundance),color="#BEBADA",alpha=0.8)+theme_bw()+labs(y="Phylum")+scale_size(range=c(0,7),breaks=c(5,10,15,20,25,30))+guides(fill=guide_legend(title="Abudnace"))

ggplot(data4)+
  geom_bar(aes(x=Sample,y=Abundance,fill=Genus,color=Genus),alpha=0.7,stat="identity")+
  scale_fill_brewer(palette = "Set3")+scale_color_brewer(palette = "Set3")+theme_bw()+
  labs(x="Sample",y="Relative abundance")
theme(axis.title = element_text(size=12),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))

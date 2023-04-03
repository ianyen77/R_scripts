library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/ARC_phyla_整理.xlsx",sheet=1)
data_plasflow<-read.xlsx("C:/Users/USER/Desktop/ARC_plasflow out.xlsx",sheet=2,colNames = T)
data_plasflow<-data_plasflow %>% 
  separate(label,into=c("type","pla_taxon"))%>%
  group_by(Sample,type)%>%
  count()
data<-merge(data_contig,data_plasflow,by="contig",all = T)
write.xlsx(data_plasflow,"C:/Users/USER/Desktop/zz.xlsx")
data<-read.xlsx("C:/Users/USER/Desktop/zz.xlsx")
RColorBrewer::brewer.pal(12,"Set3")
color<-c("#80B1D3","#BEBADA","#FFFFB3")
ggplot(data)+
  geom_bar(aes(x=Sample,y=Percentage,fill=type,color=type),alpha=0.7,stat="identity",width = 0.8)+
  scale_fill_manual(values = color)+scale_color_manual(values = color)+theme_bw()+
  labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),axis.text  = element_text(size=9),legend.title= element_text(size=12.5),legend.text = element_text(size=12))

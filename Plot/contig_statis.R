library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(ggsankey)
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/ARC_phyla_整理.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"

##ARG orf type coverage
data_stat<-data_contig%>%
  group_by(Sample,type)%>%
  summarise_at(vars(orf_coverage), funs(sum))
data_stat$Sample[data_stat$Sample=="T1"]<-"Raw"
data_stat$Sample[data_stat$Sample=="T2"]<-"Finished"
data_stat$Sample[data_stat$Sample=="T3"]<-"Upstream"
data_stat$Sample[data_stat$Sample=="T4"]<-"Midstream"
data_stat$Sample[data_stat$Sample=="T5"]<-"Downstream"
data_stat$type<-str_to_sentence(data_stat$type)
data_stat$type[data_stat$type=="Macrolide-lincosamide-streptogramin"]<-"MLS"
data_stat$Sample<-factor(data_stat$Sample,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
color1<-c( "#FFFFB3" ,"#BC80BD" , "#FB8072" ,"#80B1D3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
color<- hcl.colors(14, "Sunset")
ggplot(data_stat)+geom_bar(aes(x=Sample,y=orf_coverage,fill=type,color=type),stat="identity",alpha=0.7,width = 0.8)+
  scale_fill_manual("Location",values = color)+scale_color_manual("Location",values = color)+theme_bw()+labs(x="ARG Type",y="ARC coverage (x/GB)")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))
##ARG orf subtype coverage
data_stat<-data_contig%>%
  group_by(Sample,type,subtype)%>%
  summarise_at(vars(orf_coverage), funs(sum))

data_stat<-data_contig%>%
  group_by(Sample,type,subtype,contig_phyla,contig_taxon.x)%>%
  summarise_at(vars(orf_coverage), funs(sum))

  count(type)%>%
  mutate(type_ACC_percent=n/sum(n))
data_phyl_stat<-data_contig%>%
  count(contig_phyla)%>%
  mutate(phyla_ACC_percent=n/248)


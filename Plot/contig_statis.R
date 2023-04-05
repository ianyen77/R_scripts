library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(ggsankey)
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/ARC_phyla_整理.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"
##ARG type percentage----------------------------------
data_stat<-data_contig%>%
  group_by(type)%>%
  count()%>%
  ungroup()%>%
  mutate(sum=sum(n))%>%
  group_by(type)%>%
  mutate(cover=n/sum) 

#ARG host percentage--------------------------------
data_stat<-data_contig%>%
  group_by(contig_phyla)%>%
  count()%>%
  ungroup()%>%
  mutate(sum=sum(n))%>%
  group_by(contig_phyla)%>%
  mutate(cover=n/sum) 
#ARG sample host perecntage----------------------
data_stat<-data_contig%>%
  group_by(Sample,contig_phyla)%>%
  count()%>%
  ungroup()%>%
  group_by(Sample)%>%
  mutate(n_S=sum(n))%>%
  mutate(type_ACC_percent=n/n_S)
convert_column_names <- function(data, column) {
  column_names <- c("T1", "T2", "T3", "T4", "T5")
  new_names <- c("Raw", "Finished", "Upstream", "Midstream", "Downstream")
  
  for (i in seq_along(column_names)) {
    data[[column]][data[[column]] == column_names[i]] <- new_names[i]
  }
  
  return(data)
}
data_stat <- convert_column_names(data_stat, "Sample")
data_stat$Sample<-factor(data_stat$Sample,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
color<-c( "#8DD3C7","#FFFFB3" ,"#FB8072","#BEBADA","#B3DE69","#80B1D3" , "#FDB462","#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
color1<-hcl.colors(7,"sunset")
#relative abundance(ARC number)
ggplot(data_stat)+geom_bar(aes(x=Sample,y=type_ACC_percent,fill=contig_phyla,color=contig_phyla),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_manual("ARC Phyla",values = color1)+scale_color_manual("ARC Phyla",values = color1)+theme_bw()+labs(x="Location",y="Relative abundance")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))



##ARG orf type coverage---------------------------------------------------------------
data_stat<-data_contig%>%
  group_by(Sample,type)%>%
  summarise_at(vars(orf_coverage), funs(sum))
#這個函式可以用來將Sample名稱轉成Location名稱
convert_column_names <- function(data, column) {
  column_names <- c("T1", "T2", "T3", "T4", "T5")
  new_names <- c("Raw", "Finished", "Upstream", "Midstream", "Downstream")
  
  for (i in seq_along(column_names)) {
    data[[column]][data[[column]] == column_names[i]] <- new_names[i]
  }
  
  return(data)
}
data_stat <- convert_column_names(data_stat, "Sample")
#data_stat$Sample[data_stat$Sample=="T1"]<-"Raw"
#data_stat$Sample[data_stat$Sample=="T2"]<-"Finished"
#data_stat$Sample[data_stat$Sample=="T3"]<-"Upstream"
#data_stat$Sample[data_stat$Sample=="T4"]<-"Midstream"
#data_stat$Sample[data_stat$Sample=="T5"]<-"Downstream"
data_stat$type<-str_to_sentence(data_stat$type)
data_stat$type[data_stat$type=="Macrolide-lincosamide-streptogramin"]<-"MLS"
data_stat$Sample<-factor(data_stat$Sample,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
color1<-c( "#FFFFB3" ,"#BC80BD" , "#FB8072" ,"#80B1D3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
color<- hcl.colors(14, "Sunset")

ggplot(data_stat)+geom_bar(aes(x=Sample,y=orf_coverage,fill=type,color=type),stat="identity",alpha=0.7,width = 0.8)+
  scale_fill_manual("Location",values = color)+scale_color_manual("Location",values = color)+theme_bw()+labs(x="ARG Type",y="ARC coverage (x/GB)")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))

##core ARG list----------------------------------------------------------------------------
data_core<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/plot/venn/ARG_venn.xlsx")
data_core1<-data_core$`Raw|Finished|Upstream|Midstream|Downstream`
data_core1<-data_core1[!is.na(data_core1)]
data_core1<-as.data.frame(cbind(data_core1,rep(1,26)))
colnames(data_core1)<-c("ARGs.abundance.normalization.aganist.16S","core")
core_arg_list<-as.data.frame(data_core1)%>%
  separate(`ARGs.abundance.normalization.aganist.16S`,into=c("type","subtype"),sep = "__")
core_arg_data_contig<-data_contig[data_contig$subtype %in% core_arg_list$subtype,]
data_stat<-core_arg_data_contig%>% 
  select(Sample,type,subtype,contig_phyla,contig_taxon.x)%>%
  group_by(type,subtype,contig_taxon.x)%>%
  count()
  
##ARG orf subtype coverage-----------------------------------------------------
data_stat<-data_contig%>%
  group_by(Sample,type,subtype)%>%
  summarise_at(vars(orf_coverage), funs(sum))

data_stat<-data_contig%>%
  group_by(Sample,type,subtype,contig_phyla,contig_taxon.x)%>%
  summarise_at(vars(orf_coverage), funs(sum))
#
  count(type)%>%
  mutate(type_ACC_percent=n/sum(n))
data_phyl_stat<-data_contig%>%
  count(contig_phyla)%>%
  mutate(phyla_ACC_percent=n/248)


library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data_contig<-read.xlsx("C:/Users/USER/Desktop/ARC_analysis_phyla_plasflow.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"
data_stat<-data_contig%>%
  group_by(type.x,plas_type) %>% 
  count()
##draw ARG type plasflow output
data_stat_percentage<-data_stat%>%
  group_by(type.x)%>%
  mutate(percent=n/sum(n))%>%
  mutate(sum=sum(n))
data_stat_percentage$type.x<-str_to_sentence(data_stat_percentage$type.x)
data_stat_percentage$type.x[data_stat_percentage$type.x=="Macrolide-lincosamide-streptogramin"]<-"MLS"
data_stat_percentage$plas_type<-str_to_sentence(data_stat_percentage$plas_type)
color<-c( "#80B1D3","#BEBADA"  ,"#FFFFB3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
#relative abundance(ARC number)
ggplot(data_stat_percentage)+geom_bar(aes(x=type.x,y=percent,fill=plas_type,color=plas_type),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_manual("Location",values = color)+scale_color_manual("Location",values = color)+theme_bw()+labs(x="ARG Type",y="Relative abundance")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))
#sum(ARC number)
ggplot(data_stat_percentage)+geom_bar(aes(x=type.x,y=n,fill=plas_type,color=plas_type),stat="identity",alpha=0.7,width = 0.8)+
  scale_fill_manual("Location",values = color)+scale_color_manual("Location",values = color)+theme_bw()+labs(x="ARG Type",y="ARC number")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))

#coverage sum
data_stat_cover_sum<-data_contig%>%
  group_by(type.x,plas_type)%>%
  summarise_at(vars(contig_coverage), funs(sum))
data_stat_cover_sum$type.x<-str_to_sentence(data_stat_cover_sum$type.x)
data_stat_cover_sum$type.x[data_stat_cover_sum$type.x=="Macrolide-lincosamide-streptogramin"]<-"MLS"
ggplot(data_stat_cover_sum)+geom_bar(aes(x=type.x,y=contig_coverage,fill=plas_type,color=plas_type),stat="identity",alpha=0.7,width = 0.8)+
  scale_fill_manual("Location",values = color)+scale_color_manual("Location",values = color)+theme_bw()+labs(x="ARG Type",y="ARC coverage (x/GB)")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))

##coverage relative abundance
data_stat_cover_sum<-data_contig%>%
  group_by(type.x,plas_type)%>%
  summarise_at(vars(contig_coverage), funs(sum))%>%
  group_by(type.x)%>%
  mutate(percent=contig_coverage/sum(contig_coverage))%>%
  mutate(sum=sum(contig_coverage))
data_stat_cover_sum$type.x<-str_to_sentence(data_stat_cover_sum$type.x)
data_stat_cover_sum$type.x[data_stat_cover_sum$type.x=="Macrolide-lincosamide-streptogramin"]<-"MLS"
ggplot(data_stat_cover_sum)+geom_bar(aes(x=type.x,y=percent,fill=plas_type,color=plas_type),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_manual("Location",values = color)+scale_color_manual("Location",values = color)+theme_bw()+labs(x="ARG Type",y="Covereage Relative abundance")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))


##location plasflow output(ARC number)
data_contig_Sample<-read.xlsx("C:/Users/USER/Desktop/ARC_plasflow out.xlsx",sheet=2)
data_contig_Sample<-data_contig_Sample%>%
  separate(label,into=c("type","NUL"))
data_contig_Sample$id<-NULL
data_contig_Sample$NUL<-NULL
data_contig_Sample$Sample[data_contig_Sample$Sample=="T1"]<-"Raw"
data_contig_Sample$Sample[data_contig_Sample$Sample=="T2"]<-"Finished"
data_contig_Sample$Sample[data_contig_Sample$Sample=="T3"]<-"Upstream"
data_contig_Sample$Sample[data_contig_Sample$Sample=="T4"]<-"Midstream"
data_contig_Sample$Sample[data_contig_Sample$Sample=="T5"]<-"Downstream"
data_contig_Sample$type<-str_to_sentence(data_contig_Sample$type)

data_contig_Sample_sta<-data_contig_Sample%>%
  group_by(Sample,type)%>%
  count()%>%
  group_by(Sample)%>%
  mutate(percent=n/sum(n))%>%
  mutate(sum=sum(n))
data_contig_Sample_sta$Sample<-factor(data_contig_Sample_sta$Sample,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))

ggplot(data_contig_Sample_sta)+geom_bar(aes(x=Sample,y=percent,fill=type,color=type),alpha=0.7,stat="identity")+
  scale_fill_manual("Gene Location",values = color)+scale_color_manual("Gene Location",values = color)+theme_bw()+labs(x="Location",y="Relative abundance")+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))  


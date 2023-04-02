library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/ARC_phyla_整理.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"
data_stat<-data_contig%>%
  group_by(Sample)%>%
  count(type)%>%
  mutate(type_ACC_percent=n/sum(n))
data_phyl_stat<-data_contig%>%
  count(contig_phyla)%>%
  mutate(phyla_ACC_percent=n/248)


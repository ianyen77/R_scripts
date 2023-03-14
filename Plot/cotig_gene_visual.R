library("openxlsx")
library("gggenes")
library("tidyverse")
ARC_data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/location_co_assembly/blastx_sm70_cover70_e10/Megan/genus/ARC-host/T5-W_ARC_classfication.xlsx")
VF_data<-read.table("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/location_co_assembly/blastx_sm70_cover70_e10/VF_blast/T5-WVF.dmnd")
colnames(VF_data)<-c("qseqid", "gene", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
VF_data<-VF_data%>%
  separate(qseqid,sep="_",into=c("x1","x2","x3"),remove=F)
VF_data<-unite(VF_data,"contig",x1,x2,sep="_")
VF_data$x3<-NULL
ggplot(example_genes, aes(xmin = start, xmax = end, y = molecule, fill = gene)) +
  geom_gene_arrow() +
  facet_wrap(~ molecule, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3")
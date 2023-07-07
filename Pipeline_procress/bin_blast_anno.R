library(tidyverse)
library(openxlsx)
filenames<-list.files("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/all_sample_bbnorm_default_coassembly/binning_blast/bin_50_10_ARGblast/",full.names =T,pattern= ".dmnd")
#create your own percentage
#df$mypercentage<-df$new_est_reads/sum(df$new_est_reads)
x<-data.frame()
ARG_gene_name<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/all_sample_bbnorm_default_coassembly/binning_blast/bin_50_10_ARGblast/SARG_Struturelist_adjust.xlsx")
colnames(ARG_gene_name)[1]<-"type"
output2<- vector(mode = "list")
for(i in 1:length(filenames)){
  df <-try(read.table(filenames[i],sep="\t",header=FALSE))
  if ('try-error' %in% class(df )) {
    next}
  else{
    colnames(df)<-c("qseqid", "gene", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
    df_blast_out_reannonate<-merge(df,ARG_gene_name,all.x = T,by="gene")
    df_blast_out_reannonate<-df_blast_out_reannonate%>%
      separate(type,into=c("Type","Subtype"),sep="__")
    output2[[i]]<-df_blast_out_reannonate
    names(output2)[[i]]<-gsub(".*/","",filenames[[i]])%>%
      gsub(".SARG.dmnd","",.)
  }
}

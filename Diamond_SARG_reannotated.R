library("openxlsx")
library("tidyverse")
x1<-read.xlsx(xlsxFile = "C://Users/USER/Desktop/Practicedate_Blastx_SARG2.2_output.xlsx",sheet=2,colNames = T,sep="_")
x1$Corresponding_ids<-gsub(pattern='\\[',replacement="",x1$Corresponding_ids)
x1$Corresponding_ids<-gsub(pattern='\\]',replacement="",x1$Corresponding_ids)
x1$Corresponding_ids<-gsub(pattern="\\'",replacement="",x1$Corresponding_ids)
x1$Corresponding_ids<-gsub(pattern=" ",replacement="",x1$Corresponding_ids)
c<-separate(x1,Corresponding_ids,into = paste0("COL1_", 1:1000),sep=",")
cc1<-c%>%
  gather(paste0("COL1_", 1:1000),key="fakename",value="gene",na.rm=T)
cc1$fakename<-NULL
write.xlsx(cc1,"c:/Users/USER/Desktop/SARGdatabase.xlsx",colnames=T)
Diamond_SARG_hit<-read.xlsx(xlsxFile = "C://Users/USER/Desktop/Practicedate_Blastx_SARG2.2_output.xlsx",sheet=1,colNames = F,sep="_")
colnames(Diamond_SARG_hit)<-c("qseqid", "gene", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore")
Diamond_SARG_hit_annoate<-merge(Diamond_SARG_hit,cc1,all.x = T)
Diamond_SARG_hit_annoate<-separate(Diamond_SARG_hit_annoate,Categories_in_database,into=c("type","subtype"),sep="__")
write.xlsx(Diamond_SARG_hit_annoate,"c:/Users/USER/Desktop/Diamond_SARG_annotate.xlsx",colnames=T)
 

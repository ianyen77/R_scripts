library(pheatmap)
library(openxlsx)
library(tidyverse)
library(pheatmap)
annonate<-function(type){
  #choose blast directory 
  if(type == "ARG"){
file_list<-list.files("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/all_sample_bbnorm_default_coassembly/binning_blast/bin_50_10_ARGblast/",full.names = TRUE)
  
  }else if(type == "MGE"){
    file_list<-list.files("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/all_sample_bbnorm_default_coassembly/binning_blast/bin_50_10_MGEblast/",full.names = TRUE)
  
  }else if(type == "VF"){
    file_list<-list.files("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/all_sample_bbnorm_default_coassembly/binning_blast/bin_50_10_VFblast/",full.names = TRUE)
    }
df<-data.frame()  # 先定義一個空的資料框
for (i in seq_along(file_list)) {
  if (file.size((file_list[i]))==0) {
    next
    
    } else {
    # 讀取後續檔案，跳過第一列
      x <- read.table(file_list[i])
      colnames(x) <- colnames(x)  # 將欄位名稱修改為原始值
      if(type == "ARG"){
        xx<-gsub(".*/","",file_list[i])%>%
          gsub(".SARG.dmnd","",.)
      }else if(type == "MGE"){
        xx<-gsub(".*/","",file_list[i])%>%
          gsub(".MGEs.dmnd","",.)
      }else if(type == "VF"){
        xx<-gsub(".*/","",file_list[i])%>%
          gsub(".VF.dmnd","",.)
      }
      x$V13=rep(xx,each=as.integer(rownames(read.table(file_list[i]))))
    } 
  df <- rbind(df, x)  # 將讀取到的資料框合併至 df 中
}
colnames(df)<-c("qseqid", "gene", "pident", "length", "mismatch", "gapopen", "qstart", "qend", "sstart", "send", "evalue", "bitscore","bin")
  if(type == "ARG"){
  ARG_DB<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARG_DB/SARG_Struturelist_adjust.xlsx")
  colnames(ARG_DB)<-c("gene_name","gene")
  df_annonate_ARG<<-merge(df,ARG_DB,by="gene",all.x=T) %>% 
    separate(gene_name,into=c("type","subtype"),sep="__")
  }else if(type == "MGE"){
  ARG_DB<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/MGE/MGE namefile.xlsx",colNames = F)
  colnames(ARG_DB)<-c("gene","gene_type","gene_subtype")
  df$gene<-gsub("_1","",df$gene)
  df_annonate_MGE<<-merge(df,ARG_DB,by="gene",all.x=T)
  }else if(type == "VF"){
  ARG_DB<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/VF/VFDB/VFDB_gene_name.xlsx")
  df_annonate_VF<<-merge(df,ARG_DB,by="gene",all.x=T)
  }
}
annonate("ARG")

write.xlsx(df_annonate_VF,"C:/Users/USER/Desktop/VF_bin_annonate.xlsx")
write.xlsx(df_annonate_ARG,"C:/Users/USER/Desktop/ARG_bin_annonate.xlsx")
write.xlsx(df_annonate_MGE,"C:/Users/USER/Desktop/MGE_bin_annonate.xlsx")

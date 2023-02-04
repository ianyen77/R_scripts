library(ggplot2)
library(openxlsx)
library(ggfortify)
library(hrbrthemes)
library(tidyverse)
library(RColorBrewer)
#針對mapoutput進行一些預處理，並把它變成galaxy指定的格式
dbpata<-read.xlsx("C:/Users/USER/Desktop/kraken2 mpaout for lefse.xlsx",sheet=3,rowNames=F,colNames=T,sep.names=" ")
rownames(dbpata)<-dbpata$`#Classification`
dbpata$`#Classification`<-NULL
dbpata [ dbpata < 0.0001 ] <- 0
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
tax<-rownames(dbpata)
dbpata<-cbind(tax,dbpata)
dbpata
dbpata$tax<-gsub(pattern = "k__",replacement = "",x=dbpata$tax)
dbpata$tax<-gsub(pattern = "p__",replacement = "",x=dbpata$tax)
dbpata$tax<-gsub(pattern = "c__",replacement = "",x=dbpata$tax)
dbpata$tax<-gsub(pattern = "o__",replacement = "",x=dbpata$tax)
dbpata$tax<-gsub(pattern = "f__",replacement = "",x=dbpata$tax)
dbpata$tax<-gsub(pattern = "g__",replacement = "",x=dbpata$tax)
dbpata$tax<-gsub(pattern = "s__",replacement = "",x=dbpata$tax)
id<-colnames(dbpata)
id[1]<-"id"
dbpata<-rbind(id,dbpata)
Location<-c("Location","Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
dbpata<-rbind(Location,dbpata)
rownames(dbpata)<-NULL
write.xlsx(dbpata,"C:/Users/USER/Desktop/taxon.lefse.xlsx")
str(dbpata)
#以上是lefse 的preprocress，之後須到galaxy上跑，跑完再載下來畫圖

#載入分析結果，繪圖(多組group,雙組可再參考學長的腳本)
gal_lda <- read.xlsx("C:/Users/USER/Desktop/活頁簿1.xlsx",sheet=1,colNames=F)
colnames(gal_lda) <-  c("TAX","LogMaxMean","Location","LDA","pValue","subtypenum")
gal_lda<-gal_lda%>%
  separate(TAX,into=c("type","subtype"),sep ="__")
#gsub(pattern = "Bacteria_",replacement = "")
#gal_lda_s$TAX <- str_replace(gal_lda_s$TAX,".{1,}s__","")
gal_lda$Location <- factor(gal_lda$Location, levels = c("Downstream","Midstream","Upstream","Finished","Raw"))
gal_lda<-gal_lda[-(which(is.na(gal_lda$LDA))),]
gal_lda%>%
  group_by(Location)%>% 
  mutate(subtype= fct_reorder(subtype, LDA,.desc=F))%>%
  ggplot()+geom_col(aes(x=LDA, y = subtype,fill = fct_rev(Location)))+
  scale_fill_discrete(name = "Location")+scale_fill_brewer("Location",palette = "Set3")+
  guides(fill = guide_legend(label.hjust = 0))+labs(x=expression(LDA~Score~(log[10])),y="")+theme_bw()+theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
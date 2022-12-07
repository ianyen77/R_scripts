library(ggplot2)
library(openxlsx)
library(ggfortify)
library(hrbrthemes)
library(tidyverse)
dbpata<-read.xlsx("C:/Users/USER/Desktop/枯水_ampliccon sequencing data analysis/raw_data_xlsx/genus_raw.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/枯水_ampliccon sequencing data analysis/group.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/枯水_ampliccon sequencing data analysis/env.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata$species<-rownames(dbpata)
dbpata<-cbind(dbpata,dbpata)
dbpata<-dbpata[,-(1:15)]
dbpata<-dbpata[,-17]
dbpata<-separate(dbpata,species,into=c("D","P","C","O","F","G"),sep=";")
#計算各個階層的加總
Domain<-aggregate(dbpata[,7:21],list(dbpata$D),sum) 
Domain[c("D","P","C","O","F","G")]<-NA
Phyla<-aggregate(dbpata[,7:21],list(dbpata$D,dbpata$P),sum)
Phyla[c("D","P","C","O","F","G")]<-NA
Class<-aggregate(dbpata[,7:21],list(dbpata$D,dbpata$P,dbpata$C),sum)
Class[c("D","P","C","O","F","G")]<-NA
Order<-aggregate(dbpata[,7:21],list(dbpata$D,dbpata$P,dbpata$C,dbpata$O),sum)
Order[c("D","P","C","O","F","G")]<-NA
Families<-aggregate(dbpata[,7:21],list(dbpata$D,dbpata$P,dbpata$C,dbpata$O,dbpata$`F`),sum)
Families[c("D","P","C","O","F","G")]<-NA
Genus<-aggregate(dbpata[,7:21],list(dbpata$D,dbpata$P,dbpata$C,dbpata$O,dbpata$`F`,dbpata$G),sum)
Genus[c("D","P","C","O","F","G")]<-NA
Domain$D<-Domain$Group.1
Domain$Group.1<-NULL
Domain<-cbind(Domain,Domain[,-(16:21)])
Domain<-Domain[,-(1:15)]

Phyla$D<-Phyla$Group.1
Phyla$P<-Phyla$Group.2
Phyla$Group.1<-NULL
Phyla$Group.2<-NULL
Phyla<-cbind(Phyla,Phyla[,-(16:21)])
Phyla<-Phyla[,-(1:15)]

Class$D<-Class$Group.1
Class$P<-Class$Group.2
Class$C<-Class$Group.3
Class$Group.1<-NULL
Class$Group.2<-NULL
Class$Group.3<-NULL
Class<-cbind(Class,Class[,-(16:21)])
Class<-Class[,-(1:15)]

Order$D<-Order$Group.1
Order$P<-Order$Group.2
Order$C<-Order$Group.3
Order$O<-Order$Group.4
Order$Group.1<-NULL
Order$Group.2<-NULL
Order$Group.3<-NULL
Order$Group.4<-NULL
Order<-cbind(Order,Order[,-(16:21)])
Order<-Order[,-(1:15)]

Families$D<-Families$Group.1
Families$P<-Families$Group.2
Families$C<-Families$Group.3
Families$O<-Families$Group.4
Families$`F`<-Families$Group.5
Families$Group.1<-NULL
Families$Group.2<-NULL
Families$Group.3<-NULL
Families$Group.4<-NULL
Families$Group.5<-NULL
Families<-cbind(Families,Families[,-(16:21)])
Families<-Families[,-(1:15)]

Genus$D<-Genus$Group.1
Genus$P<-Genus$Group.2
Genus$C<-Genus$Group.3
Genus$O<-Genus$Group.4
Genus$`F`<-Genus$Group.5
Genus$G<-Genus$Group.6
Genus$Group.1<-NULL
Genus$Group.2<-NULL
Genus$Group.3<-NULL
Genus$Group.4<-NULL
Genus$Group.5<-NULL
Genus$Group.6<-NULL
Genus<-cbind(Genus,Genus[,-(16:21)])
Genus<-Genus[,-(1:15)]
#將NA排列在前面，這個部分作的不是很好，導致最高的分類會被放在最下面，需要先輸出到excel在處理(需要整個顛倒，可加一列1-x，並且由大到小排列)
mpa_format_genus<-rbind(Domain,Phyla,Class,Order,Families,Genus)
mpa_format_genus1<-arrange(mpa_format_genus,desc(is.na(D)),desc(D))
mpa_format_genus2<-arrange(mpa_format_genus1,D,P)
mpa_format_genus3<-arrange(mpa_format_genus2,D,P,C)
mpa_format_genus4<-arrange(mpa_format_genus3,D,P,C,O)
mpa_format_genus5<-arrange(mpa_format_genus4,D,P,C,O,`F`)
mpa_format_genus6<-arrange(mpa_format_genus5,D,P,C,O,`F`,G)
write.xlsx(mpa_format_genus6,file="c:/Users/USER/Desktop/mpa_format_genus.xlsx",keepNA=T)
#讀處理完之excel
lefse_prep_DATA<-read.xlsx("C:/Users/USER/Desktop/mpa_format_genus.xlsx",,sheet=1,rowNames=F,colNames=T,sep.names=" ")
lefse_prep_DATA1<-unite(data=lefse_prep_DATA,col =x,D,P,C,O,`F`,G,sep = "|")
lefse_prep_DATA1$x<-gsub(pattern='\\|NA',replacement="",lefse_prep_DATA1$x)
lefse_prep_DATA_final<-arrange(lefse_prep_DATA1,x)
write.xlsx(lefse_prep_DATA_final,file = "c:/Users/USER/Desktop/v3-v4_mpa format.xlsx",rowNames=T)
#輸出之Excel要在手動分組、title，並以數值重新儲存為txt後才可以上傳galaxy分析


#載入分析結果，繪圖(多組group,雙組可再參考學長的腳本)
gal_lda <- read.xlsx("C:/Users/USER/Desktop/lefse/NGS_Lefse output.xlsx",sheet=2,rowNames=F,colNames=F,sep.names=" ")
colnames(gal_lda) <-  c("TAX","LogMaxMean","Type","LDA","pValue")
gal_lda$TAX<- gal_lda$TAX %>%
  gsub(pattern="\\.",replacement="_")
  #gsub(pattern = "Bacteria_",replacement = "")
#gal_lda_s$TAX <- str_replace(gal_lda_s$TAX,".{1,}s__","")
gal_lda$Type <- factor(gal_lda$Type, levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
gal_lda<-gal_lda[-(which(is.na(gal_lda$LDA))),]
gal_lda%>%
  group_by(Type)%>% 
  mutate(TAX = fct_reorder(TAX, LDA,.desc=F))%>%
  ggplot(aes(x=LDA, y = TAX,fill = Type))+geom_col()+
  scale_fill_discrete(name = "Sample")+
  guides(fill = guide_legend(label.hjust = 0))+labs(x=expression(LDA~Score~(log[10])),y="")+theme_bw()

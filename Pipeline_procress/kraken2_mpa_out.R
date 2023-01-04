#此腳本主要為銜接在kraken2_mpa.pl腳本後去處理轉換成mpaformat的檔案
library("openxlsx")
library("tidyverse")
library("ggplot2")
#記得輸入前要先把mpa的樣品名稱改掉
taxon_mpa_format<-read.xlsx(xlsxFile = "C://Users/USER/Desktop/kraken2_mpa_combine.xlsx",sheet=1,colNames = T)
taxon_mpa_format<-taxon_mpa_format%>%
  filter(str_detect(`#Classification`,pattern="k__Bacteria"))

taxon_mpa1<-taxon_mpa_format%>%
  separate(`#Classification`,sep="\\|",into=c("Domain","Phyla","Class","Order","Family","Genus","Species"))

write.xlsx(taxon_mpa1,"C://Users/USER/Desktop/taxon_adjust_mpa.xlsx")
#在這之後我們需要去excel手動調整將正確的分類放在正確的column
#所有的column下面的分類都要跟column有對應上

taxon_mpa<-read.xlsx(xlsxFile = "C:/Users/TUNG'S LAB/Desktop/taxon_mpa.xlsx",sheet=1,colNames = T)

#Phyla relative abundance table
phyla_table<-taxon_mpa%>% 
  filter(is.na(Class)&is.na(Order)&is.na(Family)&is.na(Genus)&is.na(Species))%>%
  filter(!is.na(Phyla))
phyla_table[,8:9]<-apply(phyla_table[,8:9],2,function(x) x/sum(x))
write.xlsx(phyla_table,"C:/Users/TUNG'S LAB/Desktop/phyla.xlsx")

#class relative abundance table
class_table<-taxon_mpa%>%  
  filter(is.na(Order)&is.na(Family)&is.na(Genus)&is.na(Species))%>%
  filter(!is.na(Class))
class_table[,8:9]<-apply(class_table[,8:9],2,function(x) x/sum(x))
write.xlsx(class_table,"C:/Users/TUNG'S LAB/Desktop/class.xlsx")

#order relative abundance table
order_table<-taxon_mpa%>%  
  filter(is.na(Family)&is.na(Genus)&is.na(Species))%>%
  filter(!is.na(Order))
order_table[,8:9]<-apply(order_table[,8:9],2,function(x) x/sum(x))
write.xlsx(order_table,"C:/Users/TUNG'S LAB/Desktop/order.xlsx")

#family relative abundance table
fmaily_table<-taxon_mpa%>%  
  filter(is.na(Genus)&is.na(Species))%>%
  filter(!is.na(Family))
order_table[,8:9]<-apply(order_table[,8:9],2,function(x) x/sum(x))
write.xlsx(order_table,"C:/Users/TUNG'S LAB/Desktop/family.xlsx")

#genus relative abundance table
genus_table<-taxon_mpa%>%  
  filter(is.na(Species))%>%
  filter(!is.na(Genus))
genus_table[,8:9]<-apply(genus_table[,8:9],2,function(x) x/sum(x))
write.xlsx(order_table,"C:/Users/TUNG'S LAB/Desktop/genus.xlsx")

#species relative abundance table
species_table<-taxon_mpa%>%  
  filter(!is.na(Species))
species_table[,8:9]<-apply(species_table[,8:9],2,function(x) x/sum(x))
write.xlsx(order_table,"C:/Users/TUNG'S LAB/Desktop/species.xlsx")



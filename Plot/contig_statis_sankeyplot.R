library(openxlsx)
library(tidyverse)
library(pheatmap)
library(RColorBrewer)
library(ggsankey)
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/ARC_phyla_整理.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"
#data_contig$type[data_contig$type == "fosmidomycin"]<-"Others"
#data_contig$type[data_contig$type == "kasugamycin"]<-"Others"
#data_contig$type[data_contig$type == "sulfonamide"]<-"Others"
#data_contig$type[data_contig$type == "macrolide-lincosamide-streptogramin"]<-"Others"
#data_contig$type[data_contig$type == "quinolone"]<-"Others"
#data_contig$type[data_contig$type == "fosfomycin"]<-"Others"
#data_contig$type[data_contig$type == "unclassified"]<-"Others"
data_contig$type<-str_to_title(data_contig$type)
data_contig$type[data_contig$type == "Beta-Lactam"]<-"Beta-lactam"
data_contig$type[data_contig$type == "Macrolide-Lincosamide-Streptogramin"]<-"MLS"
data_contig$contig_taxon.x[is.na(data_contig$contig_taxon.x)]<-"Unclassified"
data_contig<-data_contig[grep("Actinobacteria",data_contig$contig_phyla),]
data_stat<-data_contig%>%
  group_by(type,contig_phyla)%>%
  count(type)%>%
  mutate(type_ACC_percent=n/sum(n))
data_phyl_stat<-data_contig%>%
  count(contig_phyla)%>%
  mutate(phyla_ACC_percent=n/248)
data_stat1<-data_contig%>%
  select(contig_phyla,Sample,type,subtype,contig_taxon.x)
colnames(data_stat1)<-c('Phyla','Sample','ARG type ',"ARG subtype","ARC_taxon")
data_stat<-as.data.frame(data_stat1)
data_sankey<- data_stat1%>%
  make_long('Phyla','Sample','ARG type ',"ARG subtype","ARC_taxon")
data_sankey$node<-factor(data_sankey$node)
#我們算一下各節點的數量
reagg <- data_sankey%>%
  dplyr::group_by(node)%>%
  tally()
df2 <- merge(data_sankey,
             reagg, 
             by.x = 'node', 
             by.y = 'node', 
             all.x = TRUE)
pl <- ggplot(df2, aes(x = x
                     , next_x = next_x
                     , node = node
                     , next_node = next_node
                     , fill =node
                     ,label = paste0(node, " = ", n))
)
pl <- pl +geom_sankey(flow.alpha = 0.5
                      , node.color = NA
                      ,show.legend = F)
pl <- pl +geom_sankey_text(size =5, color = "black", fill = NA, hjust = 0, 
                           position = position_nudge(x = 0.08))
pl <- pl +  theme_alluvial()
pl <- pl + theme(legend.position = "none")
#pl <- pl +  theme(axis.title = element_blank()
                 # , axis.text.y = element_blank()
                  #, axis.ticks = element_blank()  
                 # , panel.grid = element_blank())
cols <- hcl.colors(31, "Sunset")
pl <- pl + scale_fill_manual(values=cols)
pl


#Firmicute saneky--------------------------
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/ARC_phyla_整理.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"
#data_contig$type[data_contig$type == "fosmidomycin"]<-"Others"
#data_contig$type[data_contig$type == "kasugamycin"]<-"Others"
#data_contig$type[data_contig$type == "sulfonamide"]<-"Others"
#data_contig$type[data_contig$type == "macrolide-lincosamide-streptogramin"]<-"Others"
#data_contig$type[data_contig$type == "quinolone"]<-"Others"
#data_contig$type[data_contig$type == "fosfomycin"]<-"Others"
#data_contig$type[data_contig$type == "unclassified"]<-"Others"
data_contig$type<-str_to_title(data_contig$type)
data_contig$type[data_contig$type == "Beta-Lactam"]<-"Beta-lactam"
data_contig$type[data_contig$type == "Macrolide-Lincosamide-Streptogramin"]<-"MLS"
data_contig$contig_taxon.x[is.na(data_contig$contig_taxon.x)]<-"Unclassified"
data_contig$contig_taxon.x[data_contig$contig_taxon.x=="unclassified Clostridium"]<-"Clostridium"
data_contig$contig_taxon.x[data_contig$contig_taxon.x=="unclassified Fictibacillus"]<-"Fictibacillus"
data_contig<-data_contig[grep("Firmicutes",data_contig$contig_phyla),]
data_stat<-data_contig%>%
  group_by(type,contig_phyla)%>%
  count(type)%>%
  mutate(type_ACC_percent=n/sum(n))
data_phyl_stat<-data_contig%>%
  count(contig_phyla)%>%
  mutate(phyla_ACC_percent=n/248)
data_stat1<-data_contig%>%
  select(contig_phyla,Sample,type,subtype,contig_taxon.x)
colnames(data_stat1)<-c('Phyla','Sample','ARG type ',"ARG subtype","ARC_taxon")
data_stat<-as.data.frame(data_stat1)
data_sankey<- data_stat1%>%
  make_long('Phyla','Sample','ARG type ',"ARG subtype","ARC_taxon")
data_sankey$node<-factor(data_sankey$node)
#我們算一下各節點的數量
reagg <- data_sankey%>%
  dplyr::group_by(node)%>%
  tally()
df2 <- merge(data_sankey,
             reagg, 
             by.x = 'node', 
             by.y = 'node', 
             all.x = TRUE)
pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill =node
                      ,label = paste0(node, " = ", n))
)
pl <- pl +geom_sankey(flow.alpha = 0.5
                      , node.color = NA
                      ,show.legend = F)
pl <- pl +geom_sankey_text(size =3, color = "black", fill = NA, hjust = 0, 
                           position = position_nudge(x = 0.08))
pl <- pl +  theme_alluvial()
pl <- pl + theme(legend.position = "none")
#pl <- pl +  theme(axis.title = element_blank()
# , axis.text.y = element_blank()
#, axis.ticks = element_blank()  
# , panel.grid = element_blank())
cols <- hcl.colors(41, "Sunset")
pl <- pl + scale_fill_manual(values=cols)
pl

##Actinobacteria Firmicute -----------------------------
data_contig<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/ARC_phyla_整理.xlsx",sheet=1)
data_contig$contig_phyla[is.na(data_contig$contig_phyla)]<-"Unclassified"
data_contig$contig_phyla[data_contig$contig_phyla == "Bacteria"]<-"Unclassified"
#data_contig$type[data_contig$type == "fosmidomycin"]<-"Others"
#data_contig$type[data_contig$type == "kasugamycin"]<-"Others"
#data_contig$type[data_contig$type == "sulfonamide"]<-"Others"
#data_contig$type[data_contig$type == "macrolide-lincosamide-streptogramin"]<-"Others"
#data_contig$type[data_contig$type == "quinolone"]<-"Others"
#data_contig$type[data_contig$type == "fosfomycin"]<-"Others"
#data_contig$type[data_contig$type == "unclassified"]<-"Others"
data_contig$type<-str_to_title(data_contig$type)
data_contig$type[data_contig$type == "Beta-Lactam"]<-"Beta-lactam"
data_contig$type[data_contig$type == "Macrolide-Lincosamide-Streptogramin"]<-"MLS"
data_contig$contig_taxon.x[is.na(data_contig$contig_taxon.x)]<-"Unclassified"
data_contig$contig_taxon.x[data_contig$contig_taxon.x=="unclassified Clostridium"]<-"Clostridium"
data_contig$contig_taxon.x[data_contig$contig_taxon.x=="unclassified Fictibacillus"]<-"Fictibacillus"
data_contig1<-data_contig[grep("Firmicutes",data_contig$contig_phyla),]
data_contig<-rbind(data_contig1,data_contig[grep("Actinobacteria",data_contig$contig_phyla),])
data_stat<-data_contig%>%
  group_by(type,contig_phyla)%>%
  count(type)%>%
  mutate(type_ACC_percent=n/sum(n))
data_phyl_stat<-data_contig%>%
  count(contig_phyla)%>%
  mutate(phyla_ACC_percent=n/248)
data_stat1<-data_contig%>%
  select(contig_phyla,Sample,type,subtype,contig_taxon.x)
colnames(data_stat1)<-c('Phyla','Sample','ARG type ',"ARG subtype","ARC_taxon")
data_stat<-as.data.frame(data_stat1)
data_sankey<- data_stat1%>%
  make_long('Phyla','Sample','ARG type ',"ARG subtype","ARC_taxon")
data_sankey$node<-factor(data_sankey$node)
#我們算一下各節點的數量
reagg <- data_sankey%>%
  dplyr::group_by(node)%>%
  tally()
df2 <- merge(data_sankey,
             reagg, 
             by.x = 'node', 
             by.y = 'node', 
             all.x = TRUE)
pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill =node
                      ,label = paste0(node, " = ", n))
)
pl <- pl +geom_sankey(flow.alpha = 0.5
                      , node.color = NA
                      ,show.legend = F)
pl <- pl +geom_sankey_text(size =3, color = "black", fill = NA, hjust = 0, 
                           position = position_nudge(x = 0.08))
pl <- pl +  theme_alluvial()
pl <- pl + theme(legend.position = "none")
#pl <- pl +  theme(axis.title = element_blank()
# , axis.text.y = element_blank()
#, axis.ticks = element_blank()  
# , panel.grid = element_blank())
cols <- hcl.colors(59, "Sunset")
pl <- pl + scale_fill_manual(values=cols)
pl



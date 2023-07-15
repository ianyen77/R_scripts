library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(tidyverse)
library(ggsankey)
#kraken2_out<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/SARG_V3.2/kraken classification/ARC_classification_kraken2_confidence0.xlsx")
#ARCmegan_out<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/SARG_V3.2/MEGAN/SARGv3.2_ARC_analysis.xlsx")
#c<-merge(ARCmegan_out,kraken2_out,by="contig")
#write.xlsx(c,"C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/SARG_V3.2/kraken classification/ARC_classification_kraken2_merge.xlsx")
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/SARG_V3.2/kraken classification/ARC_classification_kraken2_merge.xlsx")
data$p[is.na(data$p)]<-"Unclassified"
data$c[is.na(data$c)]<-"Unclassified"
data$o[is.na(data$o)]<-"Unclassified"
data$f[is.na(data$f)]<-"Unclassified"
data$g[is.na(data$g)]<-"Unclassified"
data$s[is.na(data$s)]<-"Unclassified"
data$Type<-str_to_sentence(data$Type)
#simplified the plot
#change name of MLs
data$Type[data$Type == "Macrolide-lincosamide-streptogramin"]<-"MLS"
#simplified the ARG TYPE
#data$Type[data$Type == "fosmidomycin"]<-"Others"
data$Type[data$Type == "Kasugamycin"]<-"Others"
data$Type[data$Type == "Sulfonamide"]<-"Others"
data$Type[data$Type == "Novobiocin"]<-"Others"
data$Type[data$Type == "Mupirocin"]<-"Others"
data$Type[data$Type == "Fosfomycin"]<-"Others"
data$Type[data$Type == "Chloramphenicol"]<-"Others"
data$Type[data$Type == "Streptothricin"]<-"Others"
#Simplified phyla levle
data$p[data$p=="Thermodesulfobacteriota"]<-"Others"
data$p[data$p=="Fusobacteriota"]<-"Others"
data$p[data$p=="Cyanobacteriota"]<-"Others"
data$p[data$p=="Planctomycetota"]<-"Others"
data$p[data$p=="Verrucomicrobiota"]<-"Others"


data_sta<-data %>% 
  group_by(sample.x,Type,p) %>% 
  count()
data_stat1<-data%>%
  select(sample.x,p,Type)
colnames(data_stat1)<-c('Sample','ARC_Phlya','ARG type')
data_stat<-as.data.frame(data_stat1)
data_sankey<- data_stat1%>%
  make_long('Sample','ARC_Phlya','ARG type')
#make sakeyplot
data_sankey$node<-factor(data_sankey$node)
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
cols <- hcl.colors(21, "Sunset")
pl <- pl + scale_fill_manual(values=cols)
pl


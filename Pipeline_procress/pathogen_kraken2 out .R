library(openxlsx)
library(tidyverse)
pathogen_genus<-read.xlsx("c:/Users/USER/Desktop/pathogen list.xlsx",sheet=2,colNames = T)
pathogen_species<-read.xlsx("c:/Users/USER/Desktop/pathogen list.xlsx",sheet=1,colNames = T)
data_species<-read.xlsx("c:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/rel_abundance/species_rel_table.xlsx",sheet=1,colNames = T)
data_genus<-read.xlsx("c:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/rel_abundance/genus_rel_table.xlsx",sheet=1,colNames = T)
#transform species data
data_species<-data_species[,-(1:6)]
data_species<-data_species%>%
  separate(Species,into = c("g","Species"),sep = "__",extra = "merge")%>%
  separate(Species,into=c("Genus","Species"),sep="_",extra = "merge")%>%
  unite("Species",Genus,Species,sep = " ")
data_species$g<-NULL
#transform species data
data_genus<-data_genus[,-(1:5)]
data_genus$Species<-NULL
data_genus<-data_genus%>%
  separate(Genus,into = c("g","Genus"),sep = "__",extra = "merge")
data_genus$g<-NULL

pathogen_insam<-merge(data_species,pathogen_species)%>%
  write.xlsx("C:/Users/USER/Desktop/pathogen_species.xlsx")
pathogen_insamgenus<-merge(data_genus,pathogen_genus)%>%
  write.xlsx("C:/Users/USER/Desktop/pathogen_genus.xlsx")

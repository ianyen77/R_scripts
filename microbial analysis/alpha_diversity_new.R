library(openxlsx)
library(tidyverse)
library(vegan)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/species_rel_table.xlsx",sheet=1,rowNames=F)
rownames(data)<-data$Species
data<-data[,-(1:7)]
data<-as.data.frame(t(data))
#data<-gather(data = datakey="sample",value="abundance",2:16)
richness<-as.data.frame(apply(data,1,function(x) sum(x>0)))
shannon<-diversity(data,index="shannon")
simpson<-1-(diversity(data,index="simpson"))
invsimpsion<-diversity(data,index = "invsimpson")
alphadiversity<-cbind(richness,shannon,simpson,invsimpsion)
colnames(alphadiversity)[colnames(alphadiversity)=="apply(data, 1, function(x) sum(x > 0))"]<-"richness"
alphadiversity$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
alphadiversity$location<-factor(alphadiversity$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
alphadiversity$sample<-rownames(alphadiversity)
write.xlsx(alphadiversity,"C:/Users/USER/Desktop/alphadiversity")
alphadiversity_plotdata<-pivot_longer(alphadiversity,cols = c(richness,shannon,simpson,invsimpsion),names_to = "index")

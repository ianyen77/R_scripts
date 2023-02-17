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
simpson<-(diversity(data,index="simpson"))
invsimpson<-diversity(data,index = "invsimpson")
alphadiversity<-cbind(richness,shannon,simpson,invsimpson)
colnames(alphadiversity)[colnames(alphadiversity)=="apply(data, 1, function(x) sum(x > 0))"]<-"richness"
alphadiversity$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
alphadiversity$location<-factor(alphadiversity$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
alphadiversity$sample<-rownames(alphadiversity)
write.xlsx(alphadiversity,"C:/Users/USER/Desktop/alphadiversity")
#poltdata transformation
alphadiversity_plotdata<-pivot_longer(alphadiversity,cols = c(richness,shannon,simpson,invsimpson),names_to = "index")
alphadiversity_plotdata$index<-factor(alphadiversity_plotdata$index,levels = c("richness","shannon","simpson","invsimpson"))
alphadiversity_plotdata_bar<-alphadiversity_plotdata%>%
  group_by(location,index)%>%
  summarise(mean=mean(value),std=sd(value))
color<-c("#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69")
#boxplot
ggplot(alphadiversity_plotdata,aes(x=location,y=value,fill=location))+geom_boxplot(alpha=0.7,width = 0.8)+geom_point()+facet_wrap(~index,nrow=2,scales = "free")+theme_bw()+
  scale_fill_manual("Location",values = color)+theme(strip.text = element_text(size = 13))
#barplot
ggplot(alphadiversity_plotdata_bar,aes(x=location,y=mean,fill=location))+geom_bar(alpha=0.7,stat = "identity",width = 0.8)+facet_wrap(~index,nrow = 2,scales = "free")+theme_bw()+
  scale_fill_manual("Location",values = color)+geom_errorbar(aes(x=location,ymin=mean-std, ymax=mean+std), width=.1,position=position_dodge(.9))

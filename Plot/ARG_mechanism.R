library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(tidyverse)
argdata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet = 1)
arg_mech<-read.xlsx("C:/Users/USER/Desktop/ARG_mechanism_1.xlsx",sheet = 2)
mix_data<-merge(argdata,arg_mech,by="ARGs.abundance.normalization.aganist.16S")
mix_data<-mix_data[apply(mix_data[2:16], 1, function(x) !all(x==0)),]
plotdata<-mix_data%>%
  gather(key="sample",value="amount",2:16)%>%
  group_by(Mechanism_group,sample)%>%
  summarise_at(vars(amount), funs(sum)) %>% 
  group_by(sample) %>%
  mutate(percentage=amount/sum(amount))
  
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=amount,fill=Mechanism_group),alpha=0.7,stat="identity",width=0.8)+scale_fill_brewer(palette = "Set3")+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization against 16S")
plotdata1<-plotdata %>% 
  separate(sample,into=c("location","name","rep"),remove=F)%>%
  group_by(Mechanism_group,location) %>% 
  summarise_at(vars(amount), funs(mean,sd))
ggplot(plotdata1,aes(x=location,y=mean,ymin=mean-sd, ymax=mean+sd,fill=`Mechanism_group`))+
  geom_bar(aes(y=mean,color=Mechanism_group),position="stack",stat="identity",alpha=0.7,width = 0.9)+geom_errorbar(width=0.7)+theme_bw()+
  labs(x=NULL,y=NULL)+scale_fill_brewer(palette = "Set3")+scale_color_brewer(palette = "Set3")+theme(axis.title = element_text(size=13),axis.text = element_text(size=13),legend.title= element_text(size=12),legend.text = element_text(size=12))+xlab("Sample")+ylab("ARGs abundance normalization against 16S")

color<-hcl.colors(9,"sunset")

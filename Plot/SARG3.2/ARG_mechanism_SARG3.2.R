library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(tidyverse)
argdata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet = 1)
arg_mech<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/1.SARG_v3.2_20220917_Full_structure.xlsx",sheet = )
colnames(argdata)[1]<-"Subtype"
arg_mech<-arg_mech[!duplicated(arg_mech$Subtype),]
arg_mech<-arg_mech %>% 
  select(Subtype,HMM.category,Mechanism.group,Mechanism.subgroup,Mechanism.subgroup2)
mix_data<-merge(argdata,arg_mech,by="Subtype",all.x=T)
#mix_data<-mix_data[apply(mix_data[2:16], 1, function(x) !all(x==0)),]
mix_data$Mechanism.group[is.na(mix_data$Mechanism.group)]<-"Unknow"
mix_data$Mechanism.subgroup[is.na(mix_data$Mechanism.subgroup)]<-"Unknow"
plotdata<-mix_data%>%
  gather(key="sample",value="amount",2:16)%>%
  group_by(Mechanism.group,sample)%>%
  summarise_at(vars(amount), funs(sum)) %>% 
  group_by(sample) %>%
  mutate(percentage=amount/sum(amount))
sset3<-brewer.pal(12,"Set3")
color<-c( "#8DD3C7", "#FFFFB3" ,"#FB8072","#BEBADA" , "#80B1D3" , "#FCCDE5" ,"#CCEBC5" ,"#BC80BD","#D9D9D9"  ,"#FFED6F")
#ARG_mech relative
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=percentage,fill=Mechanism.group),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_manual("ARG Mechanism",values =color )+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization against 16S rDNA")+
  theme(axis.title = element_text(size=12.5),legend.title= element_text(size=12.5),legend.text = element_text(size=12.5))
#ARG_mech quantify
plotdata1<-mix_data%>%
  gather(key="sample",value="amount",2:16)%>%
  group_by(Mechanism.group,sample)%>%
  summarise_at(vars(amount), funs(sum)) %>% 
  group_by(sample)
ggplot(plotdata1)+
  geom_bar(aes(x=sample,y=amount,fill=Mechanism.group,),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_manual("ARG Mechanism",values =color )+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization against 16S rDNA")+
  theme(axis.title = element_text(size=12.5),legend.title= element_text(size=12.5),legend.text = element_text(size=12.5))
#Export 11.45, 6.43
#ARG_submech---------------
plotdata1<-mix_data%>%
  gather(key="sample",value="amount",2:16)%>%
  group_by(Mechanism.subgroup,sample)%>%
  summarise_at(vars(amount), funs(sum)) %>% 
  group_by(sample) %>%
  mutate(percentage=amount/sum(amount))
plotdata1$Mechanism.subgroup[plotdata1$percentage<= 0.05]<-"Others"
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(41)
plotdata1$Mechanism.subgroup<-factor(plotdata1$Mechanism.subgroup,levels = c(unique(plotdata1$Mechanism.subgroup)[unique(plotdata1$Mechanism.subgroup)!="Others"],"Others"))
ggplot(plotdata1)+
  geom_bar(aes(x=sample,y=percentage,fill=Mechanism.subgroup),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_brewer(palette ="Set3")+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization against 16S rDNA")+
  theme(axis.title = element_text(size=12.5),legend.title= element_text(size=12.5),legend.text = element_text(size=12.5))

#ARG_mech location
plotdata2<-plotdata %>% 
  separate(sample,into=c("location","name","rep"),remove=F)%>%
  group_by(Mechanism.group,location) %>% 
  summarise_at(vars(amount), funs(mean,sd))
s_name<-c("T1","T2","T3","T4","T5")
L_name<-c("Raw","Finished","Upstream","Midstream","Downstream")
for (k in 1:length(s_name)){
  plotdata2$location[plotdata2$location ==s_name[k]]<-L_name[k]
}
plotdata2$location<-factor(plotdata2$location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))
ggplot(plotdata2,aes(x=location,y=mean,ymin=mean-sd, ymax=mean+sd,fill=`Mechanism.group`))+
  geom_bar(aes(y=mean,color=Mechanism.group),position="stack",stat="identity",alpha=0.7,width = 0.8)+theme_bw()+
  labs(x=NULL,y=NULL)+scale_fill_manual("ARG Mechanism",values=color)+scale_color_manual("ARG Mechanism",values = color)+
  theme(axis.title = element_text(size=13),axis.text = element_text(size=13),legend.title= element_text(size=12),legend.text = element_text(size=12))+xlab("Sample")+ylab("ARGs abundance normalization against 16S")

#Efflux pump
plotdata<-mix_data%>%
  gather(key="sample",value="amount",2:16)%>%
  filter(Mechanism_group=="Efflux pump") %>% 
  group_by(Mechanism_subgroup,sample)%>%
  summarise_at(vars(amount), funs(sum)) %>% 
  group_by(sample) %>%
  mutate(percentage=amount/sum(amount))
sset3<-brewer.pal(12,"Set3")
color<-c( "#8DD3C7", "#FFFFB3" ,"#BEBADA" ,"#FB8072", "#80B1D3" ,"#FDB462", "#FCCDE5" ,"#D9D9D9" ,"#BC80BD" ,"#CCEBC5" ,"#FFED6F")
#ARG_mech
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=amount,fill=Mechanism.subgroup),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_manual(values =color )+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization against 16S")

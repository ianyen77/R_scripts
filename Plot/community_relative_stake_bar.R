library("openxlsx")
library("ggplot2")
library("tidyverse")
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/family_rel_table.xlsx",sheet=1)
data<-data[,-(1:4)]
data<-data[,-(2:3)]
data$Family<-gsub(pattern = "f__",replacement = "",x=data$Family)
rownames(data)<-data$Family
data$Family<-NULL
data[data<0.05]<-0
data_sum<-apply(data,2,sum)
dd<-1-data_sum
data2<-rbind(data,dd)
rownames(data2)[nrow(data2)]<-"Others"
data3<-data2[apply(data2, 1, function(x) !all(x==0)),]
data3$Family<-rownames(data3)
#data3$phyla<-factor(data3$phyla,levels = c("Actinobacteria","Firmicutes","Planctomycetota","Proteobacteria","Others"))
rownames(data3)<-NULL
data3$Family<-factor(data3$Family,levels=c("Bacillaceae","Clostridiaceae","Comamonadaceae","Micromonosporaceae","Moraxellaceae","Mycobacteriaceae","Nitrobacteraceae","Nocardioidaceae","Rhodobacteraceae","Sphingomonadaceae","Xanthobacteraceae","Others"))
data4<-gather(data =data3, key ="Sample",value = "abundance",1:15)
data5<-data4[-which(data4$abundance==0),]
Sample<-colnames(data2)
Location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
group<-data.frame(cbind(Sample,Location))
group$Location<-factor(group$Location,levels=c('Raw', 'Finished', 'Upstream','Midstream','Downstream'))
#分組圖
data_stat<-merge(data5,group,by = "Sample",all.x = T)%>%
  group_by(Phyla,Location)%>%
  summarize(avg=mean(abundance),std=sd(abundance))%>%
  ggplot()+geom_bar(aes(x=Location,y=avg,fill=Family,color=Family,alpha=0.8,stat="identity"))+scale_fill_manual(values  = color)+scale_color_manual(values = color)+theme_bw()+
  labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),legend.title= element_text(size=12.5),legend.text = element_text(size=12.5))
data_stat

#each sample plot
ggplot(data5)+
  geom_bar(aes(x=Sample,y=abundance,fill=Family,color=Family),alpha=0.85,stat="identity")+
  scale_fill_brewer(palette = "Set3")+scale_color_brewer(palette = "Set3")+theme_bw()+
  labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),axis.text  = element_text(size=9),legend.title= element_text(size=12.5),legend.text = element_text(size=12))

color<-c("#8DD3C7","#FFFFB3","#BEBADA","#80B1D3","#FB8072")

display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")

library(openxlsx)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
data_taxa<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/genus_rel_table.xlsx",sheet=1,rowNames=F,colNames =T)
data_core<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/plot/venn/genus_venn_xlsx.xlsx")
data_core1<-data_core$`Raw|Finished|Upstream|Midstream|Downstream`
data_core1<-data_core1[!is.na(data_core1)]
data_core1<-as.data.frame(cbind(data_core1,rep(1,26)))
data_taxa<-data_taxa[,-(1:5)]
data_taxa<-data_taxa[,-2]
colnames(data_core1)<-c("Genus","core")
data_core<-merge(data_taxa,data_core1,all.y = T,by="Genus")
data.matrix<-as.data.frame(cbind(core_taxa=data1$core_taxa,data_core_percentenge))

ggplot(data = data.matrix,aes(x=core_taxa,y=data_core_percentenge))+
  geom_point(color="#BEBADA",size=3)+geom_smooth(method="lm",color="#BEBADA")+theme_bw()+
  labs(y = "core ARGs Relative Abundnace ",x="Core Genus Relative abundnace") +theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))

summary(lm(data_core_percentenge ~ core_taxa, data=data.matrix))
cor.test(data.matrix$core_taxa,data.matrix$data_core_percentenge,method="pearson")


cor.test(data1$core_arg,data1$total_arg,method="pearson")
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c( "#BEBADA","#80B1D3")

data_core_percentenge<-(data_core_sum/data_sum)
data_other<-1-(data_core_percentenge)
plotdata<-as.data.frame(t(rbind(`Core ARGs`=data_core_percentenge,`Other ARGs`=data_other)))
plotdata$sample<-rownames(plotdata)
plotdata<-gather(plotdata,type,amount,`Core ARGs`,`Other ARGs`)
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=amount,fill=type,color=type),alpha=0.8,stat="identity")+scale_fill_manual(values = color)+scale_color_manual(values = color)+theme_bw()+labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),legend.title= element_text(size=12.5),legend.text = element_text(size=12.5))

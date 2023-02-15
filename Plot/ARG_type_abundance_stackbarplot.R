library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=2,rowNames=T,colNames =T)
data$sum<-apply(data,1,sum)
data<-data%>%
  arrange(desc(sum))
data$sum<-NULL
data_other<-data[-(1:11),]
others<-apply(data_other,2,sum)
data<-data[(1:11),]
data<-rbind(data,others)
rownames(data)[nrow(data)]<-"others"
data$`ARGs type`<-rownames(data)
data$`ARGs type`<-factor(data$`ARGs type`,levels = c("aminoglycoside","bacitracin","beta-lactam","fosfomycin","macrolide-lincosamide-streptogramin","multidrug","rifamycin","sulfonamide","tetracycline","unclassified","vancomycin","others"))
plotdata<-data%>%
  gather(key="sample",value="amount",1:15)
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=amount,fill=`ARGs type`),color='black',stat="identity")+
  labs(x=NULL,y=NULL)+scale_fill_brewer(palette = "Set3")+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization aganist 16S")+theme_bw()

RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")

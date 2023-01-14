library(openxlsx)
library(tidyverse)
library(RColorBrewer)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/ARG/ARG_adjust.xlsx",sheet=1,rowNames=F,colNames =T)
colnames(data)[1]<-"ARGs type"
data$`ARGs type`<-factor(data$`ARGs type`,levels = c("aminoglycoside","bacitracin","beta-lactam","fosfomycin","macrolide-lincosamide-streptogramin","multidrug","rifamycin","sulfonamide","tetracycline","unclassified","vancomycin","others"))
plotdata<-data%>%
  gather(key="sample",value="amount",2:16)
ggplot(plotdata)+
  geom_bar(aes(x=sample,y=amount,fill=`ARGs type`),color='black',stat="identity")+
  labs(x=NULL,y=NULL)+scale_fill_brewer(palette = "Set3")+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization aganist 16S")+theme_classic()

RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")

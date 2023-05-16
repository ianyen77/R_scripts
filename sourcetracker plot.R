library(openxlsx)
library(tidyverse)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/sourcetracker/source tracker.xlsx")
data<-data[1:5,1:5]
plotdata<-data %>% 
  gather(key="source",value="percentage",2:5)
ggplot(plotdata)+
  geom_bar(aes(x=SampleID,y=percentage,fill=source),alpha=0.7,stat="identity",width=0.8)+
  scale_fill_manual(values =color )+theme_bw()+xlab("Sample")+ylab("ARGs abundance normalization against 16S")

library(openxlsx)
library(tidyverse)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/sourcetracker/source tracker.xlsx")
data<-data[1:6,1:5]
plotdata<-data %>% 
  gather(key="source",value="percentage",2:5)
color<- rev(c("#704D9E","#CF63A6", "#F7A086", "#F3E79A"))
plotdata$source<-factor(plotdata$source,levels = c("Unknown","Raw","Finished","Upstream"))
ggplot(plotdata)+
  geom_bar(aes(x=SampleID,y=percentage,fill=source),alpha=0.7,stat="identity",width=0.8)+theme_bw()+
  scale_fill_manual(values =color)+theme_bw()+xlab("Sample")+ylab("relative abundance")+labs(x="Sample",y="Relative Abundnace",title ="TAXA source Tracking")

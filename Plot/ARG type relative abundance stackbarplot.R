library(tidyverse)
library(openxlsx)
#這是用來畫ARGtype relative abundance 的script，data必須要先經過trasnform，把所有ARGtype 換成在每個樣本中的百分比
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv2.2/ARGoap_out.xlsx",sheet=2,rowNames=F,colNames=T,sep.names=" ")
dbpata1<-as.data.frame(apply(dbpata[,2:16],2,function(x) x/sum(x)))
rownames(dbpata1)<-dbpata$`Type level results`
dbpata<-dbpata1
dbpata[dbpata<0.05]<-0
data_sum<-apply(dbpata,2,sum)
dd<-1-data_sum
data2<-rbind(dbpata,dd)
rownames(data2)[25]<-"Others"
data3<-data2[apply(data2, 1, function(x) !all(x==0)),]
data3$`ARGs type`<-rownames(data3)
rownames(data3)<-NULL
data3$`ARGs type`<-factor(data3$`ARGs type`,levels = c("bacitracin","beta-lactam","fosfomycin","macrolide-lincosamide-streptogramin","multidrug","rifamycin","sulfonamide","tetracycline","unclassified","vancomycin","Others"),labels  = c("Bacitracin","Beta-lactam","Fosfomycin","MLS","Multidrug","Rifamycin","Sulfonamide","Tetracycline","Unclassified","Vancomycin","Others"))
data4<-gather(data =data3, key ="xx",value = "yf",1:15)
data5<-data4[-which(data4$yf==0),]
RColorBrewer::display.brewer.all()
RColorBrewer::display.brewer.pal(n=12,name="Set3")
RColorBrewer::brewer.pal(n=12,name="Set3")
#這樣設定是為了要配合前一張圖的顏色
#color<-c("#FFFFB3","#BEBADA","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F")
color<-c( "#FFFFB3" ,"#BC80BD" , "#FB8072" ,"#80B1D3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5" ,"#D9D9D9","#BEBADA" ,"#CCEBC5" ,"#FFED6F")
ggplot(data5)+
  geom_bar(aes(x=xx,y=yf,fill=`ARGs type`,color=`ARGs type`),alpha=0.7,stat="identity")+
  labs(x=NULL,y=NULL)+scale_fill_manual(values = color)+scale_color_manual(values = color)+theme_bw()+labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),axis.text =element_text(size=12)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))


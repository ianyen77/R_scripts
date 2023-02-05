library("openxlsx")
library("ggplot2")
library("tidyverse")
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/rel_abundance/phyla_rel_table.xlsx",sheet=1)
data<-data[,-(3:7)]
data<-data[,-1]
data$Phyla<-gsub(pattern = "p__",replacement = "",x=data$Phyla)
rownames(data)<-data$Phyla
data$Phyla<-NULL
data[data<0.05]<-0
data_sum<-apply(data,2,sum)
dd<-1-data_sum
data2<-rbind(data,dd)
rownames(data2)[nrow(data2)]<-"Others"
data3<-data2[apply(data2, 1, function(x) !all(x==0)),]
data3$phyla<-rownames(data3)
data3$phyla<-factor(data3$phyla,levels = c("Actinobacteria","Firmicutes","Planctomycetota","Proteobacteria","Others"))
rownames(data3)<-NULL
data4<-gather(data =data3, key ="xx",value = "yf",1:15)
data5<-data4[-which(data4$yf==0),]
ggplot(data5)+
  geom_bar(aes(x=xx,y=yf,fill=phyla),color='black',stat="identity")+
  scale_fill_brewer(palette = "Set3")+theme_bw()+
  labs(x="Sample",y="Relative abundance")+theme(axis.title = element_text(size=12.5),legend.title= element_text(size=12.5),legend.text = element_text(size=12.5))

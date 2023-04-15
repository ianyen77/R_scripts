library(openxlsx)
library(tidyverse)
#amplicondata----------------------
data<-read.xlsx("C:/Users/USER/Desktop/lab/台水計畫/DATA/台水二次定序/genus_table.xlsx")
data<-data%>%
  separate(X1,into=c("d","p","c","o","f","g"),sep=";")%>%
  select(-d,-p,-c,-o,-f)
data<-data[data$g=="g__Mycobacterium",]

rownames(data)<-data$g
data$g<-NULL
data<-data%>%
  pivot_longer(1:6,names_to = "group", values_to = "value")
data$group<-c("Biofilm","Raw","Finished","Upstream","Midsteram","Downsteram")
data$group<-factor(data$group,levels=c("Biofilm","Raw","Finished","Upstream","Midsteram","Downsteram"))
ggplot(data) +
  geom_segment(aes(x=group, xend=group, y=0, yend=value), color="grey")+
  geom_point( aes(x=group, y=value),color="#704D9E", size=4,alpha=0.5) +
  geom_text(aes(x=group,y=value+0.005,label=round(value,3)),size=4)+
  theme_bw() +
  xlab("Sample") +
  ylab("Relative abundance")+theme(axis.title = element_text(size=12.5),legend.title= element_text(size=12.5),legend.text = element_text(size=12.5),plot.title = element_text(hjust = 0.5))+ggtitle("Amplicon sequencing Mycobacterium Relative Abudance")

#metagenomic data-----------------------
data2<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/genus_rel_table.xlsx")
data2<-data2[,c(-(1:5),-7)]%>%
  column_to_rownames("Genus")%>%
  t()%>%
  as.data.frame()%>%
  select(g__Mycobacterium,g__Mycolicibacterium)%>%
  rownames_to_column("Sample")
data2$sum<-data2$g__Mycobacterium+data2$g__Mycolicibacterium
data2$group<-rep(c("Raw","Finished","Upstream","Midsteram","Downsteram"),each=3)
data2_sta<-data2%>%
  group_by(group)%>%
  summarise(across(c(sum), list(mean = mean, sd = sd)))

data2_sta$group<-factor(data2_sta$group,levels = c("Raw","Finished","Upstream","Midsteram","Downsteram"))
data4<-merge(data,data2_sta,all=T,by="group")
ggplot(data2_sta,aes(x=group,y=sum_mean,ymin=sum_mean-sum_sd,ymax=sum_mean+sum_sd))+
         geom_point()+
         geom_errorbar()+theme_bw()+geom_line(group=1)
hcl.colors(5,"sunset")
#Mergeplot
ggplot(data4,)+
  geom_point(aes(x=group,y=sum_mean),color="#F9B282",size=4,alpha=0.7)+
  geom_errorbar(aes(x=group,y=sum_mean,ymin=sum_mean-sum_sd,ymax=sum_mean+sum_sd),color="#F9B282",width=0.1)+theme_bw()+geom_line(aes(x=group,y=sum_mean,ymin=sum_mean-sum_sd,ymax=sum_mean+sum_sd),group=1,color="#F9B282",alpha=0.7)+
  geom_segment(aes(x=group, xend=group, y=0, yend=value), color="grey")+
  geom_point( aes(x=group, y=value),color="#704D9E", size=4,alpha=0.7) +
  theme_bw() +
  xlab("group") +
  ylab("relative abundance")

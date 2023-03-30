library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
library(tidyverse)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/genus_rel_table.xlsx",sheet=1,rowNames=F,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
rownames(envata)<-envata$Genus
envata<-envata[,-(1:7)]
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
envata<-envata%>%
  filter(apply(envata,1,function(x) sum(x>0)>=15))
envata$sum<-apply(envata,1,sum)
envata<-envata%>%
  arrange(desc(sum))
envata<-envata[1:10,]
envata$sum<-NULL
envata <- decostand(envata, method = 'hellinger')
dbpata<-as.data.frame(t(dbpata))
envata<-as.data.frame(t(envata))
#判斷要用RDA還是CCA,if DCA1 length >4 use cca
sel <- decorana(dbpata)
sel

arg_cca_0<-rda(dbpata ~1, data=envata)
arg_cca_1<-rda(dbpata ~ g__Mycolicibacterium+g__Metabacillus+g__Nitrobacter+g__Nocardioides , data=envata)
summary(arg_cca_1)
vif.cca(arg_cca_1)
mod.u <- step(arg_cca_0, scope = formula(arg_cca_1), test = "perm")# "perm"增加P值等参数
mod.d <- step(arg_cca_0, scope = (list(lower = formula(arg_cca_0), upper = formula(arg_cca_1))))
mod.d
anova(arg_cca_1, by = "term")

## 繪圖數據提取
s.RDA=arg_cca_1$CCA$u#提取樣本之特徵值
B.rda.data=data.frame(arg_cca_1$CCA$u[,1:2],groupata$location)
B.rda.data=data.frame(B.rda.data,groupata$sample)
colnames(B.rda.data)=c("RDA1","RDA2","Location","Sample")
B.rda1 =round(arg_cca_1$CCA$eig[1]/sum(arg_cca_1$CCA$eig)*100,2) #計算第一軸特徵值之佔比
B.rda2 =round(arg_cca_1$CCA$eig[2]/sum(arg_cca_1$CCA$eig)*100,2) #計算第二軸特徵值之佔比
s.RDA
e.RDA=arg_cca_1$CCA$v # 提取變量特徵值
env.RDA=arg_cca_1$CCA$biplot
B.rda.env=arg_cca_1$CCA$biplot[,1:2]# 提取環境因子特徵值
env.RDA
# 檢查環境因子相關性
## 檢查整體環境因子對變量變化之相關性的顯著性
RDA.perm=permutest(arg_cca_1,permu=999)
RDA.perm
## 檢查整體環境因子對變量變化之相關性的顯著性
RDA.env=envfit(arg_cca_1,envata,permu=999)
RDA.env
color1<-c("#FB8072","#80B1D3","#BEBADA","#FDB462","#B3DE69")
B.rda.data$Location<-factor(B.rda.data$Location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
B.plot=ggplot(data=B.rda.data,aes(RDA1,RDA2),color=Location)+
  geom_point(aes(color=Location,fill=Location),size=3,alpha=0.7)+
  #scale_color_manual(values=c("red","blue","green","black","grey","darkgreen"))+
  labs(x=paste("RDA1",B.rda1," %"),y=paste("RDA2",B.rda2," %"))+scale_color_manual(values=color1)+
  theme_bw()+geom_vline(xintercept = 0, color = 'gray', linetype = 2) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2)
B.plot=B.plot+
  geom_segment(data=B.rda.env,aes(x=0,y=0,xend=B.rda.env[,1],yend=B.rda.env[,2]),colour="black",linewidth=0.01,alpha=0.7,
               arrow=arrow(angle = 35,length=unit(0.3,"cm")))+
  geom_text(data=B.rda.env,aes(x=B.rda.env[,1],y=B.rda.env[,2],label=rownames(B.rda.env)),size=2.6,
            colour="black",vjust=(0.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]),hjust=(1.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]))+
  theme(legend.position = "top")#+theme(axis.title = element_text(family = "serif", face = "bold", size = 18,colour = "black"))
print(B.plot)
?geom_segment

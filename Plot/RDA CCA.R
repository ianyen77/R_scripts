library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
library(tidyverse)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/family_rel_table.xlsx",sheet=1,rowNames=F,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-envata[,-c(1:4)]
envata<-envata[,-c(2:3)]
rownames(envata)<-envata$Family
envata$Family<-NULL
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
envata$sum<-apply(envata,1,sum)
envata<-envata%>%
  arrange(desc(sum))
envata<-envata[1:30,]
envata$sum<-NULL
envata <- decostand(envata, method = 'hellinger')
sel <- decorana(dbpata)
sel
dbpata<-as.data.frame(t(dbpata))
envata<-as.data.frame(t(envata))
arg_cca<-cca(dbpata ~f__Mycobacteriaceae+f__Nitrobacteraceae+ f__Bacillaceae+ f__Sphingomonadaceae, data=envata)
arg_cca
summary(arg_cca)
vif.cca(arg_cca)
arg_cca.0<-cca(dbpata~1,data=envata)
cca.step_forward<-step(arg_cca.0, scope = formula(arg_cca), test = "perm")
mod.d <- step(arg_cca.0, scope = (list(lower = formula(arg_cca.0), upper = formula(arg_cca))))
anova(arg_cca, by = "term")

## 繪圖數據提取
s.RDA=arg_cca$CCA$u#提取樣本之特徵值
B.rda.data=data.frame(arg_cca$CCA$u[,1:2],groupata$location)
B.rda.data=data.frame(B.rda.data,groupata$sample)
colnames(B.rda.data)=c("RDA1","RDA2","Locatoin","Sample")
B.rda1 =round(arg_cca$CCA$eig[1]/sum(arg_cca$CCA$eig)*100,2) #計算第一軸特徵值之佔比
B.rda2 =round(arg_cca$CCA$eig[2]/sum(arg_cca$CCA$eig)*100,2) #計算第二軸特徵值之佔比
s.RDA
e.RDA=arg_cca$CCA$v # 提取變量特徵值
env.RDA=arg_cca$CCA$biplot
B.rda.env=arg_cca$CCA$biplot[,1:2]# 提取環境因子特徵值
env.RDA
# 檢查環境因子相關性
## 檢查整體環境因子對變量變化之相關性的顯著性
RDA.perm=permutest(arg_cca,permu=999)
RDA.perm
## 檢查整體環境因子對變量變化之相關性的顯著性
RDA.env=envfit(arg_cca,envata,permu=999)
RDA.env
B.plot=ggplot(data=B.rda.data,aes(RDA1,RDA2),color=locatoin)+
  geom_point(aes(color=Locatoin,fill=Locatoin),size=2)+geom_text(aes(label=Sample),size=3,color="black")+
  #scale_color_manual(values=c("red","blue","green","black","grey","darkgreen"))+
  labs(x=paste("RDA1",B.rda1," %"),y=paste("RDA2",B.rda2," %"))+
  theme_bw()
B.plot=B.plot+theme(panel.grid=element_blank())+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
  geom_segment(data=B.rda.env,aes(x=0,y=0,xend=B.rda.env[,1],yend=B.rda.env[,2]),colour="grey ",size=1,
               arrow=arrow(angle = 35,length=unit(0.3,"cm")))+
  geom_text(data=B.rda.env,aes(x=B.rda.env[,1],y=B.rda.env[,2],label=rownames(B.rda.env)),size=2.6,
            colour="black",vjust=(0.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]),hjust=(1.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]))+
  theme(legend.position = "top")#+theme(axis.title = element_text(family = "serif", face = "bold", size = 18,colour = "black"))
print(B.plot)


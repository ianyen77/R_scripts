library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
dbpata<-read.xlsx("C:/Users/USER/Desktop/DBP for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/env for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/group for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata_dca<-read.xlsx("C:/Users/USER/Desktop/DBP for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
RDA=rda(dbpata,envata,scale=TRUE)
RDA
summary(RDA)
## 繪圖數據提取
s.RDA=RDA$CCA$u#提取樣本之特徵值
B.rda.data=data.frame(RDA$CCA$u[,1:2],groupata$plant)
B.rda.data=data.frame(B.rda.data,groupata$sample)
colnames(B.rda.data)=c("RDA1","RDA2","plant","Sample")
B.rda1 =round(RDA$CCA$eig[1]/sum(RDA$CCA$eig)*100,2) #計算第一軸特徵值之佔比
B.rda2 =round(RDA$CCA$eig[2]/sum(RDA$CCA$eig)*100,2) #計算第二軸特徵值之佔比
s.RDA
e.RDA=RDA$CCA$v # 提取變量特徵值
env.RDA=RDA$CCA$biplot
B.rda.env=RDA$CCA$biplot[,1:2]# 提取環境因子特徵值
env.RDA
# 檢查環境因子相關性
## 檢查整體環境因子對變量變化之相關性的顯著性
RDA.perm=permutest(RDA,permu=999)
RDA.perm
## 檢查整體環境因子對變量變化之相關性的顯著性
RDA.env=envfit(RDA,envata,permu=999)
RDA.env
B.plot=ggplot(data=B.rda.data,aes(RDA1,RDA2),color=plant)+
  geom_point(aes(color=plant,fill=plant),size=2)+geom_text(aes(label=Sample),size=3,color="black")+
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


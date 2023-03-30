library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
library(tidyverse)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/Genus_TAXA_vpa.xlsx",sheet=1,rowNames=F,colNames=T,sep.names=" ")
MGE<-read.xlsx("C:/Users/USER/Desktop/Genus_TAXA_vpa.xlsx",sheet=2,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
rownames(envata)<-envata$Genus
envata<-envata[,-(1:7)]
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
#Genus 篩選
envata<-envata%>%
  filter(apply(envata,1,function(x) sum(x>0)>=15))
envata$sum<-apply(envata,1,sum)
envata<-envata%>%
  arrange(desc(sum))
envata<-envata[1:10,]
envata$sum<-NULL
#MGE篩選
MGE<-MGE%>%
  filter(apply(MGE,1,function(x) sum(x>0)>=5))
MGE$sum<-apply(MGE,1,sum)
MGE<-MGE%>%
  arrange(desc(sum))
MGE<-MGE[1:5,]
MGE$sum<-NULL
#data format transform
#envata<-scale(envata)
envata <- decostand(envata, method = 'hellinger')
dbpata<-as.data.frame(t(dbpata))
envata<-as.data.frame(t(envata))
MGE<-as.data.frame(t(MGE))
envata<-as.data.frame(cbind(envata,MGE))
envata<-as.data.frame(scale(envata))
#檢查環境變量的相關性，共線的環境變量要去除
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use = "complete.obs"))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste(prefix, txt, sep = "")
  if (missing(cex.cor)) cex <- 0.8 / strwidth(txt)
  
  test <- cor.test(x, y)
  # borrowed from printCoefmat
  Signif <- symnum(
    test$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.05, 0.1, 1),
    symbols = c("*", ".", " ")
  )
  
  text(0.5, 0.5, txt, cex = cex * r)
  text(.8, .8, Signif, cex = cex, col = 2)
}
pairs(envata,lower.panel = panel.smooth,upper.panel = panel.cor)
#判斷要用RDA還是CCA,if DCA1 length >4 use cca
sel <- decorana(dbpata)
sel
#變量篩選
arg_cca<-rda(dbpata ~., data=envata)
summary(arg_cca)
arg_cca_0<-rda(dbpata ~1, data=envata)
summary(arg_cca_0)
#這步變量要一直不斷的篩選，直到組合起來所有變量的VIF都小於10
arg_cca_1<-rda(dbpata ~ g__Mycolicibacterium+g__Paracoccus +g__Metabacillus+insertion_element_IS91__IS91+transposase__tnpA , data=envata)
summary(arg_cca_1)
vif.cca(arg_cca_1)
mod.u <- step(arg_cca_0, scope = formula(arg_cca_1), test = "perm")# "perm"增加P值等参数
mod.d <- step(arg_cca_0, scope = (list(lower = formula(arg_cca_0), upper = formula(arg_cca_1))))
mod.d
#檢驗變量的顯著度
set.seed(111)
anova(arg_cca_1,by = "term")
#檢驗軸的顯著度
anova(arg_cca_1,by = "axis",step=1000)

#VPA
#dbpata<-dbpata[-(1:3),]
#envata<-envata[-(1:3),]
rda.vpa <- varpart(dbpata, envata[,c(1:7)],envata[,c(11:12)],chisquare = FALSE) 
str(rda.vpa)
rda.vpa
plot(rda.vpa)
## 繪圖數據提取
s.RDA=arg_cca_1$CCA$u#提取樣本之特徵值
B.rda.data=data.frame(arg_cca_1$CCA$u[,1:2],groupata$location)
B.rda.data=data.frame(B.rda.data,groupata$sample)
colnames(B.rda.data)=c("RDA1","RDA2","Location","Sample")
B.rda1 =round(arg_cca_1$CCA$eig[1]/sum(arg_cca_1$CCA$eig)*100,2) #計算第一軸特徵值之佔比
B.rda2 =round(arg_cca_1$CCA$eig[2]/sum(arg_cca_1$CCA$eig)*100,2) #計算第二軸特徵值之佔比
s.RDA

##繪製ARG subtype 在RDA上的位置
e.RDA=as.data.frame(arg_cca_1$CCA$v)# 提取變量特徵值
e.RDA$labels<-rownames(e.RDA)
core_ARG_list<-c("aminoglycoside__aac(2')-I","bacitracin__bacA","beta-lactam__class A beta-lactamase","fosfomycin__fosX","fosmidomycin__rosB","macrolide-lincosamide-streptogramin__erm(39)","macrolide-lincosamide-streptogramin__macB","multidrug__acrB","multidrug__bpeF","multidrug__ceoB"
,"multidrug__emrE","multidrug__mdtB","multidrug__mdtC","multidrug__mdtF","multidrug__mexF","multidrug__mexW","multidrug__multidrug_ABC_transporter","multidrug__multidrug_transporter","quinolone__mfpA","rifamycin__ADP-ribosylating transferase_arr","rifamycin__rifampin monooxygenase","tetracycline__tetV","unclassified__transcriptional regulatory protein CpxR cpxR","vancomycin__vanA","vancomycin__vanR","vancomycin__vanS"
)
e.RDA<-e.RDA[e.RDA$labels %in% core_ARG_list, ] 
e.RDA<-e.RDA %>% 
  separate(labels,into=c("type","subtype"),sep = "__")

##
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
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal(8,"Purples")
color1<-c("#FB8072","#80B1D3","#BEBADA","#FDB462","#B3DE69")
B.rda.data$Location<-factor(B.rda.data$Location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
B.plot=ggplot(data=B.rda.data,aes(RDA1,RDA2),color=Location)+
  geom_point(aes(color=Location,fill=Location),size=3,alpha=0.7)+
  #scale_color_manual(values=c("red","blue","green","black","grey","darkgreen"))+
  labs(x=paste("RDA1",B.rda1," %"),y=paste("RDA2",B.rda2," %"))+scale_color_manual(values=color1)+
  theme_bw()+geom_vline(xintercept = 0, color = 'gray', linetype = 2) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2)
B.plot=B.plot+
  geom_segment(data=B.rda.env,aes(x=0,y=0,xend=B.rda.env[,1],yend=B.rda.env[,2]),colour="#6A51A3",linewidth=0.3,alpha=0.7,
               arrow=arrow(angle = 35,length=unit(0.3,"cm")))+
  geom_text(data=B.rda.env,aes(x=(B.rda.env[,1]+0.05),y=(B.rda.env[,2]),label=rownames(B.rda.env)),size=3,
            colour="#6A51A3",vjust=(0.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]),hjust=(1.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]))#+theme(axis.title = element_text(family = "serif", face = "bold", size = 18,colour = "black"))
B.plot

e.plot=ggplot(data=e.RDA,aes(RDA1,RDA2))+
  geom_point(size=1,alpha=0.7)+geom_text(aes(label=e.RDA$subtype),size=2)+
  #scale_color_manual(values=c("red","blue","green","black","grey","darkgreen"))+
  labs(x=paste("RDA1",B.rda1," %"),y=paste("RDA2",B.rda2," %"))+scale_color_manual(values=color1)+
  theme_bw()+geom_vline(xintercept = 0, color = 'gray', linetype = 2) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2)
e.plot=e.plot+
  geom_segment(data=B.rda.env,aes(x=0,y=0,xend=B.rda.env[,1],yend=B.rda.env[,2]),colour="#6A51A3",linewidth=0.3,alpha=0.7,
               arrow=arrow(angle = 35,length=unit(0.3,"cm")))+
  geom_text(data=B.rda.env,aes(x=(B.rda.env[,1]+0.05),y=(B.rda.env[,2]),label=rownames(B.rda.env)),size=3,
            colour="#6A51A3",vjust=(0.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]),hjust=(1.5-sign(B.rda.env[,1]))/2,angle=(45)*atan(B.rda.env[,2]/B.rda.env[,1]))#+theme(axis.title = element_text(family = "serif", face = "bold", size = 18,colour = "black"))
e.plot

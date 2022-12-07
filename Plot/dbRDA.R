library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
dbpata<-read.xlsx("C:/Users/USER/Desktop/ASV.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/env.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/group.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata <- decostand(dbpata, method = 'hellinger')
dbRDA=dbrda(dbpata ~Free_Chlorine+pH+TOC, envata, dist="bray")
test<-anova.cca(dbRDA, by="margin", permu=999)
test
#這部分跟下面的ordiR2step可以擇一進行這兩都是在進行環境變量的篩選(但因為完美的篩選組合並不存在)
otu.tab.1<-dbrda(dbpata ~Temperature+Total_Manganese+ORP+pH+TOC+Dissolved_Manganese+Distance+Sulfate+ATP, envata,,dist="bray")
vif.cca(otu.tab.1)
mod.u <- step(otu.tab.0,scope = formula(otu.tab.1), test = "perm")# "perm"增加P值等参数
mod.d <- step(otu.tab.0, scope = (list(lower = formula(otu.tab.0), upper = formula(otu.tab.1))))
mod.d
otu.tab.1<-dbrda(dbpata ~Temperature+Total_Manganese+ORP+pH+TOC+Dissolved_Manganese+Distance+Sulfate+ATP, envata,,dist="bray")
#找不到最佳的combination
fwd.sel <- ordiR2step(dbrda(dbpata ~ 1, envata, dist="bray"), # lower model limit (simple!)
                      scope = formula(dbRDA), # upper model limit (the "full" model)
                      direction = "forward",
                      R2scope = TRUE, # can't surpass the "full" model's R2
                      pstep = 1000,
                      trace = FALSE) # change to TRUE to see the selection process!
fwd.sel$call

## 繪圖數據取出
summary(dbRDA)
s.dbRDA=dbRDA$CCA$u # 樣本特徵
s.dbRDA
e.dbRDA=dbRDA$CCA$v # 物種特徵
B.dbrda.env=dbRDA$CCA$biplot[,1:2]# 環境因子特徵
B.dbrda.env_sign <- B.dbrda.env[which(test$`Pr(>F)`<0.1),1:2]

B.dbrda.data=data.frame(dbRDA$CCA$u[,1:2],groupata$plant)
B.dbrda.data=data.frame(B.dbrda.data,groupata$sample)
colnames(B.dbrda.data)=c("dbRDA1","dbRDA2","plant","Sample")
B.dbrda1 =round(dbRDA$CCA$eig[1]/sum(dbRDA$CCA$eig)*100,2) #計算第一軸特徵值之佔比
B.dbrda2 =round(dbRDA$CCA$eig[2]/sum(dbRDA$CCA$eig)*100,2) #計算第二軸特徵值之


B.plot=ggplot(data=B.dbrda.data,aes(x=dbRDA1,y=dbRDA2,color=plant))+
  geom_point(,size=2)+geom_text(aes(label=Sample),size=3,color="black")+
  labs(x=paste("db-RDA1",B.dbrda1," %"),y=paste("db-RDA2",B.dbrda2," %"))+
  theme_bw()
B.plot=B.plot+theme(panel.grid=element_blank())+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 0)+
  geom_segment(data=B.dbrda.env_sign,aes(x=0,y=0,xend=B.dbrda.env_sign[,1]/4,yend=B.dbrda.env_sign[,2]/4),arrow = arrow(length = unit(0.2, 'cm')), size = 0.5, color = 'blue')+
  geom_text(data = B.dbrda.env_sign, aes(B.dbrda.env_sign[,1]/4 * 1.1, B.dbrda.env_sign[,2]/4 * 1.1,label=rownames(B.dbrda.env_sign)), color = 'blue', size = 3)
  
print(B.plot)

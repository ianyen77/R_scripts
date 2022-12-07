library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
#input row=sample column=變數
dbpata<-read.xlsx("C:/Users/USER/Desktop/新增資料夾/ASV.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/新增資料夾/group.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/新增資料夾/env.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata <- decostand(dbpata, method = 'hellinger')
family_bray<-vegdist(dbpata, method="bray")
length_1<-length(rownames(dbpata))
pcoa = cmdscale(family_bray, k=(length_1-1), eig=TRUE)
points=scores(pcoa)
eig = pcoa$eig
eig
points<-as.data.frame(points)
points <-cbind(points, groupata[match(rownames(points), rownames(groupata)), ])
#用Adonis 來檢定組間差異是不是顯著的
x<-adonis(dbpata~disinfection,data = groupata)
x$aov.tab
#將環境變量擬合到PCoA中，並使用BH校正多重檢定
pcoa_ef <- envfit(pcoa~., data = envata, perm = 999, choices = c(1,2), display = 'sites') 
pcoa_ef
pcoa_ef_adj <- pcoa_ef
pcoa_ef_adj$vectors$pvals <- p.adjust(pcoa_ef_adj$vectors$pvals, method = 'BH') 
pcoa_ef_adj
#將環境變量的座標、多元回歸r2、回歸顯著性(校正後的p值)
pcoa_env <- data.frame(cbind(pcoa_ef_adj$vectors$arrows, pcoa_ef_adj$vectors$r, pcoa_ef_adj$vectors$pvals)) 
names(pcoa_env) <- c('PC1', 'PC2', 'r2', 'p.adj') 
#將跟變數有顯著相關的環境因子留下
pcoa_env_sign <- pcoa_env[which(pcoa_env$p.adj < 0.1),1:2]
pcoa_env_sign$sample <- NA 
pcoa_env_sign$group <- rownames(pcoa_env_sign)



ggplot(points, aes(x=Dim1, y=Dim2,colour=plant))+
  theme_bw()+
  geom_point(size=3)+geom_text(aes(label=sample_ID),size=3,color="black")+geom_hline(yintercept=0) + geom_vline(xintercept=0)+theme_bw() + theme(panel.grid=element_blank())+
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="bray_curtis PCoA")+
  geom_segment(data = pcoa_env_sign, aes(x = 0,y = 0, xend = PC1/4, yend = PC2/4), arrow = arrow(length = unit(0.2, 'cm')), size = 0.5, color = 'blue') +
  geom_text(data = pcoa_env_sign, aes(PC1/4 * 1.1, PC2/4 * 1.1, label = group), color = 'blue', size = 3)

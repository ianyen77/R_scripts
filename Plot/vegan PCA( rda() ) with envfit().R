library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
dbpata<-read.xlsx("C:/Users/USER/Desktop/DBP for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
envata<-read.xlsx("C:/Users/USER/Desktop/env for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/group for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata_dca<-read.xlsx("C:/Users/USER/Desktop/DBP for DCA.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
pca=rda(dbpata,scale=T) # scale=T，將數值標準化
#加入環境變數
tbpca_ef <- envfit(pca~., data = envata, perm = 999, choices = c(1,2), display = 'sites') 
tbpca_ef
tbpca_ef_adj <- tbpca_ef 
tbpca_ef_adj$vectors$pvals <- p.adjust(tbpca_ef_adj$vectors$pvals, method = 'bonferroni') 
tbpca_ef_adj
#將環境變量的座標、多元回歸r2、回歸顯著性(校正後的p值)
tbpca_env <- data.frame(cbind(tbpca_ef_adj$vectors$arrows, tbpca_ef_adj$vectors$r, tbpca_ef_adj$vectors$pvals)) 
names(tbpca_env) <- c('PC1', 'PC2', 'r2', 'p.adj') 
#將跟變數有顯著相關的環境因子留下
tbpca_env_sign <- tbpca_env[which(tbpca_env$p.adj < 0.25),1:2]
tbpca_env_sign$sample <- NA 
tbpca_env_sign$group <- rownames(tbpca_env_sign)
## 繪圖數據取出
s.pca=pca$CA$u # 取出樣本特徵值，可繪圖
s.pca
e.pca=pca$CA$v # 取出變量特徵值，可用於繪圖
e.pca
eig=pca$CA$eig # 提取特徵根，計算pc1、PC2之解釋度
#作碎石圖
screeplot(pca,bstick = TRUE,npcs = length(pca$CA$eig))
#將PC1、pc2之占比轉換成百分比
eig.sum=sum(eig)
eig.percentage=round(eig/eig.sum*100,2)
df_pcs <-data.frame(s.pca, Species = groupata$plant,Sample=groupata$sample)
ggplot(df_pcs,aes(x=PC1,y=PC2,color=Species))+
  geom_point(aes(color = groupata$plant),size=3) +geom_text(aes(label=Sample),size=3,color="black")+
  #scale_color_manual(values = c('red', 'orange', 'green3')) + 
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), legend.title = element_blank(), legend.key = element_rect(fill = 'transparent')) +  
  labs(x = paste('PCA1: ', eig.percentage[1], '%'), y = paste('PCA2: ',eig.percentage[2], '%')) + 
  geom_vline(xintercept = 0, color = 'gray', size = 0.5) +  
  geom_hline(yintercept = 0, color = 'gray', size = 0.5) + 
  geom_segment(data = tbpca_env_sign, aes(x = 0,y = 0, xend = PC1/5, yend = PC2/5), arrow = arrow(length = unit(0.2, 'cm')), size = 0.5, color = 'blue') +
  geom_text(data = tbpca_env_sign, aes(PC1/5 * 1.1, PC2/5 * 1.1, label = group), color = 'blue', size = 3)

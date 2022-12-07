library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
dbpata<-read.xlsx("C:/Users/USER/Desktop/ASV.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/group.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
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
ggplot(points, aes(x=Dim1, y=Dim2,colour=plant))+
  theme_bw()+
  geom_point(size=3)+geom_text(aes(label=sample_ID),size=3,color="black")+geom_hline(yintercept=0) + geom_vline(xintercept=0)+theme_bw() + theme(panel.grid=element_blank())+
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="bray_curtis PCoA")

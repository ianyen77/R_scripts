library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
library(scatterplot3d)
library(pairwiseAdonis)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/bacteria_db/bracken_out/combine_brackenout/combine_bracken_s.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/standard_db/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
#因為species太多了，我們把小於0.01%的species清掉
#dbpata[dbpata<0.0001]<-0
#dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
#dbpata1<-as.data.frame(apply(dbpata,2,function(x) x/sum(x)))
#dbpata<-dbpata1
#以上不一定需要，看你分析的DATA
rownames(dbpata)<-dbpata$Species
dbpata<-dbpata[,-(1:7)]
dbpata <-as.data.frame(t(dbpata))
#Hellinger不一定需要，ARG不用
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
x<-adonis2(dbpata~location,data = groupata)
x
dbpata_adonis <- paste0("adonis R2: ",round(x$R2,2), "; P-value: ", x$`Pr(>F)`)
#檢驗adnois，我們需要檢驗一下這個adonis的結果是不是因為分組數據的離散程度不同造成的
dbpata.dis <- vegdist(dbpata, method="bray", binary=F)
dispersion <- betadisper(dbpata.dis, group=groupata$location)
permutest(dispersion,permutations = 99999)
#dune.pairwise.adonis <- pairwise.adonis(x=dbpata, factors=groupata$location, sim.function = "vegdist",sim.method = "bray",reduce = NULL,perm = 999)

#準備畫圖
points$location<-factor(points$location,levels=c('Raw', 'Finished', 'Upstream','Midstream','Downstream'))
RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c("#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69")
#if you use the Rcolor brewer,你需要利用scale_color_brewer(palette="paletee_name")
#自選顏色的話用Scale_color_manual(values=color_vector)

#2dpcoa
ggplot(points, aes(x=Dim1, y=Dim2,colour=location))+
  theme_bw()+
  geom_point(size=3,alpha=0.7)+#geom_text(aes(label=sample_ID),size=3,color="black",)+
  scale_color_manual(values = color)+geom_vline(xintercept = 0, color = 'gray', linetype = 2) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2,) +
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="Bray_curtis PCoA")+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))+scale_color_manual("Location",values =color)
#3dpoca
colors=color[as.numeric(points$location)]
colnames(points)[1]<-paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep="")
colnames(points)[2]<-paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep="")
colnames(points)[3]<-paste("PCoA 3 (", format(100 * eig[3] / sum(eig), digits=4), "%)", sep="")
scatterplot3d(points[,1:3],color=colors,main="Bray_curtis PCoA",pch=16,cex.symbols = 1.3)
legend("bottom",col=color,legend=levels(points$location),pt.bg = colors,pch=16,inset=-0.17,xpd =T,horiz = T)
?scatterplot3d
7.21 5.72

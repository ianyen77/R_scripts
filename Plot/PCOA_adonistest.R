library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/ARGoap_out.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
#這個不一定要，下面這個只是把全部都是0的rows清掉
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
dbpata <-as.data.frame(t(dbpata))
#Hellinger轉換不是每種EATA都需要的，通常指有taxa需要
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
dbpata.dist <- vegdist(dbpata, method="bray", binary=F)
#檢驗Adonis是不是因為betadispersion造成的
dispersion <- betadisper(dbpata.dist, group=groupata$location)
permutest(dispersion)
dbpata_adonis <- paste0(" adonis R2: ",round(dune.div$R2,2), "; P-value: ", dune.div$`Pr(>F)`)
#開始畫圖，先選色
points$location<-factor(points$location,levels=c('Raw', 'Finished', 'Upstream','Midstream','Downstream'))
RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c("#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69")
#if you use the Rcolor brewer,你需要利用scale_color_brewer(palette="paletee_name")
#自選顏色的話用Scale_color_manual(values=color_vector)

ggplot(points, aes(x=Dim1, y=Dim2,colour=location))+
  theme_bw()+
  geom_point(size=3)+#geom_text(aes(label=sample_ID),size=3,color="black",)+
  scale_color_manual(values = color)+geom_hline(yintercept=0)+ geom_vline(xintercept=0)+theme(panel.grid=element_blank())+
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="Bray_curtis PCoA")+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))



library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/rel_abundance/species_rel_table.xlsx",sheet=2,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata <-as.data.frame(t(dbpata))
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
x<-adonis(dbpata~location,data = groupata)
x$aov.tab
points$location<-factor(points$location,levels=c('Raw', 'Finished', 'Upstream','Midstream','Downstream'))
ggplot(points, aes(x=Dim1, y=Dim2,colour=location))+
  theme_bw()+
  geom_point(size=3)+#geom_text(aes(label=sample_ID),size=3,color="black",)+
  scale_color_manual(values = color)+geom_hline(yintercept=0)+ geom_vline(xintercept=0)+theme(panel.grid=element_blank())+
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="Bray_curtis PCoA")+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))
RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c("#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69")
#if you use the Rcolor brewer,你需要利用scale_color_brewer(palette="paletee_name")
#自選顏色的話用Scale_color_manual(values=color_vector)
?scale_color_brewer()
?theme_bw

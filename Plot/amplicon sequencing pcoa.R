library(vegan)
library(ecodist)
library(openxlsx)
library(ggfortify)
library(RColorBrewer)
dbpata1<-read.xlsx("C:/Users/USER/Desktop/lab/台水計畫/DATA/台水二次定序/2023.02.24 species table.xlsx",sheet=3,rowNames=F,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/台水計畫/DATA/台水二次定序/2023.02.24 species table.xlsx",sheet=5,rowNames=T,colNames=T,sep.names=" ")
groupata$Location<-c("Biofilm","Raw","Finished","Upsteram","Midstream","Downstream")
groupata$location<-NULL
dbpata<-dbpata1
dbpata[is.na(dbpata)]<-0
rownames(dbpata)<-dbpata$Species
dbpata$Species<-NULL
dbpata <- decostand(dbpata, method = 'hellinger')
dbpata <-as.data.frame(t(dbpata))
family_bray<-vegdist(dbpata, method="bray")
length_1<-length(rownames(dbpata))
pcoa = cmdscale(family_bray, k=(length_1-1), eig=TRUE)
points=scores(pcoa)
eig = pcoa$eig
eig
points<-as.data.frame(points)
points <-cbind(points, groupata[match(rownames(points), rownames(groupata)), ])
hcl.colors(2,"sunset")

ggplot(points, aes(x=Dim1, y=Dim2))+
  theme_bw()+
  geom_point(size=3,alpha=0.6,color="#704D9E",fill="#704D9E")+geom_vline(xintercept = 0, color = 'gray', linetype = 2) +geom_text(aes(label=Location),size=3.6,color="black",hjust = 0.4, vjust = 2)+
  geom_hline(yintercept = 0, color = 'gray', linetype = 2,) +
  labs(x=paste("PCoA 1 (", format(100 * eig[1] / sum(eig), digits=4), "%)", sep=""),
       y=paste("PCoA 2 (", format(100 * eig[2] / sum(eig), digits=4), "%)", sep=""),
       title="Bray_curtis PCoA")+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))


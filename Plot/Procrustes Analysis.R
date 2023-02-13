library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/rel_abundance/species_rel_table.xlsx",sheet=2,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/Taxa/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
arg_data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/ARG/argoap_out.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
#這個不一定要，下面這個只是把全部都是0的rows清掉
dbpata[dbpata<0.0001]<-0
dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
dbpata1<-as.data.frame(apply(dbpata,2,function(x) x/sum(x)))
dbpata <-as.data.frame(t(dbpata1))
arg_data<-as.data.frame(t(arg_data))
dbpata <- decostand(dbpata, method = 'hellinger')
taxa_bray<-vegdist(dbpata, method="bray")
arg_bray<-vegdist(arg_data, method="bray")
#普氏分析可以用pcoa,pca,nmds等都可以，看哪個分離的好
pcoa1 = cmdscale(taxa_bray, eig=TRUE)
pcoa2 = cmdscale(arg_bray, eig=TRUE)
#mds.taxa<-monoMDS(taxa_bray)
#mds.arg<-monoMDS(arg_bray)
pro.g.s<-procrustes(pcoa1,pcoa2,symmetric = T)
summary(pro.g.s)
protest(pcoa1,pcoa2)
plot(pro.g.s, kind = 1,type="text")

Y<-cbind(data.frame(pro.g.s$Yrot),data.frame(pro.g.s$X))
X<-data.frame(pro.g.s$rotation)
Y$sample<-rownames(Y)
Y <- merge(Y, groupata, by = 'sample')
#color
RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set2")
brewer.pal(n=12,name="Set2")
color<-c("#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69")
#if you use the Rcolor brewer,你需要利用scale_color_brewer(palette="paletee_name")
#自選顏色的話用Scale_color_manual(values=color_vector)
#ggplot
p <- ggplot(Y) +
  geom_point(aes(X1, X2, color =location), size = 3, shape = 16) +
  geom_point(aes(Dim1,Dim2,color = location), size = 3, shape = 16) +
  scale_color_manual("Location",values = color) +
  geom_segment(aes(x = X1, y = X2,
                   xend = (X1 + Dim1)/2, yend = (X2 + Dim2)/2),
               # geom_segment 绘制两点间的直线
               arrow = arrow(length = unit(0, 'cm')),
               color = "#8DA0CB", size = 0.75) +
  geom_segment(aes(x = (X1 + Dim1)/2, y = (X2 + Dim2)/2,
                   xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.2, 'cm')),
               color = "#B3B3B3", size =  0.75) +
  theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent')) +
  labs(x = 'Dimension 1', y = 'Dimension 2', color = '') +
  geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_abline(intercept = 0, slope = X[1,2]/X[1,1], size = 0.3) +
  geom_abline(intercept = 0, slope = X[2,2]/X[2,1], size = 0.3) +
  annotate('text', label = sprintf('M^2 == 0.3224'),
           x = -0.2, y = 0.15, size =4, parse = TRUE) +
  annotate('text', label = 'P==0.001',
           x = -0.2, y = 0.135, size = 4, parse = TRUE)+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))
p

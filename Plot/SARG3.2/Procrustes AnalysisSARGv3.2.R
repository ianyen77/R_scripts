library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(RColorBrewer)
dbpata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/bacteria_db/bracken_out/combine_brackenout/combine_bracken_s.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/standard_db/groupdata.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
arg_data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
#dbpata<-dbpata[,-(1:3)]
#groupata<-groupata[-(1:3),]
#arg_data<-arg_data[,-(1:3)]
#這個不一定要，下面這個只是把全部都是0的rows清掉
#dbpata[dbpata<0.0001]<-0
#dbpata<-dbpata[apply(dbpata, 1, function(x) !all(x==0)),]
#dbpata1<-as.data.frame(apply(dbpata,2,function(x) x/sum(x)))
dbpata <-as.data.frame(t(dbpata))
arg_data<-as.data.frame(t(arg_data))
dbpata <- decostand(dbpata, method = 'hellinger')
#arg_data <- decostand(arg_data, method = 'hellinger')
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
Y$location<-factor(Y$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
#color
RColorBrewer::display.brewer.all()
display.brewer.pal(n=12,name="Set3")
brewer.pal(n=12,name="Set3")
color<-c("#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69")
#if you use the Rcolor brewer,你需要利用scale_color_brewer(palette="paletee_name")
#自選顏色的話用Scale_color_manual(values=color_vector)
#ggplot
p <- ggplot(Y) +
  geom_segment(aes(x = X1, y = X2,
                   xend = Dim1, yend = Dim2,color=location),
               # geom_segment 绘制两点间的直线
               size = 0.75,linetype="dashed",alpha=0.7) +
  geom_point(aes(X1, X2, color =location),shape=16,size = 3,alpha=0.5) +
  geom_point(aes(Dim1,Dim2,color = location),shape=17,size = 3,alpha=0.5) +
  scale_color_manual("Location",values = color) +theme_bw() +labs( title="Procrustes analysis")+
  labs(x = 'Dimension 1', y = 'Dimension 2', color = '') +
  geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  annotate('text', label = sprintf('M^2 == 0.154'),
           x = 0.15, y = 0.25, size =4, parse = TRUE) +
  annotate('text', label = 'P==0.001',
           x = 0.15, y = 0.232, size = 4, parse = TRUE)+theme(axis.title = element_text(size=12),legend.title= element_text(size=12),legend.text = element_text(size=12))
p
#Export 7.21 5.73
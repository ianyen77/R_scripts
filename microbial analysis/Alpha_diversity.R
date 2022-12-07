library(vegan)
library(ecodist)
library(ggplot2)
library(openxlsx)
library(ggfortify)
dbpata<-read.xlsx("C:/Users/USER/Desktop/xx.xlsx",sheet=2,rowNames=T,colNames=T,sep.names=" ")
groupata<-read.xlsx("C:/Users/USER/Desktop/test_output/ampliccon sequencing data analysis/group.xlsx",sheet=1,rowNames=T,colNames=T,sep.names=" ")
dbpata1<-dbpata
dbpata1[dbpata1!=0]<-1
observed_species<-apply(dbpata1,1,sum)
a <- NULL
a$observed_species <-observed_species
a<-as.data.frame(a)
shannon<-diversity(dbpata,index="shannon")
simpson<-diversity(dbpata,index="simpson")
chao1<-estimateR(dbpata)
alpha_diversity<-cbind(a,shannon,simpson,chao1=chao1[1,])
alpha_diversity<-cbind(alpha_diversity,plant=groupata$plant,location=groupata$location,sample=groupata$sample_ID)
alpha_diversity<-mutate(alpha_diversity,Pielou_evenness=shannon/log(observed_species))
shannon<-ggplot(alpha_diversity)+
  geom_point(aes(x=sample,y=shannon,group=plant),color="navy")+
  geom_line(aes(x=sample,y=shannon,group=plant),color="navy")+
  facet_wrap(~plant,scales="free_x")+
  theme_bw()+
  labs(title="Shannon")+
  scale_y_continuous(limits = c(1,5))

simpson<-ggplot(alpha_diversity)+
  geom_point(aes(x=sample,y=simpson,group=plant),color="navy")+
  geom_line(aes(x=sample,y=simpson,group=plant),color="navy")+
  facet_wrap(~plant,scales="free_x")+
  theme_bw()+
  labs(title="Simpson")+
  scale_y_continuous(limits=c(0.6,1))

chao1<-ggplot(alpha_diversity)+
  geom_point(aes(x=sample,y=chao1,group=plant),color="navy")+
  geom_line(aes(x=sample,y=chao1,group=plant),color="navy")+
  facet_wrap(~plant,scales="free_x")+
  theme_bw()+
  labs(title="Chao1")+
  scale_y_continuous(limits=c(20,170))

evenness<-ggplot(alpha_diversity)+
  geom_point(aes(x=sample,y=Pielou_evenness,group=plant),color="navy")+
  geom_line(aes(x=sample,y=Pielou_evenness,group=plant),color="navy")+
  facet_wrap(~plant,scales="free_x")+
  theme_bw()+
  labs(title="Pielou_evenness")+
  scale_y_continuous(limits=c(0.4,1))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(shannon,simpson,chao1,evenness)

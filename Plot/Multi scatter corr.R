library(openxlsx)
library(GGally)
library(tidyverse)
data <- read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/mycolicibacterium/Contig taxa conc.xlsx",sheet=2,rowNames = F)
#data transform----------------------------------------
data <-data%>%
  separate(corrtest,into=c("n","type"),sep="__")%>%
  column_to_rownames(var = "type")%>%
  select(-n)%>%
  t()%>%
  as.data.frame()


#single group plot----------------------------
{
lower_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(color="#BEBADA",alpha=0.7,size=2) + 
    geom_smooth(method =lm,alpha=0.3,size=0.7,color="#BEBADA")
  p
}
diag_fn <- function(data, mapping, ...) {
  ggplot(data = data, 
         mapping = mapping) +
    geom_density(...,alpha=0.7,fill="#BEBADA",color="#BEBADA")
}
upper_fn<- function(data, mapping,...) {
  ggally_cor(data = data, 
             mapping = mapping,size=7)
}
}
#ggpairs
x<-ggpairs(data, lower = list(continuous = wrap(lower_fn, method="lm")),
        diag = list(continuous=wrap(diag_fn)),
        upper = list(continuous=wrap(upper_fn)))+
  theme_bw()+
  theme(strip.text.x = element_text ( size = 12),
        strip.text.y = element_text ( size = 12),
        strip.text = element_text(face = "italic"))
x
#ggsave("plot.pdf",x, width = 20, height = 13)
#two group plot-------------------------------------------------------------------------
data$group <-c("Raw","Raw","Raw","DW","DW","DW","DW","DW","DW","DW","DW","DW","DW","DW","DW") 
data<-as.data.frame(data)
data$group<-factor(data$group,levels = c("Raw","DW"))
#plot function
{
  lower_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + 
      geom_point(aes(color=group),alpha=0.7,size=2) + 
      geom_smooth(method =lm,alpha=0.3,size=0.7,aes(color=group),fill="gray")+
      scale_color_manual(values=color)
    p
  }
  diag_fn <- function(data, mapping,group,...) {
    ggplot(data = data, 
           mapping = mapping) +
      geom_density(...,alpha=0.7,aes(fill=group))
  }
  upper_fn<- function(data, mapping,...) {
    ggally_cor(data = data, 
               mapping = mapping)
  }
}
RColorBrewer::brewer.pal(12,"Set3")
color<-c("#80B1D3","#BEBADA" )

#ggpairs

ggpairs(data, lower = list(continuous = wrap(lower_fn, method="lm")),
        diag = list(continuous=wrap(diag_fn))
        ,ggplot2::aes(color=group))+
  theme_bw()+
  scale_fill_manual(values=color)+
  scale_color_manual(values=color)+
  theme(strip.text.x = element_text ( size = 12),
        strip.text.y = element_text ( size = 12),
        strip.text = element_text(face = "italic"))



#multi group plot--------------------------------------------------
data$group <-c("Raw","Raw","Raw","Finished","Finished","Finished","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS") 
data<-as.data.frame(data)
data$group<-factor(data$group,levels = c("Raw","Finished","DWDS"))
#plot function
{
lower_fn <- function(data, mapping, ...){
  p <- ggplot(data = data, mapping = mapping) + 
    geom_point(aes(color=group),alpha=0.7,size=2) + 
    geom_smooth(method =lm,alpha=0.3,size=0.7,aes(color=group),fill="gray")+
    scale_color_manual(values=color)
  p
}
diag_fn <- function(data, mapping,group,...) {
  ggplot(data = data, 
         mapping = mapping) +
    geom_density(...,alpha=0.7,aes(fill=group))
}
upper_fn<- function(data, mapping,...) {
  ggally_cor(data = data, 
             mapping = mapping)
}
}
RColorBrewer::brewer.pal(12,"Set3")
color<-c("#FB8072","#80B1D3","#BEBADA" )

#ggpairs

ggpairs(data, lower = list(continuous = wrap(lower_fn, method="lm")),
        diag = list(continuous=wrap(diag_fn))
        ,ggplot2::aes(color=group))+
  theme_bw()+
  scale_fill_manual(values=color)+
  scale_color_manual(values=color)+
  theme(strip.text.x = element_text ( size = 12),
        strip.text.y = element_text ( size = 12),
        strip.text = element_text(face = "italic"))


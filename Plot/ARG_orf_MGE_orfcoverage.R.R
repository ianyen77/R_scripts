library(openxlsx)
library(tidyverse)
library(ggpubr)
library(Cairo)
##ARG-like ORF coverage
file_path <- "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/indivisual assembly/ARC_sm70/ARC_ORF/ARG_orf_coverage/ARG_lik_orf_coverage/"
file_list <- list.files(path = file_path, pattern = ".xlsx", full.names = TRUE)
df2 <- map_dfr(file_list, ~{
  read.xlsx(.x) %>% 
    select(orf_coverage) %>% 
    summarise(sum_ARG_coverage = sum(orf_coverage)) %>% 
    mutate(filename = str_remove(.x, ".xlsx"))%>%
    mutate(filename = str_replace(filename, ".*/", ""))#%>% 
    #column_to_rownames(var = "filename")
})
##因為有兩個sample 沒有比對到ARG，所以我們手動幫她加入
new_row <- data.frame(sum_ARG_coverage=c(0,0) ,filename =c("T4-W-2ARG_like_orf_cov_all","T4-W-3ARG_like_orf_cov_all"))
df2<- rbind(slice(df2, 1:10), new_row, slice(df2, 11:13))

##MGE-like ORF coverage
file_path <- "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/indivisual assembly/MGE_coverage/MGE_lik_orf_coverage/"
file_list <- list.files(path = file_path, pattern = ".xlsx", full.names = TRUE)

df3 <- map_dfr(file_list, ~{
  read.xlsx(.x) %>% 
    select(orf_coverage) %>% 
    summarise(sum_MGE_coverage = sum(orf_coverage)) %>% 
    mutate(filename = str_remove(.x, ".xlsx"))%>%
    mutate(filename = str_replace(filename, ".*/", ""))#%>% 
  #column_to_rownames(var = "filename")
})
x<-data.frame(ARG=df2$sum_ARG_coverage,MGE=df3$sum_MGE_coverage)
#contig number
x$ncontig<-c(90354,91885,113099,290322,307908,282737,57459,99158,60933,11284,11654,4449,18765,17889,13120)
#orf number
x$norf<-c(145709,150952,189623,443161,448192,432003,100355,144837,144837,31880,23860,9656,57648,51512,38876)
#Normaliztion coverage with contig number
x<-x%>%
  mutate(A_ncontig=ARG/ncontig)%>%
  mutate(M_ncontig=MGE/ncontig)
#sample selection
x1<-x[-(1:6),]
x2<-x[-(1:3),]
ggplot(data = x,aes(x=M_ncontig,y=A_ncontig))+
  geom_point(size=3,alpha=0.7,color=  "#80B1D3")+geom_smooth(color=  "#80B1D3",method =lm,alpha=0.3) +theme_bw()+labs(x="Total MGE-like ORF coverage against contig number(x/GB)",y="Total ARG-like ORF coverage against contig number(x/GB)")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
cor.test(x2$A_ncontig,x2$M_ncontig)
##ggscatter---------------------
ggscatter(data=x2,x="M_ncontig",y="A_ncontig", add = "reg.line", conf.int = TRUE,  color=  "#80B1D3",alpha=0.7,size=3,
          add.params = list(fill = "lightgray"))+stat_cor(method = "pearson", label.x =0.15, label.y = 0.001)+theme_bw()+labs(x="Total MGE-like ORF coverage against contig number(x/GB)",y="Total ARG-like ORF coverage against contig number(x/GB)")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))




x$Type<-c("Raw","Raw","Raw","Finished","Finished","Finished","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS","DWDS")
#x$Type<-rep(c("Raw","Raw","Raw","DW","DW","DW","DW","DW","DW","DW","DW","DW","DW","DW","DW"))
x$Type<-factor(x$Type,levels =c("Raw","Finished","DWDS"))
#x$Type<-factor(x$Type,levels =c("Raw","DW"))
color<-hcl.colors(6,"sunset")
color1<-c("#F9B282","#ED7C97","#704D9E" )
color2<-c("#704D9E","#F9B282")
##Multigroup--- without normalized,without group regression
ggscatter(data=x,x="MGE",y="ARG",color="Type", conf.int = TRUE,alpha=0.7,size=3)+  geom_smooth(method = "lm", color = "grey",alpha=0.2)+
 stat_cor(method = "pearson", label.x = 300,label.y=20)+theme_bw()+labs(x="Total MGE-like ORF coverage (x/GB)",y="Total ARG-like ORF(x/GB)")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))+scale_color_manual(values=color1)

##Multigroup-----without normalized
ggscatter(data=x,x="MGE",y="ARG",fill="Type",color="Type", add = "reg.line", conf.int = TRUE,alpha=0.7,size=3,
          add.params = list(fill = "lightgray"))+stat_cor(aes(color=Type),method = "pearson", label.x = 300)+theme_bw()+labs(x="Total MGE-like ORF coverage (x/GB)",y="Total ARG-like ORF(x/GB)")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))+scale_color_manual(values=color1)


##Multigroup---with normalized,without group regression
ggscatter(data=x,x="M_ncontig",y="A_ncontig",fill="Type",color="Type", conf.int = TRUE, alpha=0.7,size=3,
          add.params = list(fill = "lightgray"))+geom_smooth(method = "lm", color = "grey",alpha=0.2)+stat_cor(method = "pearson")+theme_bw()+labs(x="Total MGE-like ORF coverage against contigs number(x/GB)",y="Total ARG-like ORF coverage against contig number(x/GB)")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))+scale_color_manual(values=color1)

##Multigroup-----with normalized
ggscatter(data=x,x="M_ncontig",y="A_ncontig",fill="Type",color="Type", add = "reg.line", conf.int = TRUE,alpha=0.7,size=3,
          add.params = list(fill = "lightgray"))+stat_cor(aes(color=Type),method = "pearson")+theme_bw()+labs(x="Total MGE-like ORF coverage against contigs number(x/GB)",y="Total ARG-like ORF coverage against contig number(x/GB)")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))+scale_color_manual(values=color1)

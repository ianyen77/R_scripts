library(openxlsx)
library(tidyverse)
library(RColorBrewer)
library("car")
library("FSA")
library("mdthemes")
library(ggpubr)
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/MGE/ARG_OAP v3.2/MGE_ARGoap_3.2out.xlsx",sheet=2,rowNames=T,colNames =T)
data<-as.data.frame(t(data))
data$sum<-apply(data,1,sum)
data$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
data$location<-factor(data$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
varaible_group_mean<-data%>%
  group_by(location)%>%
  summarise(type_mean=mean(sum),type_sd=sd(sum))
#進行pairwise統計
varaible_and_group<-sum~location#想測試的變數跟組別
{{#我們必須先檢查數據是不是常態分布及變異數的同質性，才能決定我們要用的檢定方法。
  #檢查數據是否是常態分布的,利用shapiro.test來檢驗數據是不是常態的，如果p>0.05那麼數據就是常態的
  Data.levels<-split(data, data$location)
  for(i in seq(length(Data.levels))) {
    group.n<-length(Data.levels[[i]]$location)
    group.name <-Data.levels[[i]]$location[1]
    cat(paste("Group: ", group.name, sep=''), sep="", append=TRUE)
    if (group.n < 50) {
      shapiro.result<- shapiro.test(Data.levels[[i]]$sum)
      cat(", Shapiro-Wilk normality test W = ", shapiro.result$statistic, " p-value = ", shapiro.result$p.value, "\n" , sep="")
    } else {
      ks.result<-ks.test(Data.levels[[i]]$sum, pnorm, mean(Data.levels[[i]]$sum), sd(Data.levels[[i]]$sum))
      cat(", Kolmogorov-Smirnov normality test D = ", ks.result$statistic, " p-value = ", ks.result$p.value, "\n" , sep="", append=TRUE)
    }
  }
  #檢查數據變異數的同質性，，如果levenes test 的結果p>0.05，那我們可以認為以這幾個組別間的變異數沒有明顯差異，他們是同質的P
  homo<-leveneTest(varaible_and_group,data = data)
  if (homo$`Pr(>F)`[1]>0.05){
    print("data is homo")
  }else{print ("data is nonhomo")}
  #如果不同質可以用ftest來看是誰不同質
  #res.ftest <- var.test(Data.levels[[1]]$bacitracin,Data.levels[[4]]$bacitracin,data = data)
  #res.ftest
}
#我們必須手動去看是否是常態及同質的，如果兩者皆符合，那我們可以使用t-test
pairwise.t.test(data$sum,data$location,p.adjust.method = "BH")
?pairwise.t.test
#如果兩者中有一不符合，那我們得使用wilcoxon rank sum test
pairwise.wilcox.test(data$sum,data$location,p.adjust.method = "BH")
wilcox.test(Data.levels[[4]]$sum,Data.levels[[1]]$sum,exact = F)

#進行anova
res.aov <- aov(sum ~ location, data = data)
summary(res.aov)
TukeyHSD(res.aov)
plot(res.aov, 2)
leveneTest(sum ~ location, data = data)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
?TukeyHSD()
}
#-------plot
#barplot
varaible_group_mean$location<-factor(varaible_group_mean$location,levels=c("Raw","Finished","Upstream","Midstream","Downstream"))
ggplot(varaible_group_mean)+geom_bar(aes(x=location, y=type_mean), stat="identity",fill="#8DD3C7",alpha=0.7,width=0.8)+geom_errorbar(aes(x=location,ymin=type_mean-type_sd, ymax=type_mean+type_sd), width=.2,position=position_dodge(.9))+
  theme_bw()+ labs(x="Location",y="Total MGEs abundance normalization against 16S rDNA")+theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
#Export width 6.98 height6.72

arg<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet=2,rowNames=T,colNames =T)
arg<-as.data.frame(t(arg))
arg$sum<-apply(arg,1,sum)
arg$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
arg$location<-factor(arg$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
MGE_ARG<-as.data.frame(cbind(as.data.frame(cbind(data$sum,arg$sum)),arg$location))
colnames(MGE_ARG)<-c("MGE_sum","ARG_sum","Location")
#corr plot(未分組)
ggscatter(MGE_ARG,x="MGE_sum",y="ARG_sum", add = "reg.line", conf.int = TRUE,  color=  "#80B1D3",alpha=0.7,size=3,
          add.params = list(fill = "lightgray"))+stat_cor(method = "pearson", label.x =0.2, label.y = 0.1)+theme_bw()+labs(x="Total MGEs abundance against 16S rDNA",y="Total ARGs abundance against 16S rDNA")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
#corr plot (分組，未分組回歸)
ggscatter(MGE_ARG,x="MGE_sum",y="ARG_sum",fill="Type",color="Type", conf.int = TRUE, alpha=0.7,size=3,
          add.params = list(fill = "lightgray"))+geom_smooth(method = "lm", color = "grey",alpha=0.2)+stat_cor(method = "pearson")+theme_bw()+labs(x="Total MGEs abundance against 16S rDNA",y="Total ARGs abundance against 16S rDNA")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))+scale_color_manual(values=color1)
#corr plot(分組)
MGE_ARG$Type<-c("Raw","Raw","Raw","Finished","Finished","Finished",rep("DWDS",9))
ggscatter(MGE_ARG,x="MGE_sum",y="ARG_sum",fill="Type",color="Type", add = "reg.line", conf.int = TRUE,alpha=0.85,size=3,
          add.params = list(fill = "lightgray"))+stat_cor(aes(color=Type),method = "pearson")+theme_bw()+labs(x="Total MGEs abundance against 16S rDNA",y="Total ARGs abundance against 16S rDNA")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))+scale_color_manual(values=color1)
#Export 7.85 6.30
RColorBrewer::brewer.pal(12,"Set3")
color1<-c( "#BEBADA","#FB8072","#80B1D3")
color1<-c("#704D9E" ,"#ED7C97","#F9B282")

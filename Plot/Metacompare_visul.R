library(openxlsx)
library(scatterplot3d)
library(tidyverse)
library(ggplot2)
library("car")
library("FSA")

##Indivisual assembly visulazation-----------------------------------------
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/Metacompare/Metacompare.xlsx",sheet=2,rowNames=F,colNames = T)
data1<-data[,c(9,12,13)]
data1<-log10(data1[,1:3])
data1$Risk.Score<-data$Risk.Score
data1$Risk.Score<-round(data$Risk.Score,1)
color1<-c("#FB8072","#80B1D3","#BEBADA","#FDB462","#B3DE69")
color<-rep(color1,each=3)
color2<-hcl.colors(5,"sunset")
colo2<-rep(color2,each=3)
data1$Location<-data$Location
data1$Location<-factor(data1$Location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))
#hazard space
s3d<-scatterplot3d(data1[,1:3],main="Metacompare risk Matrix Scores",color= color,pch=18,cex.symbols = 2,type="h",grid=T)
text(s3d$xyz.convert(data1[,1:3]),adj=0.2,pos=3,labels =data1[,4],cex= 0.66, col = "black")
legend("bottom",col=color1,legend=levels(data1$Location),pt.bg = color1,pch=18,inset=-0.17,xpd =T,horiz = T)

#bar plot
data_barplot<-data%>%
  group_by(Location)%>%
  summarise(mean=mean(Risk.Score),std=sd(Risk.Score))
data_barplot$Location<-factor(data_barplot$Location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))
bar_plot<-ggplot(data_barplot,aes(x=Location,y=mean))+geom_bar(alpha=0.7,stat = "identity",width = 0.8,fill="#F3E79A")+theme_bw()+geom_errorbar(aes(x=Location,ymin=mean-std, ymax=mean+std), width=.1,position=position_dodge(.9))+labs(x="Location",y="MetaCompare Risk Scores",title = "Individual Assembly Risk Scores")+theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
bar_plot

#lolipop plot
data1$Sample<-data$Sample
ggplot(data1) +
  geom_segment(aes(x=Sample, xend=Sample, y=0, yend=Risk.Score), color="grey")+
  geom_point( aes(x=Sample, y=Risk.Score),color="#704D9E", size=4,alpha=0.5) +
  theme_bw() +
  xlab("Sample") +
  ylab("Metacompare Risk Score")

varaible_and_group<-Risk.Score~Location#想測試的變數跟組別
#我們必須先檢查數據是不是常態分布及變異數的同質性，才能決定我們要用的檢定方法。
{#檢查數據是否是常態分布的,利用shapiro.test來檢驗數據是不是常態的，如果p>0.05那麼數據就是常態的
  Data.levels<-split(data1, data$Location)
  for(i in seq(length(Data.levels))) {
    group.n<-length(Data.levels[[i]]$Location)
    group.name <-Data.levels[[i]]$Location[1]
    cat(paste("Group: ", group.name, sep=''), sep="", append=TRUE)
    if (group.n < 50) {
      shapiro.result<- shapiro.test(Data.levels[[i]]$Risk.Score)
      cat(", Shapiro-Wilk normality test W = ", shapiro.result$statistic, " p-value = ", shapiro.result$p.value, "\n" , sep="")
    } else {
      ks.result<-ks.test(Data.levels[[i]]$Risk.Score, pnorm, mean(Data.levels[[i]]$Risk.Score), sd(Data.levels[[i]]$Risk.Score))
      cat(", Kolmogorov-Smirnov normality test D = ", ks.result$statistic, " p-value = ", ks.result$p.value, "\n" , sep="", append=TRUE)
    }
  }
  #檢查數據變異數的同質性，，如果levenes test 的結果p>0.05，那我們可以認為以這幾個組別間的變異數沒有明顯差異，他們是同質的P
  homo<-leveneTest(varaible_and_group,data = data1)
  if (homo$`Pr(>F)`[1]>0.05){
    print("data is homo")
  }else{print ("data is nonhomo")}
  #如果不同質可以用ftest來看是誰不同質
  #res.ftest <- var.test(Data.levels[[1]]$bacitracin,Data.levels[[4]]$bacitracin,data = data)
  #res.ftest
}
#我們必須手動去看是否是常態及同質的，如果兩者皆符合，那我們可以使用t-test
pairwise.t.test(data1$Risk.Score,data$Location)
?pairwise.t.test
#如果兩者中有一不符合，那我們得使用wilcoxon rank sum test
pairwise.wilcox.test(data$bacitracin,data$location,p.adjust.method = "BH")
wilcox.test(Data.levels[[5]]$bacitracin,Data.levels[[4]]$bacitracin,p.adjust.method = "BH")


##ANOVA
data<-data1
varaible_and_group<-Risk.Score~Location#想測試的變數跟組別

#我們必須先檢查數據是不是常態分布及變異數的同質性，才能決定我們要用的檢定方法。
#檢查數據變異數的同質性，可以使用levenes test
#如果levenes test 的結果p>0.05，那我們可以認為以這幾個組別間的變異數沒有明顯差異，他們是同質的P
{homo<-leveneTest(varaible_and_group,data = data)
  homo
  if (homo$`Pr(>F)`[1]>0.05){
    print("data is homo")
  }else{print ("data is nonhomo")}
  #接著檢查數據是否是常態分布的
  res.aov <- aov(varaible_and_group, data = data)
  plot(res.aov,2)#這個是QQplot 可以透過這張圖來看一下有哪些點可以拿掉。
  aov_residuals <- residuals(object = res.aov )
  #利用shapiro.test來檢驗數據是不是常態的，如果p>0.05那麼數據就是常態的
  a<-shapiro.test(x = aov_residuals)
  if(a["p.value"]>0.05){
    print("data is normal distribution")
  }else{print("data is  not a normal distribution")}
  #如果沒問題(常態且同質)就可以看ANOVA的結果了
  if (a["p.value"]>0.05 && homo$`Pr(>F)`[1]>0.05){
    #如果p<0.05，那表是多組間的差距是不同的，那我們可以用事後檢定來看到底是誰不一樣
    print ("we can use anova ")
    anova_p<-summary(res.aov)
    if (anova_p[[1]]$`Pr(>F)`[1]<0.05){
      print ("anova p<0.05, use TukeyHSD")
      
      TukeyHSD(res.aov) 
    }else{
      print("anova p>0.05,difference is insignificiant")
    }
  }else{
    #數據在anova的兩項假設中有一項不符合，因此我們要使用kruskal-wallis來檢定
    print ("use kruskal wallise rank sum test")
    kruskal_output<-kruskal.test(varaible_and_group, data = data)
    if (kruskal_output$p.value< 0.05){
      #kruskal-wallis 檢定 p <0.05，表示組間有差距，因此我們要使用事後檢定
      #我們可以使用Dunntest來看看是哪一組不同
      PT = dunnTest(varaible_and_group, data = data,
                    method="bh")    # Can adjust p-values; 
      print (PT)
    }else{
      print ("kruskal-wallis p>0.05,difference is insignificiant")
    }
  }
}






#location Co-assembly --------------------------------------
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC_analysis/Metacompare/Metacompare.xlsx",sheet=1,rowNames=F,colNames = T)
data1<-data[,c(9,12,13)]
data1<-log10(data1[,1:3])
data1$Risk.Score<-data$Risk.Score
data1$Risk.Score<-round(data$Risk.Score,1)
color1<-c("#FB8072","#80B1D3","#BEBADA","#FDB462","#B3DE69")
color<-rep(color1,each=3)
color2<-hcl.colors(5,"sunset")
colo2<-rep(color2,each=3)
data1$Location<-data$Location
data1$Location<-factor(data1$Location,levels =c("Raw","Finished","Upstream","Midstream","Downstream"))
#lolipop plot
ggplot(data1) +
  geom_segment(aes(x=Location, xend=Location, y=0, yend=Risk.Score), color="grey")+
  geom_point( aes(x=Location, y=Risk.Score),color="#F3E79A", size=4,alpha=0.8) +geom_text(aes(x=Location, y=Risk.Score+1,label=Risk.Score))+
  theme_bw() +
  xlab("Sample") +
  ylab("Metacompare Risk Score")+
  ggtitle("Location Co-Assembly Metacompare Risk Score")+
  theme(axis.title = element_text(size=13),axis.text =element_text(size=12.5)  ,legend.title= element_text(size=12),legend.text = element_text(size=12))
#3d scatter
s3d<-scatterplot3d(data1[,1:3],main="Metacompare risk Matrix Scores",color= color2,pch=18,cex.symbols = 2,type="h",grid=T)
text(s3d$xyz.convert(data1[,1:3]),adj=0.2,pos=3,labels =data1[,4],cex= 0.66, col = "black")
legend("bottom",col=color2,legend=levels(data1$Location),pt.bg = color1,pch=18,inset=-0.17,xpd =T,horiz = T)

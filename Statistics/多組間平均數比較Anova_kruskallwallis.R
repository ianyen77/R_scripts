#This is a script for testing data normality, homogeneity then choosing the correct method to test if a variable is significantly different in 2 sample
#Author:Ian
#Date;2022/12/03
library("PMCMRplus")
library("dplyr")
library("tidyverse")
library("openxlsx")
library("stringr")
library("car")
library("FSA")
library("RColorBrewer")
library("ggsignif")
library("mdthemes")
data<-read.xlsx(xlsxFile = "C:/Users/TUNG'S LAB/Desktop/統計腳本/練習DATA.xlsx",sheet=3)
#目的:比對某參數在兩個季節採樣間是否有顯著差異(排除原水)
data_S<-filter(data,season=="S")
data_W<-filter(data,season=="W")
data_Y<-filter(data,type=="Y")
data_M<-filter(data,type=="M")
data_T<-filter(data,type=="T")
#我們來測試一下溫度在三水場中有沒有顯著差異

varaible_and_group<-Temperature~type#想測試的變數跟組別

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
    PT = dunnTest(Temperature~type, data = data,
                  method="bh")    # Can adjust p-values; 
    print (PT)
  }else{
    print ("kruskal-wallis p>0.05,difference is insignificiant")
  }
  }
}

#以下開始繪圖，先計算平均跟標準差，其實這部不是必要
varaible_group_mean<-data%>%
  group_by(type)%>%
  summarise(type_mean=mean(Temperature,na.rm=T),type_sd=sd(Temperature))

plot<-data%>%
  ggplot(aes(x=type,y=Temperature,fill=type))+geom_boxplot()+#geom_errorbar(aes(ymin=type_mean-type_sd, ymax=type_mean+type_sd), width=.2,position=position_dodge(.9))+
  scale_fill_brewer(palette="Set3")+theme_bw()+geom_point()+
  theme(panel.background = element_blank(),panel.grid = element_blank(),legend.title = element_text(hjust=0.5))+ylim(18, 35)+
  geom_line(data=tibble(x=c(2,3),y=c(32.3,32.3)),aes(x=x,y=y),inherit.aes = F,size=0.8)+geom_text(data=tibble(x=2.5,y=32.6),aes(x=x,y=y,label="*"),size=5,inherit.aes = F)+
  guides(fill=guide_legend(title="Plant"))+ylab("Temperature (°C)") + xlab("Plant")
  #+geom_line(data=tibble(x=c(1,3),y=c(29,29)),aes(x=x,y=y),inherit.aes = F,size=0.8)+geom_text(data=tibble(x=2,y=29.3),aes(x=x,y=y,label="N.S"),size=5,inherit.aes = F)+
plot

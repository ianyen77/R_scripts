#為了知道中下游的差距是哪裡來的我們必須統計
library("tidyverse")
library("openxlsx")
library("car")
library("FSA")
library("ggsignif")
library("mdthemes")
data_type<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet=2,rowNames = F)
data_subtype<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet=1,rowNames = F)

core_list<-read.csv("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/plot/Venn/SARG v3.3.csv")[,23]
core_list<-core_list[core_list!=""]
data<-data[data$subtype %in% core_list,]
data<-separate(data,subtype,into = c("type","subtype"),sep="__")
#-----------------------------
data<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARG/SARGv3.2/ARG_oap_V3.2out.xlsx",sheet=2,rowNames = T,colNames = T)
#我們先將留下我們想要的DATA，並將組別資訊加到Data中
data<-as.data.frame(t(data))
data$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
data$location<-factor(data$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))

varaible_and_group<-aminoglycoside~location#想測試的變數跟組別
#我們必須先檢查數據是不是常態分布及變異數的同質性，才能決定我們要用的檢定方法。
str(data)
{#檢查數據是否是常態分布的,利用shapiro.test來檢驗數據是不是常態的，如果p>0.05那麼數據就是常態的
  Data.levels<-split(data, data$location)
  for(i in seq(length(Data.levels))) {
    group.n<-length(Data.levels[[i]]$location)
    group.name <-Data.levels[[i]]$location[1]
    cat(paste("Group: ", group.name, sep=''), sep="", append=TRUE)
    if (group.n < 50) {
      shapiro.result<- shapiro.test(Data.levels[[i]]$aminoglycoside)
      cat(", Shapiro-Wilk normality test W = ", shapiro.result$statistic, " p-value = ", shapiro.result$p.value, "\n" , sep="")
    } else {
      ks.result<-ks.test(Data.levels[[i]]$aminoglycoside, pnorm, mean(Data.levels[[i]]$aminoglycoside), sd(Data.levels[[i]]$aminoglycoside))
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
pairwise.t.test(data$aminoglycoside,data$location)
?pairwise.t.test
#如果兩者中有一不符合，那我們得使用wilcoxon rank sum test
pairwise.wilcox.test(data$bacitracin,data$location,p.adjust.method = "BH")
wilcox.test(Data.levels[[5]]$bacitracin,Data.levels[[4]]$bacitracin,p.adjust.method = "BH")



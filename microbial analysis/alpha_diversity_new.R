library(openxlsx)
library(tidyverse)
library(vegan)
library(car)
library("FSA")
data<-read.xlsx("C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/TAXA/rel abundance table/species_rel_table.xlsx",sheet=1,rowNames=F)
rownames(data)<-data$Species
data<-data[,-(1:7)]
data<-as.data.frame(t(data))
#data<-gather(data = datakey="sample",value="abundance",2:16)
richness<-apply(data,1,function(x) sum(x>0))
shannon<-diversity(data,index="shannon")
simpson<-(diversity(data,index="simpson"))
invsimpson<-diversity(data,index = "invsimpson")
Pielou_eveness<-(shannon/log(richness))
alphadiversity<-as.data.frame(cbind(richness,shannon,simpson,invsimpson,Pielou_eveness))
alphadiversity$location<-c("Raw","Raw","Raw","Finished","Finished","Finished","Upstream","Upstream","Upstream","Midstream","Midstream","Midstream","Downstream","Downstream","Downstream")
alphadiversity$location<-factor(alphadiversity$location,levels = c("Raw","Finished","Upstream","Midstream","Downstream"))
alphadiversity$sample<-rownames(alphadiversity)
write.xlsx(alphadiversity,"C:/Users/USER/Desktop/taxa_alphadiversity.xlsx")
alphadiversity$invsimpson<-NULL
#poltdata transformation
alphadiversity_plotdata<-pivot_longer(alphadiversity,cols = c(richness,shannon,simpson,Pielou_eveness),names_to = "index")
alphadiversity_plotdata$index<-factor(alphadiversity_plotdata$index,levels = c("richness","shannon","simpson","Pielou_eveness"),labels=c("Richness","Shannon","Simpson","Pielou_eveness"))
alphadiversity_plotdata_bar<-alphadiversity_plotdata%>%
  group_by(location,index)%>%
  summarise(mean=mean(value),std=sd(value))
color<-c("#FB8072","#BEBADA","#80B1D3","#FDB462","#B3DE69")
#boxplot
ggplot(alphadiversity_plotdata,aes(x=location,y=value,fill=location))+geom_boxplot(alpha=0.7,width = 0.8)+geom_point()+facet_wrap(~index,nrow=2,scales = "free")+theme_bw()+
  scale_fill_manual("Location",values = color)+theme(strip.text = element_text(size = 11))+labs(x="",y="")
#barplot
ggplot(alphadiversity_plotdata_bar,aes(x=location,y=mean,fill=location))+geom_bar(alpha=0.7,stat = "identity",width = 0.8)+facet_wrap(~index,nrow = 2,scales = "free")+theme_bw()+
  scale_fill_manual("Location",values = color)+geom_errorbar(aes(x=location,ymin=mean-std, ymax=mean+std), width=.1,position=position_dodge(.9))+labs(x="",y="")

#statistic
varaible_and_group<-invsimpson~location#想測試的變數跟組別
#我們必須先檢查數據是不是常態分布及變異數的同質性，才能決定我們要用的檢定方法。

{#檢查數據是否是常態分布的,利用shapiro.test來檢驗數據是不是常態的，如果p>0.05那麼數據就是常態的
  Data.levels<-split(alphadiversity, alphadiversity$location)
  for(i in seq(length(Data.levels))) {
    group.n<-length(Data.levels[[i]]$location)
    group.name <-Data.levels[[i]]$location[1]
    cat(paste("Group: ", group.name, sep=''), sep="", append=TRUE)
    if (group.n < 50) {
      shapiro.result<- shapiro.test(Data.levels[[i]]$invsimpson)
      cat(", Shapiro-Wilk normality test W = ", shapiro.result$statistic, " p-value = ", shapiro.result$p.value, "\n" , sep="")
    } else {
      ks.result<-ks.test(Data.levels[[i]]$invsimpson, pnorm, mean(Data.levels[[i]]$invsimpson), sd(Data.levels[[i]]$invsimpson))
      cat(", Kolmogorov-Smirnov normality test D = ", ks.result$statistic, " p-value = ", ks.result$p.value, "\n" , sep="", append=TRUE)
    }
  }
  #檢查數據變異數的同質性，，如果levenes test 的結果p>0.05，那我們可以認為以這幾個組別間的變異數沒有明顯差異，他們是同質的P
  homo<-leveneTest(varaible_and_group,data = alphadiversity)
  if (homo$`Pr(>F)`[1]>0.05){
    print("data is homo")
  }else{print ("data is nonhomo")}
  #如果不同質可以用ftest來看是誰不同質
  #res.ftest <- var.test(Data.levels[[1]]$bacitracin,Data.levels[[4]]$bacitracin,data = data)
  #res.ftest
}
#我們必須手動去看是否是常態及同質的，如果兩者皆符合，那我們可以使用t-test
pairwise.t.test(alphadiversity$invsimpson,alphadiversity$location,p.adjust.method = "BH")
?pairwise.t.test
#如果兩者中有一不符合，那我們得使用wilcoxon rank sum test
pairwise.wilcox.test(alphadiversity$invsimpson,alphadiversity$location,p.adjust.method = "BH",exact=F)
wilcox.test(Data.levels[[5]]$invsimpson,Data.levels[[4]]$invsimpson,p.adjust.method = "BH",exact = F)

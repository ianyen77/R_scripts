library(tidyverse)
library(openxlsx)
filenames<-list.files("~/kraken2_out/bacteria_DB_out/bracken2out/s_out",full.names =T)
#create your own percentage
#df$mypercentage<-df$new_est_reads/sum(df$new_est_reads)
x<-data.frame()
for(i in 1:length(filenames)){
  df <- read.table(filenames[i],sep="\t",header=TRUE)
  df<-df[,c(1,7)]
  colnames(df)[2]<-gsub(".*/","",filenames[i])%>%
    gsub(".S.bracken","",.)
  if(i==1){
    df2<-df
  }
  else{
    df2<-merge(df2,df,by="name",all=T)
  }
}
df2[is.na(df2)]<-0
#remove all o rows
df2<-df2[apply(df2[,2:16], 1, function(x) !all(x==0)),]
#check sum percentage
apply(df2[2:16],2,sum)
write.xlsx(df2,"~/kraken2_out/bacteria_DB_out/bracken2out/combine_bracken_s.xlsx")

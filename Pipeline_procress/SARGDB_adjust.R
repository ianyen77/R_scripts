library("tidyverse")
library("openxlsx")
x1<-read.xlsx(xlsxFile = "C:/Users/USER/Desktop/SARG_Struturelist.xlsx",sheet=1,colNames = T)
x1$Corresponding_ids<-gsub(pattern='\\[',replacement="",x1$Corresponding_ids)
x1$Corresponding_ids<-gsub(pattern='\\]',replacement="",x1$Corresponding_ids)
x1$Corresponding_ids<-gsub(pattern="\\'",replacement="",x1$Corresponding_ids)
x1$Corresponding_ids<-gsub(pattern=" ",replacement="",x1$Corresponding_ids)
c<-separate(x1,Corresponding_ids,into = paste0("COL1_", 1:1000),sep=",")
SARG_DB_adjust<-c%>%
  gather(paste0("COL1_", 1:1000),key="fakename",value="gene",na.rm=T)
SARG_DB_adjust$fakename<-NULL
write.xlsx(SARG_DB_adjust,"C:/Users/USER/Desktop/SARG_Struturelist_adjust.xlsx")
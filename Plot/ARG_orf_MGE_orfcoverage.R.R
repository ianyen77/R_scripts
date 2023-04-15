library(openxlsx)
library(tidyverse)

file_path <- "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/location_co_assembly/ARC_blastx_sm70_cover70_e10/ARC/ARC_ORF/ARC_ORF_SARGreblast/ARG_like_orf_coverage/orf_cover_adj/"

file_list <- list.files(path = file_path, pattern = ".xlsx", full.names = TRUE)
df2 <- map_dfr(file_list, ~{
  read.xlsx(.x) %>% 
    select(orf_adj_coverage) %>% 
    summarise(sum_ARG_coverage = sum(orf_adj_coverage)) %>% 
    mutate(filename = str_remove(.x, ".xlsx"))%>%
    mutate(filename = str_replace(filename, ".*/", ""))#%>% 
    #column_to_rownames(var = "filename")
})


file_path <- "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/location_co_assembly/MGE_coverage/MGE_coverage/MGE_lik_orf_coverage/"

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
ggplot(data = x,aes(x=MGE,y=ARG))+
  geom_point()+theme()



# 对列表中的每个元素求和，并将结果存储在数据框中
df <- data.frame()
for (i in colnames(my_list)){
  sum_x<-sum(my_list[[i]])
  row<-data.frame(sum=sum_x, colnames=i)
  df<- bind_rows(df, row)
  
}

my_list <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))

my_list <- data.frame(a = c(1, 2, 3), b = c(4, 5, 6), c = c(7, 8, 9))

dfx <- map_dfr(my_list, ~{
  sum_x <- sum(.x)
  data.frame(sum_orf = sum_x)
}, .id = "filename")

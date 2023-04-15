library(openxlsx)
library(tidyverse)
library(purrr)
# 設定檔案路徑及檔案格式
file_path <- "C:/Users/USER/Desktop/lab/實驗/Metagenomic in DWDS/DATA/newDATA/ARC/location_co_assembly/ARC_blastx_sm70_cover70_e10/ARC/ARC_ORF/ARC_ORF_SARGreblast/ARG_like_orf_coverage/orf_cover_adj/"

# 取得檔案清單
file_list <- list.files(path = file_path, pattern = ".xlsx", full.names = TRUE)
df<-data.frame()
###以下這個有問題，會多此一舉
df <- NULL  # 先定義一個空的資料框
for (i in seq_along(file_list)) {
  if (i == 1) {
    # 讀取第一個檔案的全部內容，包括第一列
    x <- read.xlsx(file_list[[i]], startRow = 1)
    colnames(x) <- colnames(x)  # 將欄位名稱修改為原始值
  } else {
    # 讀取後續檔案，跳過第一列
    x <- read.xlsx(file_list[[i]], startRow = 2,colNames = F)
    colnames(x) <- colnames(df)  # 將欄位名稱修改為前一個資料框的欄位名稱
  }
  df <- rbind(df, x)  # 將讀取到的資料框合併至 df 中
}

###以下這個有問題，會少row
# 使用 map_dfr 函數將所有excel檔案合併成一個df
df <- map_dfr(file_list, ~{
  # 讀取excel檔案，跳過第一列
  read_excel(.x, skip = ifelse(.x == file_list[1], 0, 1)) %>%
    # 重命名列名稱
    setNames(col_names)
})
df<-data.frame()
for(i in file_list){
  x<-read.xlsx(i)
  df<-rbind(df,x)
} 

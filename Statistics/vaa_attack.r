# 加载 quantmod 包
library(quantmod)
library(TTR)


stocklist<-c("SPY","EFA","EEM","AGG","LQD","IEF","SHY")

monthly_returns_list <- list()
price_df <- data.frame()
#取得過去月份的的資料
for (x in stocklist){

  print(x)
  

  stock_data <- getSymbols(x, from = Sys.Date() - 500, to = Sys.Date(), auto.assign = FALSE)
  

  stock_xts <- as.xts(stock_data)
  temp_data<-stock_xts[,4]
  if (nrow(price_df) == 0) {
    price_df <- temp_data
  } else {
    price_df <- cbind(price_df,temp_data)
  }
  price_df<-as.data.frame(price_df)
  #monthly_returns <- periodReturn(stock_xts, period = 'monthly', type = 'arithmetic', leading = TRUE)
  
  #monthly_returns_list[[x]] <- monthly_returns
}

#進行時間的處理
price_df$DATE<-rownames(price_df)
price_df$DATE<-as.Date(price_df$DATE)

#設定要計算的時間區間
latest_date <-max(price_df$DATE)
one_month_ago <- latest_date - 30
three_months_ago <- latest_date - 90
six_months_ago <- latest_date - 180
one_year_ago <- latest_date - 365
#寫一個計算報酬率的函數
calculate_return <- function(x, start_date, end_date) {
  start_price <- x[x$DATE == start_date, ]
  end_price <- x[x$DATE== end_date,]
  return_rate <- (end_price - start_price)/start_price
  return(return_rate)
}
peroid_date<-c(one_month_ago,three_months_ago,six_months_ago,one_year_ago)
###----------------
#開始建立函數
returns_df<-data.frame()

for(x in peroid_date){
if (x%in% price_df$DATE){
  temp_returns<-calculate_return(price_df,x,latest_date)
  if (nrow(returns_df) == 0) {
    returns_df <- temp_returns
  } else {
    returns_df <- rbind(returns_df, temp_returns)}
  }else{
  #直接找最接近的日期
  nearest_trade_date_index<- findInterval(x,price_df$DATE)
  print("nearest DATE")
  print(nearest_date<-price_df$DATE[nearest_trade_date_index])
  #找最接近的日期但小於給定日期
  print("nearest but smaller DATE")
  print(nearest_date_smaller<- max(price_df$DATE[price_df$DATE < x]))
  if (nearest_date== nearest_date_smaller) {
    temp_returns<-calculate_return(price_df,nearest_date,latest_date)
    if (nrow(returns_df) == 0) {
      returns_df <- temp_returns
    } else {
      returns_df <- rbind(returns_df, temp_returns)}}
  else{
      temp_returns<-calculate_return(price_df,nearest_date_smaller,latest_date)
      if (nrow(returns_df) == 0) {
        returns_df <- temp_returns
      } else {
        returns_df <- rbind(returns_df, temp_returns)}
    }
}
}
returns_df$DATE<-NULL
rownames(returns_df)<-c("1month_return","3month_return","6month_return","12month_return")

momentum_score<-returns_df[1,]*12+returns_df[2,]*4+returns_df[3,]*2+returns_df[4,]*1

rownames(momentum_score)[1]<-"momentum_score"

#看目前市場狀況要投資攻擊型，還是防守型資產。
if (sum(momentum_score[1,1:4]>0)==4){
  print("動能足夠，投資攻擊資產動能最高者")
  print (c("攻擊型資產動能",momentum_score[1,1:4]))
}else{
  print("動能不足，投資防守型資產動能最高者")
  print (c("防守型資產動能",momentum_score[1,5:7]))
}

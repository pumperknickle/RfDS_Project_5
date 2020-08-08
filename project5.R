library(quantmod)
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(dplyr)
getSymbols("SPY")
ema <- EMA(SPY[,c("SPY.Close")], n = 100 )

stock_prices <- c("SPY") %>%
        tq_get(get = "stock.prices",
                            from = "1990-07-31",
               period = "monthly")

stock_returns <- stock_prices %>%
        group_by(symbol) %>%
        tq_transmute(select = adjusted,
                                        mutate_fun = periodReturn,
                                        period = "monthly",
                                        col_rename = "value")

portfolio_value <- 0
deposits <- seq(from = 1000, to = 332000, by = 1000)

performance.1 <- data.frame(matrix(ncol = 1, nrow = 0), stringsAsFactors =  TRUE)
x <- c("value")
colnames(performance.1) <- x

for (i in 1:nrow(stock_returns)){
  portfolio_value <- portfolio_value + (stock_returns[i, "value"]*portfolio_value) + 1000
  
  performance.1 <- rbind(performance.1, c( portfolio_value))
}

performance.1.returns <- cbind(stock_returns[,"date"], performance.1[[1]] - deposits)
performance.1 <- cbind(stock_returns[,"date"], performance.1)


portfolio_value <- 0

performance.2 <- data.frame(matrix(ncol = 1, nrow = 0), stringsAsFactors =  TRUE)
x <- c("value")
colnames(performance.2) <- x

for (i in 1:nrow(stock_returns)){
  if (is.na(ema[i, "EMA"]) | 
      (stock_prices[which(stock_prices$date == stock_returns[i,"date"]), "close"] 
       > ema[i, "EMA"])){
    portfolio_value <- portfolio_value + (stock_returns[i, "value"]*portfolio_value) + 1000
  }
  else {
    portfolio_value <- portfolio_value + 1000
  }
  
  performance.2 <- rbind(performance.2, c( portfolio_value))
}

performance.2.returns <- cbind(stock_returns[,"date"], performance.2[[1]] - deposits)
performance.2 <- cbind(stock_returns[,"date"], performance.2)

df1 <- data.frame(dates=performance.1[[1]], total_return=performance.1[[2]])
df2 <- data.frame(dates=performance.2[[1]], total_return=performance.2[[2]])

df3 <- df1 %>%  mutate(Type = 'EMA TTR') %>% bind_rows(df2 %>% mutate(Type = 'S&P Monthly'))

p <- ggplot(df3,aes(y = total_return,x = dates,color = Type)) + 
  geom_line() +
  ggtitle("Total USD Returns over Time")

print("S&P Monthly")
print(df1, row.names=FALSE)
print("EMA TTR")
print(df2, row.names=FALSE)


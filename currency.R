#File for currency calculations
#Packages
install.packages("tidyverse")
install.packages("ggthemes")

#libraries
library(ggplot2)
library(dplyr)
library(ggthemes)

#read csv - remember to change wd using setwd()
bitcoin_dataset <- read.csv("bitcoin_dataset.csv")

#change data (imported manually) into tbl
bitcoin <- tbl_df(bitcoin_dataset)

#Covert date string to Date type
bitcoin <- mutate(bitcoin, Date = as.Date(Date,format = '%m/%d/%Y'))
View(bitcoin)

#calculate return on price

bitcoin <- mutate(bitcoin, return=(btc_market_price-lag(btc_market_price))/lag(btc_market_price)*100, daily_new_coins=btc_total_bitcoins-lag(btc_total_bitcoins))

View(bitcoin)

tail(bitcoin$return, n= 50)

#extract all useful columns for currency analysis
bitcoin_currency <- bitcoin %>% 
  select("Date", "btc_market_price", "return", "btc_total_bitcoins", "daily_new_coins", "btc_n_transactions",
         "btc_n_transactions_total", "btc_n_transactions_excluding_popular", "btc_n_transactions_per_block",
         "btc_n_transactions_excluding_chains_longer_than_100", "btc_market_cap", 
         "btc_trade_volume", "btc_median_confirmation_time", "btc_hash_rate", "btc_output_volume",
         "btc_estimated_transaction_volume", "btc_estimated_transaction_volume_usd")%>%
  filter(Date >= "2013-04-02")

View(bitcoin_currency)

#simple plots
ggplot(bitcoin_currency, aes(x = Date, y = btc_market_price)) + geom_line() 
ggplot(bitcoin_currency, aes(x = Date, y = btc_total_bitcoins)) + geom_line() 
ggplot(bitcoin_currency, aes(x = Date, y = btc_n_transactions_total)) + geom_line() 


#convert varibles to logs

log_btc_market_price <- log (bitcoin_currency$btc_market_price)
log_return <- log (bitcoin_currency$return)
log_btc_total_bitcoins <- log (bitcoin_currency$btc_total_bitcoins)
log_daily_new_coins <- log (bitcoin_currency$daily_new_coins)
log_btc_trade_volume <- log (bitcoin_currency$btc_trade_volume)
log_btc_n_transactions <- log (bitcoin_currency$btc_n_transactions)
log_btc_n_transactions_total <- log (bitcoin_currency$btc_n_transactions_total)


#new dataframe with log variables

log_var <- data.frame(bitcoin_currency$Date,log_btc_market_price,bitcoin_currency$return,log_btc_total_bitcoins,log_daily_new_coins,log_btc_trade_volume,log_btc_n_transactions,log_btc_n_transactions_total)


#add dummy variable for new_coins
log_var <- mutate(log_var, dummy = 0)

for (i in 599:nrow(log_var)){
  log_var[i, "dummy"] <- 1 
}

View(log_var)

#Plots of log variables
ggplot(log_var, aes(x = bitcoin_currency$Date, y = log_btc_market_price)) + geom_line() 
ggplot(log_var, aes(x = bitcoin_currency$Date, y = bitcoin_currency$return)) + geom_line() 
ggplot(log_var, aes(x = bitcoin_currency$Date, y = log_btc_total_bitcoins)) + geom_line() 
ggplot(log_var, aes(x = bitcoin_currency$Date, y = log_daily_new_coins)) + geom_line() 
ggplot(log_var, aes(x = bitcoin_currency$Date, y = log_btc_trade_volume)) + geom_line() 
ggplot(log_var, aes(x = bitcoin_currency$Date, y = log_btc_n_transactions)) + geom_line() 
ggplot(log_var, aes(x = bitcoin_currency$Date, y = log_btc_n_transactions_total)) + geom_line() 

#multiple regression
reg_1 <- lm(log_btc_market_price ~ log_btc_n_transactions_total + log_btc_total_bitcoins, data=log_var)
summary(reg_1)

reg_1.res <- residuals(reg_1)
reg_1.predict <- predict(reg_1)

reg_1.res
reg_1.predict
log_btc_market_price

comparison <- data.frame(bitcoin_currency$Date,log_btc_market_price,reg_1.predict,reg_1.res)

head(comparison,n=10)

ggplot(comparison, aes(x=comparison$bitcoin_currency.Date,y=log_btc_market_price))+geom_point() + geom_point(aes(y = reg_1.predict, color=reg_1.predict), shape = 1)


reg_2 <- lm(log_btc_market_price ~ log_btc_n_transactions_total + log_btc_total_bitcoins + dummy, data=log_var)
summary(reg_2)

reg_3 <- lm(log_btc_market_price ~ log_btc_n_transactions + log_daily_new_coins, data=log_var)
summary(reg_3)

reg_4 <- lm(log_btc_market_price ~ log_btc_n_transactions + log_daily_new_coins + dummy, data=log_var)
summary(reg_4)

#Explanation: btc_market_price = 9.246e+01 + (-1.902e-05) * btc_total_bitcoins + 8.751e-06  * btc_n_transactions_total
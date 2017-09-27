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

#extract all useful columns for currency analysis
bitcoin_currency <- bitcoin %>% 
  select("Date", "btc_market_price", "btc_total_bitcoins", "btc_n_transactions",
                           "btc_n_transactions_total", "btc_n_transactions_excluding_popular", "btc_n_transactions_per_block",
                           "btc_n_transactions_excluding_chains_longer_than_100", "btc_market_cap", 
                           "btc_trade_volume", "btc_median_confirmation_time", "btc_hash_rate", "btc_output_volume",
                           "btc_estimated_transaction_volume", "btc_estimated_transaction_volume_usd")%>%
  filter(Date >= "2013-04-02")
          
View(bitcoin_currency)
 
#simple plots
ggplot(bitcoin, aes(x = Date, y = btc_market_price)) + geom_line() 
ggplot(bitcoin, aes(x = Date, y = btc_total_bitcoins)) + geom_line() 
ggplot(bitcoin, aes(x = Date, y = btc_n_transactions_total)) + geom_line() 

#multiple regression
a = lm(btc_market_price ~ btc_total_bitcoins + btc_n_transactions_total, data = bitcoin)
summary(a)
#Explanation: btc_market_price = 9.246e+01 + (-1.902e-05) * btc_total_bitcoins + 8.751e-06  * btc_n_transactions_total
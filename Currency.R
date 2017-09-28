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

#Calculate new variables for analysis, add this to bitcoin dataframe, 1. price return  called "btc_price_return" and 2. coin supply daily as "btc_daily_new_coins"
#New variables are calculated and added to the "bitcoin" dataframe 

bitcoin <- mutate(bitcoin, btc_price_return=(btc_market_price-lag(btc_market_price))/lag(btc_market_price)*100, btc_daily_new_coins=btc_total_bitcoins-lag(btc_total_bitcoins))

#View structure of new dataframe with new variables
str(bitcoin)

#Extract all useful columns from "bitcoin" dataframe for data analysis, place these new columns and with selected date periods into a dataframe called "bitcoin_variables"
bitcoin_variables <- bitcoin %>% 
  select("Date", "btc_market_price", "btc_price_return", "btc_total_bitcoins", "btc_daily_new_coins", "btc_n_transactions",
         "btc_n_transactions_total", "btc_n_transactions_excluding_popular", "btc_n_transactions_per_block",
         "btc_n_transactions_excluding_chains_longer_than_100", "btc_market_cap", 
         "btc_trade_volume", "btc_median_confirmation_time", "btc_hash_rate", "btc_output_volume",
         "btc_estimated_transaction_volume", "btc_estimated_transaction_volume_usd")%>%
  filter(Date >= "2013-04-02")

#View the structure of the new dataframe "bitcoin_variables"
str(bitcoin_variables) 

#Plot potential variables for analysis in "bitcoin_variables", this will inform us of the variables' current functional form
ggplot(bitcoin_variables, aes(x = Date, y = btc_market_price)) + geom_line()
ggplot(bitcoin_variables, aes(x = Date, y = btc_total_bitcoins)) + geom_line() 
ggplot(bitcoin_variables, aes(x = Date, y = btc_daily_new_coins)) + geom_line() 
ggplot(bitcoin_variables, aes(x = Date, y = btc_n_transactions)) + geom_line() 
ggplot(bitcoin_variables, aes(x = Date, y = btc_n_transactions_total)) + geom_line() 

#Tranform key variables in "bitcoin_variables" to natural logarithms i.e. to linearise our variables for modelling
log_btc_market_price <- log (bitcoin_variables$btc_market_price)
log_btc_total_bitcoins <- log (bitcoin_variables$btc_total_bitcoins)
log_btc_daily_new_coins <- log (bitcoin_variables$btc_daily_new_coins)
log_btc_n_transactions <- log (bitcoin_variables$btc_n_transactions)
log_btc_n_transactions_total <- log (bitcoin_variables$btc_n_transactions_total)

#Log variables are now added to a new dataframe called "bitcoin_variables_log" which can be used as the basis for regression
bitcoin_variables_log <- data.frame(bitcoin_variables$Date,log_btc_market_price,bitcoin_variables$btc_price_return,log_btc_total_bitcoins,log_btc_daily_new_coins,log_btc_n_transactions,log_btc_n_transactions_total)

#Plots of log variables
ggplot(bitcoin_variables_log, aes(x = bitcoin_variables$Date, y = log_btc_market_price)) + geom_line() 
ggplot(bitcoin_variables_log, aes(x = bitcoin_variables$Date, y = log_btc_total_bitcoins)) + geom_line() 
ggplot(bitcoin_variables_log, aes(x = bitcoin_variables$Date, y = log_btc_daily_new_coins)) + geom_line() 
ggplot(bitcoin_variables_log, aes(x = bitcoin_variables$Date, y = log_btc_n_transactions)) + geom_line() 
ggplot(bitcoin_variables_log, aes(x = bitcoin_variables$Date, y = log_btc_n_transactions_total)) + geom_line() 

#Multiple regressions
#Regression 1 (reg_1) market price on transactions and total bitcoin supply with intercept
reg_1 <- lm(log_btc_market_price ~ log_btc_n_transactions_total + log_btc_total_bitcoins, data=bitcoin_variables_log)
summary(reg_1)

install.packages("stargazer")
library(stargazer)
stargazer(reg_1, type = "text", title="Regression Results", digits = 2)

reg_1.res <- residuals(reg_1)
reg_1.predict <- predict(reg_1)

#log_btc_market_price

comparison <- data.frame(bitcoin_variables$Date,log_btc_market_price,reg_1.predict,reg_1.res)

comparison_2 <- comparison %>%
  filter(bitcoin_variables.Date >= "2015-01-02")

View(comparison_2)

ggplot(comparison_2, aes(x=bitcoin_variables.Date,y=log_btc_market_price))+geom_point(colour="black", shape=21, fill="orange")+geom_point(aes(y=reg_1.predict),colour="Red",shape=22,size=-1)

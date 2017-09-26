#File for drivers calculations
#Packages
install.packages("tidyverse")
install.packages("ggthemes")

#libraries
library(ggplot2)
library(dplyr)
library(ggthemes)

#load the dataset
bitcoin_dataset <- read.csv('bitcoin_dataset.csv')

#change data (imported manually) into tbl
data <-tbl_df(bitcoin_dataset)

#Covert date string to Date type
data <-mutate(data,Date =as.Date(Date,format='%m/%d/%Y' ))

#subset data: only use data from Apri1 1st, 2013
btc_data <- data[776:nrow(data),]
View(btc_data)


#Plot theme ---- use '+theme_publicaton' with ggplot call
theme_Publication <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line.x = element_line(colour="black"),
            axis.line.y = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}

#Price over time
price_over_time<-ggplot(data=btc_data, aes(x=Date, y=btc_market_price ))+ 
  geom_line() +theme_Publication() +xlab("Date") +ylab("Price $")+ ggtitle('Price over time')
price_over_time


#return over time
btc_data <-mutate(btc_data,return = (btc_market_price-lag(btc_market_price))/lag(btc_market_price)*100)

return_over_time<-ggplot(data=btc_data, aes(x=Date, y=return))+ 
  geom_line() +theme_Publication() +xlab("Date") +ylab("Return %")+ ggtitle('Return over time')
return_over_time

#Price vs number of bitcoins
price_v_total <-
ggplot(data = btc_data) +
  geom_point(aes(x =( btc_total_bitcoins/1000000),y = btc_market_price )) +
  ylab( "Price") +
  xlab( "Total number of bitcoins (millions)") +ggtitle('Price vs Supply of bitcoins') +theme_Publication()
price_v_total
#Correlation Price vs number of bitcoins
cor.test(btc_data$btc_market_price,btc_data$btc_total_bitcoins)

#Price vs Mining difficulty
price_v_mining <-
  ggplot(data = btc_data) +
  geom_point(aes(x =( btc_difficulty/1000000),y = btc_market_price )) +
  ylab( "Price") +
  xlab( "Relative mining difficulty") +ggtitle('Price vs Mining difficulty') +theme_Publication()
price_v_mining
#Correlation Price vs Mining difficulty
cor.test(btc_data$btc_market_price,btc_data$btc_difficulty)


#Number of bitcoins over time
total_over_time <-
  ggplot(data = btc_data) +
  geom_point(aes(x =( btc_total_bitcoins/1000000),y = Date )) +
  ylab( "Time") +
  xlab( "Total number of bitcoins (millions)") +ggtitle('Supply of bitcoins over time') +theme_Publication()
total_over_time


#Mining difficulty over time
mining_over_time <-
  ggplot(data = btc_data) +
  geom_point(aes(x =( btc_difficulty/1000000),y = Date )) +
  ylab( "Time") +
  xlab( "Relative mining difficulty") +ggtitle('Mining difficulty over time') +theme_Publication()
mining_over_time

#return density plot
return_density_plot <- ggplot(btc_data,aes(x=return))+geom_density()+theme_Publication()+xlab('Return')+ylab('Density')+
  ggtitle('Price Return Density Plot')
return_density_plot

#descriptive statistics of return 
summary(btc_data[,'return'])
mean(btc_data$return,na.rm=TRUE)
median(btc_data$return,na.rm=TRUE)
sd(btc_data$return,na.rm=TRUE)
var(btc_data$return,na.rm=TRUE)

#daily confirmed transaction volume time series plot
#Do not use it 
daily_transactions_over_time <- ggplot(btc_data,aes(x=Date,y=btc_n_transactions))+geom_line()+theme_Publication()+xlab('Date')+ylab('Daily Confirmed Transactions')+
  ggtitle('Daily Confirmed Transactions Over Time')
daily_transactions_over_time

#total transactions volume time series plot
total_transactions_over_time <- ggplot(btc_data,aes(x=Date,y=btc_n_transactions_total))+geom_line()+theme_Publication()+xlab('Date')+ylab('Total Transactions')+
  ggtitle('Total Transactions Over Time')
total_transactions_over_time

#scatter plot price vs daily confirmed transactions
#Do not use it
price_v_daily_confirmed_transactions <- ggplot(btc_data,aes(y=btc_market_price,x=btc_n_transactions))+geom_point()+theme_Publication()+xlab('Daily Confirmed Transactions')+ylab('Price')+ggtitle('Price vs Daily Confirmed Transactions')
price_v_daily_confirmed_transactions

#scatter plot price vs total transactions
price_v_total_transactions <- ggplot(btc_data,aes(y=btc_market_price,x=btc_n_transactions_total))+geom_point()+theme_Publication()+xlab('Total Transactions')+ylab('Price')+ggtitle('Price vs Total Transactions')
price_v_total_transactions


#correlation test: price with daily confirmed transactions
#Do not use it
cor.test(btc_data$btc_market_price,btc_data$btc_n_transactions)

#correlation test:price with total transactions
cor.test(btc_data$btc_market_price,btc_data$btc_n_transactions_total)


#File for asset calculations
#requires Mark's section to be run first to get the dataset imported


#Packages
install.packages("tidyverse")
install.packages("ggthemes")

#libraries
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(lubridate)
library(tidyr)


#Importing FX CSV and removing repeated date column
FX <- read_csv("FX.csv")
fx <- select(FX, Date, CNYUSD, JPYUSD, GBPUSD, EURUSD)

#Importing Gold Dataset
gold <- read_csv("Gold Series Data.csv")
names(gold)[2] <- 'goldprice'

#Formatting Date column, then merging datasets, removing NA observations and selecting required variables
fx$Date <- dmy(fx$Date)
gold$Date <- dmy(gold$Date)
fxdata <- btc_data %>% left_join(fx, by='Date') %>% left_join(gold, by='Date')
fxdata <- fxdata %>% select(Date, btc_market_price, JPYUSD, CNYUSD, EURUSD, GBPUSD, goldprice) %>% filter(!is.na(JPYUSD))

#Defining a function to calculate returns and using it to create columns for returns
#Returns have to be recalculated as some prices have been removed
gen_ret <- function(colname) {
  (colname - lag(colname)) / lag(colname) * 100
}
fxdata <- mutate(fxdata, btcret = gen_ret(btc_market_price), cnyret = gen_ret(CNYUSD), eurret = gen_ret(EURUSD), jpyret = gen_ret(JPYUSD), gbpret = gen_ret(GBPUSD), goldret = gen_ret(goldprice))


#Correlation table
corr_table<-fxdata %>% select(btcret, gbpret, jpyret, eurret, cnyret, goldret) %>% cor(use = 'complete.obs')
library(reshape2)
melted_currency <- melt(corr_table)

ggplot(data = melted_currency, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low = "blue", high = "red")+
  ylab( "") +
  xlab( "") +ggtitle('Correlation Heatmap') 

#New Long dataset and Scatter against BTC: Note the lack of relationship in all the charts
fxdata3 <- fxdata %>% select(Date, btcret, gbpret, jpyret, eurret, cnyret, goldret) %>% gather(curr, ret, -Date, -btcret)
ggplot(fxdata3, aes(x = btcret)) + geom_point(aes(y=ret)) + facet_wrap(~curr) + xlim(-5, 5) + ylim(-5, 5) +
theme_Publication() +
  ylab( "Returns") + xlab( "Bitcoin Returns") +ggtitle('Bitcoin returns vs other currencies') 
rm(fxdata3)

#New Long dataset and Scatter against EUR: Note the relationships between EURJPY and GBPEUR pairs. 
#CNY has a somewhat insignificant relationship, possibly explained by Chinese currency controls. Also explains extremely low volatility
fxdata3a <- fxdata %>% select(Date, btcret, gbpret, jpyret, eurret, cnyret, goldret) %>% gather(curr, ret, -Date, -eurret)
ggplot(fxdata3a, aes(x = eurret)) + geom_point(aes(y=ret)) + facet_wrap(~curr) + xlim(-5, 5) + ylim(-5, 5) +
  theme_Publication() +
  ylab( "Returns") + xlab( "Euro Returns") +ggtitle('Euro returns vs other currencies') 
rm(fxdata3a)

#Scatterplot of JPY vs Gold and BTC: JPY has positive relationship with Gold that should be visible in BTC if it was a safe haven currency
fxdata3b <- fxdata %>% select(Date, btcret, gbpret, jpyret, eurret, cnyret, goldret) %>% gather(curr, ret, -Date, -jpyret)
ggplot(fxdata3b, aes(x = jpyret)) + geom_point(aes(y=ret)) + facet_wrap(~curr) + xlim(-5, 5) + ylim(-5, 5) +
  theme_Publication() +
  ylab( "Returns") + xlab( "JPY Returns") +ggtitle('JPY returns vs other currencies') 
rm(fxdata3b)




#Density plots of returns (Left out CNY as its variance is too small)
fxdata3b <- fxdata %>% select(Date, btcret, gbpret, jpyret, eurret, goldret) %>% gather(curr, ret, -Date)
ggplot(fxdata3b, aes(x = ret)) + geom_density(aes(colour = curr)) + xlim(-7.5, 7.5) +
  ylab( "Returns") + xlab( "Currency") +ggtitle('Density of returns')+
  theme_Publication()
rm(fxdata3b)


#Calculate t-test on Hypothesis: mean = 0 
mean0 <- function(colname) {
  round(as.numeric(t.test(colname)[3]), 3)
}
pvmean0 <- c('BTC' = mean0(fxdata$btcret),
             'EUR' = mean0(fxdata$eurret),
             'GBP' = mean0(fxdata$gbpret),
             'JPY' = mean0(fxdata$jpyret),
             'CNY' = mean0(fxdata$cnyret),
             'Gold' = mean0(fxdata$goldret))

#Calculate t-test on Hypothesis: mean1 = mean2 
meaneq <- function(colname) {
  round(as.numeric(t.test(fxdata$btcret, colname)[3]), 3)
}

pvmeaneq <- c('BTC' = meaneq(fxdata$btcret),
              'EUR' = meaneq(fxdata$eurret),
              'GBP' = meaneq(fxdata$gbpret),
              'JPY' = meaneq(fxdata$jpyret),
              'CNY' = meaneq(fxdata$cnyret),
              'Gold' = meaneq(fxdata$goldret))




#boxplot of returns (Can we draw a line across boxplot at mean of btcret?)
fxdata3a <- fxdata %>% select(Date, btcret, gbpret, jpyret, eurret, cnyret, goldret) %>% gather(curr, ret, -Date)
ggplot(fxdata3a, aes(x=curr)) + geom_boxplot(aes(y=ret)) + ylim(-5, 5)+ theme_Publication() +
  theme_Publication()+
  ylab( "Returns") + xlab( "Currency") +ggtitle('Mean of returns')
rm(fxdata3a)










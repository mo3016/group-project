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

#a ggplot test
ggplot(data, aes(x = Date, y = btc_market_price)) + geom_line()
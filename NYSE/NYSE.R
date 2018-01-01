# NYSE Analysis

##Changing the Working Directory

setwd('./Kaggle/NYSE')

## Loading the Libraries

library(Quandl)
library(ggplot2)
library(readr)
library(dplyr)
library(quantmod)

## Reading the Data

nyse<-read.table("./prices.csv",header = TRUE,sep=",") #importing data into R
head(nyse) #finding structure of the stock
unique(nyse$symbol) #found it has 501 unique stocks


## Picking Apple

aapl <- subset(nyse,symbol=="AAPL") #extracting only Apple

aapl[1:3,]
close<-aapl$close #creating vector called close
close[1:3] 
plot(close, type="l",xlab = "days",ylab = "Closing Price",main="Plotting Line Chart of Apple Stock") #plotting a line chart
returns <- (close[1:(length(close)-1)]-close[2:length(close)])/close[2:length(close)] #calculating arthmatic daily return
plot(returns,type = "l",xlab = "AAPL",main="Plotting daily returns of AAPL Stock") #plotting daily return chart
macd <- MACD(close,nFast=12,nSlow=26,nSig = 9,maType = SMA,percent = FALSE) #MACD calculation
ohlc <- aapl[c("date","open","high","low","close","volume")] #OHLC dataframe as the excel data cant be used to create a candlestick chart
head(ohlc)



## Picking RL

RL <- subset(nyse,symbol=="RL") #extracting only Apple

RL[1:3,]
close <- RL$close #creating vector called close
close[1:3] 
plot(close, type="l",xlab = "days",ylab = "Closing Price",main="Plotting Line Chart of RL Stock") #plotting a line chart
returns <- (close[1:(length(close)-1)]-close[2:length(close)])/close[2:length(close)] #calculating arthmatic daily return
plot(returns,type = "l",xlab = "RL",main="Plotting daily returns of RL Stock") #plotting daily return chart
macd <- MACD(close,nFast=12,nSlow=26,nSig = 9,maType = SMA,percent = FALSE) #MACD calculation
ohlc <- RL[c("date","open","high","low","close","volume")] #OHLC dataframe as the excel data cant be used to create a candlestick chart
head(ohlc)



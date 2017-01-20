# Melbourne Housing Market

## Importing the Libraries
library(ggplot2)
library(dplyr)
library(plyr)
library(scales)
library(readr)

## Getting the Dataset
housing <- read.csv('./Melbourne Housing.csv')
head(housing)
summary(housing)

## Checking for NA Values
any(is.na(housing))


housing_filter <- filter(housing,Price!="NA")
n <- length(housing_filter$Price)
maxprice<-sort(housing_filter$Price,partial=n-9)[n-9]
minprice<-sort(housing_filter$Price,partial=10)[10]
housing_maxfilter <- filter(housing_filter,Price>=maxprice)
housing_minfilter <- filter(housing_filter,Price<=minprice)

## Exploratory Data Analysis
ggplot(housing_maxfilter,aes(Suburb,Price))+geom_bar(stat = "identity") + scale_y_continuous(labels=comma)
ggplot(housing_minfilter,aes(Suburb,Price))+geom_bar(stat = "identity") + scale_y_continuous(labels=comma)
qplot(Price,data=housing_filter,geom = "freqpoly",bins=50) + scale_x_continuous(labels=comma) 

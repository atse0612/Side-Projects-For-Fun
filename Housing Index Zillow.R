# Zillow Housing Index From 2010

### Changing the Working Directory

setwd('./Kaggle/Zillow')

### Loading the Libraries

library(forecast)
library(zoo)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(dplyr)
library(readr)

### Reading the Datasets

rent <- read.csv('./price.csv')
sqft <- read.csv('./pricepersqft.csv')

### Looking at the Top 10

values=head(rent,10)
values=data.frame(t(as.matrix(values[,7:81])))
colnames(values)=rent[1:10,2]


### Monthly Percentage Change (Seattle)

suppressMessages(library(quantmod))

pct_change <- function(rent) {
  
  nc <- ncol(rent)
  ln <- colnames(rent)
  
  meta <- rent[c(1:6)]
  data <- rent[c(7:nc)]
  
  data <- t(apply(data, 1, Delt))
  
  rv <- cbind(meta, data)
  colnames(rv) <- ln
  
  rv[-7]
}

# Select data for the Seattle, WA metro region.
# Total of 98 places.
pc <- subset(rent, rent$Metro == 'Seattle')
pc <- pct_change(pc)

last = ncol(pc)

pc <- pc[order(pc[last], decreasing = TRUE),]
pc <- cbind(pc[c(2,5)], round(pc[(last-3):last], 3))

# Top 10 places in the Seattle region with the
# highest most recent monthly percentage change.
head(pc, n=10)


### Monthly Percentage Change (San Francisco)

pct_change <- function(rent) {
  
  nc <- ncol(rent)
  ln <- colnames(rent)
  
  meta <- rent[c(1:6)]
  data <- rent[c(7:nc)]
  
  data <- t(apply(data, 1, Delt))
  
  rv <- cbind(meta, data)
  colnames(rv) <- ln
  
  rv[-7]
}

# Select data for the San Francisco, CA metro region.

pc <- subset(rent, rent$Metro == 'San Francisco')
pc <- pct_change(pc)

last = ncol(pc)

pc <- pc[order(pc[last], decreasing = TRUE),]
pc <- cbind(pc[c(2,5)], round(pc[(last-3):last], 3))

# Top 10 places in the San Francisco region with the
# highest most recent monthly percentage change.
head(pc, n=10)



### Yearly Percentage Change (Sacramento)

get_range <- function(rent) {
  last = ncol(rent)
  n <- colnames(rent)
  val <- length(n[7:last])
  val <- round(val/12)
  rv <- seq(last - val * 12, last, 12)
  rv
}

# Select data for the Sacramento, CA metro region.
# Total of 55 places.
pc <- subset(rent, rent$Metro == 'Sacramento')

years <- get_range(pc)
pc <- cbind(pc[1:6], pc[years])

# use function defined above
pc <- pct_change(pc)

last = ncol(pc)

pc <- pc[order(pc[last], decreasing = TRUE),]
pc <- cbind(pc[c(2,5)], round(pc[(last-3):last], 2))

# Top 10 places in the Sacramento region with the
# highest most recent yearly percentage change.
head(pc, n=10)


### Yearly Percentage Change (San Francisco)

get_range <- function(rent) {
  last = ncol(rent)
  n <- colnames(rent)
  val <- length(n[7:last])
  val <- round(val/12)
  rv <- seq(last - val * 12, last, 12)
  rv
}

# Select data for the San Francisco, CA metro region.
pc <- subset(rent, rent$Metro == 'San Francisco')

years <- get_range(pc)
pc <- cbind(pc[1:6], pc[years])

# use function defined above
pc <- pct_change(pc)

last = ncol(pc)

pc <- pc[order(pc[last], decreasing = TRUE),]
pc <- cbind(pc[c(2,5)], round(pc[(last-3):last], 2))

# Top 10 places in the San Francisco region with the
# highest most recent yearly percentage change.
head(pc, n=10)



### Index Numbers (Los Angeles)

index_base_100 <- function(rent) {
  
  nc <- ncol(rent)
  ln <- colnames(rent)
  
  meta <- rent[, c(1:6)]
  data <- rent[7:nc]
  base <- rent[7]
  
  index <- function(x) {
    x / base
  }
  
  data <- apply(data, 2, index)
  data <- data.frame(data)
  
  data <- data * 100
  data <- round(data)
  
  rv <- cbind(meta, data)
  colnames(rv) <- ln
  
  rv
}

# Select data for the Los Angeles, CA metro region.
# Total of 148 places.
# Base: November 2010 = 100
idx <- subset(rent, rent$Metro == 'Los Angeles')
idx <- index_base_100(idx)

last = ncol(idx)
s <- seq(last-36, last, 12)

idx <- idx[order(idx[last], decreasing = TRUE),]
idx <- cbind(idx[c(2,5)], idx[s])

# The top 10 places in the Los Angeles metro region
# with the largest index change over the base period.
head(idx, n=10)

#### San Francisco Region for Index Numbers

index_base_100 <- function(rent) {
  
  nc <- ncol(rent)
  ln <- colnames(rent)
  
  meta <- rent[, c(1:6)]
  data <- rent[7:nc]
  base <- rent[7]
  
  index <- function(x) {
    x / base
  }
  
  data <- apply(data, 2, index)
  data <- data.frame(data)
  
  data <- data * 100
  data <- round(data)
  
  rv <- cbind(meta, data)
  colnames(rv) <- ln
  
  rv
}

# Select data for the San Francisco, CA metro region.
# Base: November 2010 = 100
idx <- subset(rent, rent$Metro == 'San Francisco')
idx <- index_base_100(idx)

last = ncol(idx)
s <- seq(last-36, last, 12)

idx <- idx[order(idx[last], decreasing = TRUE),]
idx <- cbind(idx[c(2,5)], idx[s])

# The top 10 places in the San Francisco metro region
# with the largest index change over the base period.
head(idx, n=10)




### Top 10 Cities By Population Using Time-Series Analysis

date <- seq(as.Date("2010/11/01"), as.Date("2017/01/31"),"month")
date <- as.yearmon(date)
ts=zoo(values,order.by = date)
values=fortify(ts)
values$Index=as.Date(values$Index)

autoplot(ts,facet=NULL)+
  theme_minimal()+
  labs(x="Time",y="Price")

forecasts=matrix(,ncol=10,nrow=11)

for(i in 1:10){
  
  forecasts[,i]=forecast(auto.arima(ts[,i],lambda = 0,stepwise = F),h=11)$mean
  
}

colnames(forecasts) = rent[1:10,2]
results=rbind(values[,2:11],forecasts)
date_2 <- seq(as.Date("2010/11/01"), as.Date("2017/12/31"),"month")
date_2 <- as.yearmon(date_2)
results=zoo(results,order.by = date_2)
autoplot(results,facet=NULL)+
  theme_minimal()+
  labs(x="Time",y="Price")+
  geom_vline(aes(xintercept=2017),size=0.2)

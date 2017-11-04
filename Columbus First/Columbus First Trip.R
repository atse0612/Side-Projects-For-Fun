# Columbus First Trip


## Loading the Libraries

library(data.table)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(tm)
library(SnowballC)
library(RSentiment)
library(stringr)
library(SnowballC)
library(RWeka)
library(DT)
library(gdata)

## Reading the Dataset and Changing Working Directory

setwd('./Kaggle/Columbus First')

columbus <- read.csv('./Columbus.csv', encoding = "UTF-8")
str(columbus)
summary(columbus)

## Polishing Up


columbus$month <- as.factor(columbus$month)
columbus$month <- factor(columbus$month,levels(columbus$month)[c(1,8,7,6,2,4,3,5)]) #Reorder Levels
columbus$nmonth <- columbus$month
levels(columbus$nmonth) <- c("08","09","10","11","12","01","02","03")
columbus$day <- as.factor(columbus$day)
columbus$year <- as.factor(columbus$year)
columbus$nwords <- sapply(gregexpr("[A-z]\\W+", columbus$text), length) + 1L # Number of words in the text
columbus$date <- paste(columbus$day,"-",columbus$nmonth,"-",columbus$year,sep = "")
columbus$date <- as.POSIXct(strptime(columbus$date,format = "%e-%m-%Y"))

## Calculate Sentiments

corpus = Corpus(VectorSource(list(columbus$text)))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers) 
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, removeWords, stopwords('en'))


dtm_colon = DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
freq_colon <- colSums(as.matrix(dtm_colon))

sentiments_colon = calculate_sentiment(names(freq_colon)) 
sentiments_colon = cbind(sentiments_colon, as.data.frame(freq_colon))

sent_pos_colon = sentiments_colon[sentiments_colon$sentiment == 'Positive',]
sent_neg_colon = sentiments_colon[sentiments_colon$sentiment == 'Negative',]
sent_neu_colon = sentiments_colon[sentiments_colon$sentiment == 'Neutral',]

cat("We have more positive Sentiments: ",sum(sent_pos_colon$freq_colon)," than negative: ",sum(sent_neg_colon$freq_colon))

# Airbnb


## Loading the Libraries

library(data.table)
library(ggplot2)
library(lubridate)
library(wordcloud)
library(tm)
library(SnowballC)
library(RSentiment)
library(caTools)
library(randomForest)
library(rpart)
library(rpart.plot)


## Reading the Dataset and Changing Working Directory

setwd('./Kaggle/Airbnb Texas')
airbnb <- read.csv('./Airbnb_Texas_Rentals.csv')

## Converting Factors to Integers

airbnb$average_rate_per_night <- as.integer(airbnb$average_rate_per_night)
airbnb$bedrooms_count <- as.integer(airbnb$bedrooms_count)

## Changing the Description

airbnb$description <- as.character(airbnb$description)

## Checking for Changes being Made

str(airbnb)
summary(airbnb)

## Using the tapply Function

tapply(airbnb$average_rate_per_night, airbnb$city, mean)


## Subsetting on Dallas

dallas <- subset(airbnb, airbnb$city == "Dallas")

## Visualization on Dallas

dallaslocation <- as.data.frame(table(round(dallas$latitude,2),round(dallas$longitude,2)))
dallaslocation$Var1 <- as.numeric(as.character(dallaslocation$Var1))
dallaslocation$Var2 <- as.numeric(as.character(dallaslocation$Var2))
ggplot(dallaslocation, aes(x=Var1, y=Var2))+geom_tile(aes(fill=Freq))+scale_fill_gradient(low="green", high="blue")

### There are only a few areas in Dallas that has more than 30 listings that are open.


## Creating a Linear Regression Model

set.seed(13265)
spl = sample.split(dallas$average_rate_per_night, 0.8) # Train at 80%
train <- subset(dallas, spl == TRUE)
test <- subset(dallas, spl == FALSE)
dal_model <- lm(average_rate_per_night ~ bedrooms_count + latitude + longitude, data=train)
summary(dal_model)



## Calculating the MSE (Mean Square Error)

predictlm <- predict(dal_model, newdata = test)
mselm <- mean((predictlm-test$average_rate_per_night)^2)
mselm

## Regression Tree Model

cart1 <- rpart(average_rate_per_night ~ bedrooms_count + latitude + longitude, data=train, method = "anova")
predictcart <- predict(cart1, newdata = test)
msecart <- mean((predictcart-test$average_rate_per_night)^2)
msecart


prp(cart1)

## Random Forest Model

forest <- randomForest(average_rate_per_night ~ bedrooms_count + latitude + longitude, data=train)
predictrf <- predict(forest, newdata=test)
mserf <- mean((predictrf-test$average_rate_per_night)^2)
mserf


## Cleaning the Words

corpus <- Corpus(VectorSource(dallas$description))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stemDocument)


freq <- DocumentTermMatrix(corpus)
freq

## Removing Sparse Terms

freq <- removeSparseTerms(freq, 0.995)

dallas_sparse <- as.data.frame(as.matrix(freq))
colnames(dallas_sparse) <- make.names(colnames(dallas_sparse))
dallas_sparse$average_rate <- dallas$average_rate_per_night

## Performing Random Forest Model Again

set.seed(2562)
spl2 <- sample.split(dallas_sparse$average_rate, SplitRatio = 0.8) # Training Set at 80%
train2 <- subset(dallas_sparse, spl2==TRUE)
test2 <- subset(dallas_sparse, spl2==FALSE)

newforest <- randomForest(average_rate~., data=train2)
summary(newforest)
predictrf2 <- predict(newforest, newdata=test2)
mserf2 <- mean((predictrf2-test2$average_rate)^2)
mserf2


## Combining to One Model

dallas_sparse$bedroom <- dallas$bedrooms_count
dallas_sparse$latitude <- dallas$latitude
dallas_sparse$longitude <- dallas$longitude

set.seed(10241)
spl3 <- sample.split(dallas_sparse$average_rate, SplitRatio = 0.8) # Training at 80%
train3 <- subset(dallas_sparse, spl3=TRUE)
test3 <- subset(dallas_sparse, spl3==FALSE)
allforest <- randomForest(average_rate~., data=train3)
predictrf3 <- predict(allforest, newdata=test3)
mse_all <- mean((predictrf3-test3$average_rate)^2)
mse_all

## Wording Visualization

vu <- varUsed(allforest, count=TRUE)
vusorted <- sort(vu, decreasing=FALSE, index.return=TRUE)
dotchart(vusorted$x, names(allforest$forest$xlevels[vusorted$ix]))



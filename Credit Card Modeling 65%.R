# Credit Card Predictive Modeling

### Load Libraries

library(randomForest)
library(e1071)
library(rpart)
library(rpart.plot)
library(caTools)
library(readr)
library(caret)

# Receiving Dataset and Change Working Directory
setwd('./Kaggle')
ccard <- read_csv("./creditcard.csv")
head(ccard)

str(ccard)

## Predictive Modeling

### Set Data 65:35

set.seed(22540)
split <- sample.split(ccard$Class, SplitRatio = 0.65)
train <- subset(ccard, split == T)
cv <- subset(ccard, split == F)

### check output Class distributiion
table(cv$Class)

# Logistic Regression

glm.model <- glm(Class ~ ., data = train, family = "binomial")
glm.predict <- predict(glm.model, cv, type = "response")
table(cv$Class, glm.predict > 0.5)

# Decision Tree Model

tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 50)
prp(tree.model)

tree.predict <- predict(tree.model, cv, type = "class")
confusionMatrix(cv$Class, tree.predict)


# Keeping 10,000 Rows with Class=0

data.class.0 <- subset(ccard, ccard$Class == 0)
data.class.1 <- subset(ccard, ccard$Class == 1)
nrow(data.class.0)

nrow(data.class.1)

data.class.0 <- data.class.0[1:10000, ]
nrow(data.class.0)

data <- rbind(data.class.0, data.class.1)
nrow(data)

# Split Data 65:35

set.seed(205)
split <- sample.split(data$Class, SplitRatio = 0.65)
train <- subset(data, split == T)
cv <- subset(data, split == F)

table(cv$Class)

# Logistic Regression for Split

glm.model <- glm(Class ~ ., data = train, family = "binomial", control = list(maxit = 50))
glm.predict <- predict(glm.model, cv, type = "response")
table(cv$Class, glm.predict > 0.5)

# SVM Model

svm.model <- svm(Class ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.3)
svm.predict <- predict(svm.model, cv)
confusionMatrix(cv$Class, svm.predict)

# Decision Tree Split

tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 10)
prp(tree.model) 

tree.predict <- predict(tree.model, cv, type = "class")
confusionMatrix(cv$Class, tree.predict)


# Random Forest Predictions

set.seed(10)
rf.model <- randomForest(Class ~ ., data = train,
                         ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, cv)
confusionMatrix(cv$Class, rf.predict)


varImpPlot(rf.model)

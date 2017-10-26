# Caravan Analysis

## Loading the Libraries

library(ggplot2)
library(readr)
library(Amelia)
library(data.table)
library(RColorBrewer)
library(mlbench)
library(DMwR)
library(caret)
library(pROC)



## Changing the Working Directory

setwd('./Kaggle/Caravan')

## Reading the Dataset

cvan <- fread("./caravan-insurance-challenge.csv")
dim(cvan)
head(cvan)

## Creating the Train Test Split Dataset

train <- cvan[ORIGIN == "train", ]
test <- cvan[ORIGIN != "train", ]

## Putting Them on Tables

ftable(train[, CARAVAN])
prop.table(ftable(train[, CARAVAN]))


tr_outcome <- train[, CARAVAN]

tr_feat <- train[,  !"CARAVAN"]

test_outcome <- test[, CARAVAN]
test_features <- test[, !"CARAVAN"]

## Looking at the Missing Map

missmap(train)


## Looking at the Plots Between MGODRK and Caravan

ggplot(train[, .N, by = list(MGODRK, CARAVAN)], 
  aes(x = MGODRK, y = N, fill = factor(CARAVAN),
  color = factor(CARAVAN), alpha = .3  )) + 
  geom_bar(stat = "identity")


## Looking at the Plots Between MGODOV and Caravan

ggplot(train[, .N, by = list(MGODOV, CARAVAN)], 
       aes(x = MGODOV, y = N, fill = factor(CARAVAN), 
       color = factor(CARAVAN), alpha = .3 )) + 
       geom_bar(stat = "identity")


## Gathering the Train Table

train <- train[, Religion := ifelse(MGODRK == 1, "R", 
                                    (ifelse(MGODPR == 1, "P", 
                                            ifelse(MGODOV == 1, "O","N"))))]

ftable(train[, MRELGE])


## MRELGE Plot

ggplot(train[, .N, by = MRELGE], 
       aes(x= factor(MRELGE), y = N, 
           color = factor(MRELGE), fill = factor(MRELGE), 
           alpha = 0.3, size = N)) + 
           geom_point()


## Looking At Variables Close to Zero

nearzero_train <- nearZeroVar(train, saveMetrics = TRUE, 
                              freqCut=  95/5)
nearzero_train


## Looking at the Origin Variable

train <- train[, ORIGIN := NULL]
test <- test[, ORIGIN := NULL]


## Outcomes and Features of Origin 

outcome.train <- train[, CARAVAN]

features.train <- train[,  !"CARAVAN"]

outcome.test <- test[, CARAVAN]
features.test <- test[, !CARAVAN]


train <- train[, CARAVAN := ifelse(CARAVAN == 0, "No", "Yes")]
test  <- test[, CARAVAN := ifelse(CARAVAN ==  0, "No", "Yes")]


## Train Table for Caravan
ftable(train[, CARAVAN])

## Test Table for Caravan
ftable(test[, CARAVAN])

## Gathering the Smote Information

train <- train[, CARAVAN := factor(CARAVAN)]
train <- train[, Religion := NULL]
trsmote <- SMOTE(CARAVAN ~ MHKOOP , perc.over = 200, perc.under = 150,
                     data = train,
                     k = 10)


dim(trsmote)

## Comparing Smote to Original

ftable(train[, CARAVAN])

ftable(trsmote[, CARAVAN])

## Running Train Control

trCtrl <- trainControl(method = "repeatedcv", repeats = 3, 
                     summaryFunction=twoClassSummary , classProbs = TRUE, verbose =  1)

control <- rfeControl(functions=rfFuncs, method="cv", number=10)

## Boosted Trees

bst.grid <- expand.grid(mstop = 50, 
                        maxdepth = 12)

bstFit <- train(CARAVAN~., data = trsmote, 
                trControl = trCtrl, 
                method = "blackboost",
                tuneGrid = bst.grid, 
                metric = "ROC")

## Creating the Best Fit

top <- varImp(bstFit, scale = FALSE)
print(top)


plot(top)

## Predicted Values Boosted Trees

bstFit


## Blackboost Predictions for Train

blackboost_pred <- data.frame(predict(bstFit))

blackboost_pred_prob <- predict(bstFit, type = "prob")


ggplot(blackboost_pred, aes(x =  blackboost_pred_prob$Yes, 
                            fill = trsmote[, CARAVAN], 
                            colour = trsmote[, CARAVAN], 
                            alpha = 0.3)) + geom_density()

## Blackboost Predictions for Test

blackboost_pred_test <- data.frame(predict(bstFit, newdata = test))

blackboost_pred_prob_test <- predict(bstFit, newdata = test, type = "prob")

ggplot(blackboost_pred_test, aes(x =  blackboost_pred_prob_test$Yes, 
                                 fill = test[, CARAVAN], 
                                 colour = test[, CARAVAN], 
                                 alpha = 0.3)) + geom_histogram()


## Confusion Matrix

confusionMatrix(blackboost_pred_test$predict.bstFit..newdata...test., test[, CARAVAN])

## Test Class

bst_test_class <- ifelse(blackboost_pred_test$predict.bstFit..newdata...test.== "Yes", 1, 0)
test_class <- ifelse(test[, CARAVAN] == "Yes", 1, 0)

roc(test_class, bst_test_class)

## Using XG Boost

xgb.grid <- expand.grid(nrounds = 3, 
                        max_depth = 12, 
                        eta = 0.1, 
                        gamma = 1, 
                        colsample_bytree = 1, 
                        min_child_weight = 1, 
                        subsample = 0.75)
xgbFit <- train(CARAVAN ~., 
                data = trsmote, 
                method = "xgbTree", 
                metric = "ROC", 
                trControl = trCtrl, 
                tuneGrid = xgb.grid)


## Getting the Results and Plotting It

xgbFit$results


ggplot(xgbFit$results, aes(x =  eta, y = ROC, 
                           fill = ROC, size = ROC, 
                           color =  factor(gamma))) + geom_point()


## Predicted Values

xgbFit.pred <- predict(xgbFit)
xgbFit.pred.prob <- predict(xgbFit, type = "prob")


xgbFit.pred <- data.frame(xgbFit.pred)


xgbFit.pred.test <- predict(xgbFit, newdata = test)
xgbFit.pred.test.prob <- predict(xgbFit, newdata = test, type = "prob")


xgbFit.pred.test <- data.frame(xgbFit.pred.test)
colnames(xgbFit.pred.test)


## Plotting the Values

ggplot(xgbFit.pred.test, aes(x =  xgbFit.pred.test.prob$Yes, 
                             fill = test[, CARAVAN], 
                             color = test[, CARAVAN], 
                             alpha = 0.3)) + geom_histogram()


## Gathering the Confusion Matrix

xgb_test_class <- ifelse(xgbFit.pred.test$xgbFit.pred.test== "Yes", 1, 0)

confusionMatrix(xgb_test_class, test_class)

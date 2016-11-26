## Heart Disease Decision Trees
# Loading the data

library(FFTrees)
FFTrees.guide()
data(heartdisease)
head(heartdisease)
summary(heartdisease)

# Heart Disease Data
set.seed(100)
samples <- sample(c(T, F), size = nrow(heartdisease), replace = T)
heartdisease.train <- heartdisease[samples,]
heartdisease.test <- heartdisease[samples == 0,]
heart.FFTrees <- FFTrees(formula = diagnosis ~., data = heartdisease.train,data.test = heartdisease.test)
print(heart.FFTrees)
class(heart.FFTrees)
names(heart.FFTrees)
heart.FFTrees$cue.accuracies

# ROC Plot
showcues(heart.FFTrees,  main = "Heartdisease Cue Accuracy")

# Stats
heart.FFTrees$FFTrees.stats
summary(heart.FFTrees) 

# Area Under the Curve
heart.FFTrees$auc

# Train Decision DF

heart.FFTrees$decision.train[1:5,]
heart.FFTrees$levelout.train[1:5,]

# Selecting Cues and Plotting Trees
heart.as.FFTrees <- FFTrees(formula = diagnosis ~ age + sex, data = heartdisease)
plot(heart.FFTrees, main = "Heart Disease",decision.names = c("Healthy", "Disease"))
plot(heart.FFTrees, main = "Heart Disease",decision.names = c("Healthy", "Disease"), train.p = 5)

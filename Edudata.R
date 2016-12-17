
---
## Students' Academic Performance

# Change Directory  
setwd("./Kaggle")
  
# Load Libraries
library(ggplot2)
library(dplyr)
library(randomForest)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(caTools)
library(party)


# Reading the Data

edu <- read.csv('./Edudata.csv')
str(edu)
summary(edu)

### Exploratory Data Analysis

# Raised Hands

ggplot(edu, aes(x = raisedhands)) + geom_histogram(bins=50, color = "red",fill="blue",alpha=0.2) +
  scale_x_continuous(breaks = seq(0,100,5)) + 
  labs(x = "Raised Hands", y = "Student Count")


# Visited Resources

ggplot(edu, aes(x = VisITedResources)) + geom_histogram(bins=50, color = "orange",fill="orange",alpha=0.4) +
  scale_x_continuous(breaks = seq(0,100,5)) + 
  labs(x = "Visited Resources", y = "Student Count")


# Announcements

ggplot(edu, aes(x = AnnouncementsView)) + geom_histogram(bins = 50,color = "black",fill="red",alpha=0.5) +
  scale_x_continuous(breaks = seq(0,100,5)) + 
  labs(x = "Announcements View", y = "Student Count")


# Discussion

ggplot(edu, aes(x = Discussion)) + geom_histogram(bins=50,color = "black",fill="grey") +
  scale_x_continuous(breaks = seq(0,100,5)) + 
  labs(x = "Discussion Participation", y = "Student Count")


### Barplots

ggplot(edu, aes(x = gender)) + geom_bar(aes(fill=gender)) + 
  labs(x = "Gender", y = "Student Count") +
  scale_y_continuous(breaks = seq(0,300,30)) + coord_flip()



ggplot(edu, aes(x = NationalITy)) + geom_bar(aes(fill=NationalITy)) + 
  labs(x = "Nationality", y = "Student Count") +
  scale_y_continuous(breaks = seq(0,200,20)) + coord_flip()



ggplot(edu, aes(x = PlaceofBirth)) + geom_bar(aes(fill = NationalITy)) + 
  labs(x = "Birth Place", y = "Student Count") + coord_flip() # usa is a mix of nationalities



ggplot(edu, aes(x = GradeID, fill = Class)) + geom_bar() + 
  labs(x = "Grade ID", y = "Student Count") + coord_flip() # g-06 has students with only low grades



ggplot(edu, aes(x = GradeID, fill = gender)) + geom_bar() + 
  labs(x = "Grade ID", y = "Student Count") + coord_flip() # g-10 has no females



ggplot(edu, aes(x = SectionID, fill = Topic,alpha=0.1)) + geom_bar() +
  labs(x = "Section ID", y = "Student Count") +
  coord_flip()



ggplot(edu, aes(x = Topic, fill = gender,alpha=0.2)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") +
  scale_y_continuous(breaks = seq(0,100,4)) + coord_flip()



ggplot(edu, aes(x = Topic, fill = NationalITy)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") + coord_flip() +
  scale_y_continuous(breaks = seq(0,100,4)) + coord_flip()


ggplot(edu, aes(x = Topic, fill = SectionID,alpha=0.3)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") + coord_flip() +
  scale_y_continuous(breaks = seq(0,100,4))



# Section C for Mostly Spanish Students

ggplot(edu, aes(x = Topic, fill = Semester)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") + coord_flip() +
  scale_y_continuous(breaks = seq(0,100,4))



# IT Students Are Mostly in 1st Semester

ggplot(edu, aes(x = Topic, fill = Relation,alpha=0.5)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") + coord_flip() +
  scale_y_continuous(breaks = seq(0,100,4))


# Most French Students have Mom as Guardian in Comparison to Father


ggplot(edu, aes(x = Topic, fill = Class)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") + coord_flip() +
  scale_y_continuous(breaks = seq(0,100,4))


ggplot(edu, aes(x = Topic, fill = Class)) + geom_bar(position = "fill") +
  labs(x = "Topic", y = "Student Count") + coord_flip() +
  scale_y_continuous(breaks = seq(0,100,4))


# Geology has no low class students


ggplot(edu, aes(x = Semester)) + geom_bar(aes(fill=Semester)) + 
  labs(x = "Semester", y = "Student Count")

ggplot(edu, aes(x = Relation, fill = Semester)) + geom_bar() +
  labs(x = "Guardian", y = "Student Count")

ggplot(edu, aes(x = ParentAnsweringSurvey, fill = ParentschoolSatisfaction)) +
  geom_bar() + 
  labs(x = "Does parents answer surveys ?", y = "Student Count")

# Parent Satisfaction

ggplot(edu, aes(x = ParentschoolSatisfaction)) +
  geom_bar(aes(fill=ParentschoolSatisfaction)) + 
  labs(x = "Are the Parents Satisfied With the School ?", y = "Student Count")



ggplot(edu, aes(x = StudentAbsenceDays)) + geom_bar(aes(fill=StudentAbsenceDays)) + 
  labs(x = "Is the student absent for more than seven days", y = "Student Count")

ggplot(edu, aes(x = Class, fill = gender)) + geom_bar() +
  labs(x = "Class", y = "Student Count")

# Few Girls in the Low Class

ggplot(edu, aes(x = Class, fill = Relation)) + geom_bar() +
  labs(x = "Class", y = "Student Count")


ggplot(edu, aes(x = Class, fill = ParentAnsweringSurvey)) + geom_bar() +
  labs(x = "Class", y = "Student Count")


ggplot(edu, aes(x = Class, fill = StudentAbsenceDays)) + geom_bar() +
  labs(x = "Class", y = "Student Count")


### Boxplots


ggplot(edu, aes(x = gender, y = raisedhands)) + geom_boxplot(aes(fill=gender))



ggplot(edu, aes(x = gender, y = VisITedResources)) + geom_boxplot(aes(fill=gender))


# Girls Use More Resources

ggplot(edu, aes(x = NationalITy, y = raisedhands)) + geom_boxplot(aes(fill=NationalITy))

ggplot(edu, aes(x = StageID, y = raisedhands)) + geom_boxplot(aes(fill=StageID))


ggplot(edu, aes(x = StageID, y = Discussion)) + geom_boxplot(aes(fill=StageID))

ggplot(edu, aes(x = GradeID, y = raisedhands)) + geom_boxplot(aes(fill=GradeID))

ggplot(edu, aes(x = SectionID, y = Discussion)) + geom_boxplot(aes(fill=SectionID))

ggplot(edu, aes(x = Topic, y = raisedhands)) + geom_boxplot(aes(fill=Topic))

ggplot(edu, aes(x = Semester, y = raisedhands)) + geom_boxplot(aes(fill=Semester))

ggplot(edu, aes(x = Relation, y = raisedhands)) + geom_boxplot(aes(fill=Relation))

ggplot(edu, aes(x = ParentAnsweringSurvey, y = raisedhands)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))

ggplot(edu, aes(x = ParentAnsweringSurvey, y = VisITedResources)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))

ggplot(edu, aes(x = ParentAnsweringSurvey, y = AnnouncementsView)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))

ggplot(edu, aes(x = ParentAnsweringSurvey, y = Discussion)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))

ggplot(edu, aes(x = ParentschoolSatisfaction, y = raisedhands)) + geom_boxplot(aes(fill=ParentschoolSatisfaction))

ggplot(edu, aes(x = ParentschoolSatisfaction, y = VisITedResources)) + geom_boxplot(aes(fill=ParentschoolSatisfaction))

ggplot(edu, aes(x = ParentschoolSatisfaction, y = AnnouncementsView)) + geom_boxplot(aes(fill=ParentschoolSatisfaction))

ggplot(edu, aes(x = ParentschoolSatisfaction, y = Discussion)) + geom_boxplot(aes(fill=ParentschoolSatisfaction))

ggplot(edu, aes(x = StudentAbsenceDays, y = raisedhands)) + geom_boxplot(aes(fill=StudentAbsenceDays))

# More Students Leave = Less Hand Raises


ggplot(edu, aes(x = StudentAbsenceDays, y = VisITedResources)) + geom_boxplot(aes(fill=StudentAbsenceDays))

ggplot(edu, aes(x = StudentAbsenceDays, y = AnnouncementsView)) + geom_boxplot(aes(fill=StudentAbsenceDays))

ggplot(edu, aes(x = StudentAbsenceDays, y = Discussion)) + geom_boxplot(aes(fill=StudentAbsenceDays))

ggplot(edu, aes(x = ParentAnsweringSurvey, y = raisedhands)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))

# Yes Answers to Surveys = More Raised hands


ggplot(edu, aes(x = ParentAnsweringSurvey, y = VisITedResources)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))

ggplot(edu, aes(x = ParentAnsweringSurvey, y = AnnouncementsView)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))

ggplot(edu, aes(x = ParentAnsweringSurvey, y = Discussion)) + geom_boxplot(aes(fill=ParentAnsweringSurvey))


###Class-Wise Boxplots

ggplot(edu, aes(x = Class, y = raisedhands)) + geom_boxplot(aes(fill=Class))

# High Marks = Active Participation


ggplot(edu, aes(x = Class, y = VisITedResources)) + geom_boxplot(aes(fill=Class))


# High Marks by visited Resources


ggplot(edu, aes(x = Class, y = AnnouncementsView)) + geom_boxplot(aes(fill=Class))


# More Marks More Announcements 


ggplot(edu, aes(x = Class, y = Discussion)) + geom_boxplot(aes(fill=Class))


### Scatterplots


ggplot(edu, aes(x = raisedhands, y = VisITedResources)) + geom_point() +
  geom_smooth(method = "lm",color='green')

ggplot(edu, aes(x = raisedhands, y = AnnouncementsView)) + geom_point() +
  geom_smooth(method = "lm",color='red')

ggplot(edu, aes(x = raisedhands, y = Discussion)) + geom_point() +
  geom_smooth(method = "lm",color='purple')

ggplot(edu, aes(x = VisITedResources, y = AnnouncementsView)) + geom_point() +
  geom_smooth(method = "lm",color='cyan')

ggplot(edu, aes(x = VisITedResources, y = Discussion)) + geom_point() +
  geom_smooth(method = "lm",color='firebrick')

ggplot(edu, aes(x = AnnouncementsView, y = Discussion)) + geom_point() +
  geom_smooth(method = "lm",color='hotpink')


### Density Plots


ggplot(edu, aes(x = raisedhands, color = gender)) + geom_density()


ggplot(edu, aes(x = raisedhands, color = Topic)) + geom_density()



ggplot(edu, aes(x = raisedhands, color = SectionID)) + geom_density()

ggplot(edu, aes(x = raisedhands, color = Semester)) + geom_density()

ggplot(edu, aes(x = raisedhands, color = Class)) + geom_density()


### Tile Map

tile.map <- edu %>% group_by(gender, NationalITy) %>%
  summarise(Count = n()) %>% arrange(desc(Count))

ggplot(tile.map, aes(x = gender, NationalITy, fill = Count)) + geom_tile()



### Predictive Modeling



# Splitting data into train and cross-validation sets by using a different sample.

set.seed(23210)
split <- sample.split(edu$Class, SplitRatio = 0.75)
train <- subset(edu, split == T)
cv <- subset(edu, split == F)



# Decision Tree


tree.model <- rpart(Class ~ ., data = train, method = "class", minbucket = 1)
prp(tree.model)


tree.predict <- predict(tree.model, cv, type = "class")
table(cv$Class, tree.predict)


# Decision Tree Using Caret Package


rpart.control = trainControl(method = "repeatedcv", number = 10, repeats = 3)
rpart.grid = expand.grid(.cp = seq(0.01, 0.5, 0.02))
rpart.model.caret <-train(Class ~ ., data = train, method = "rpart", preProcess = "scale",
    trControl = rpart.control, tuneGrid = rpart.grid)



rpart.predict.caret <- predict.train(rpart.model.caret, cv)
confusionMatrix(rpart.predict.caret, cv$Class)



Accuracy -> 0.7355

# Random Forest


set.seed(10005)

rf.model <- randomForest(Class ~ .- SectionID , data = train, importance = TRUE,
      ntree = 2000, nodesize = 20)

rf.predict <- predict(rf.model, cv)
confusionMatrix(cv$Class, rf.predict)



varImpPlot(rf.model)


Accuracy -> 0.6777

# C-Forest Utilizing Party


cforest.model = cforest(Class ~ .-SectionID , data = train,
    controls=cforest_unbiased(ntree=2000, mtry = 3))



cforest.prediction = predict(cforest.model, cv, OOB = TRUE, type = "response")
confusionMatrix(cv$Class, cforest.prediction)


Accuracy -> 0.7438

# Suppost Vector Machines


svm.model <- svm(Class ~ ., data = train, kernel = "radial", cost = 10, gamma = 0.15)
svm.predict <- predict(svm.model, cv)
confusionMatrix(cv$Class, svm.predict)


Accuracy ->  0.777

# Ensemble Model


results <- data.frame(tree = tree.predict, rpart = rpart.predict.caret, rf = rf.predict,
      cforest = cforest.prediction, svm = svm.predict,
      actual.class = cv$Class, final.prediction = rep("-",nrow(cv)))

results




getmode <- function(x) {
  unique.x <- unique(x)
  unique.x[which.max(tabulate(match(x, unique.x)))]
}



results$final.prediction <- apply(results, 1, getmode)
confusionMatrix(results$actual.class, results$final.prediction)


Accuracy -> 0.810 (best)

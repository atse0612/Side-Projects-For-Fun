### NFL Draft

# Load Libraries
options(warn=-1)
library(dplyr)
library(ggplot2)
library(repr)

setwd('./Kaggle')


# Read the Data
draft <- read.csv('./NFL Draft.csv')
head(draft)
str(draft)

# Checking for the NA Values

any(is.na(draft))

# Creating the Linear Regression
draft <- draft %>% select(Pick, DrAV) %>% filter(Pick<257) %>% na.omit()
fit.1 <- lm(DrAV ~ Pick, data=draft)
fit.2 <- lm(DrAV ~ poly(Pick,2), data=draft)
fit.3 <- lm(DrAV ~ poly(Pick,3), data=draft)
fit.4 <- lm(DrAV ~ poly(Pick,4), data=draft)
fit.5 <- lm(DrAV ~ poly(Pick,5), data=draft)
fit.6 <- lm(DrAV ~ poly(Pick,6), data=draft)
fit.7 <- lm(DrAV ~ poly(Pick,7), data=draft)

anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7)

# Draft Pick Exploratory Analysis

draft$y_hat <- predict(fit.5)
group_by_pick <- draft %>% group_by(Pick) %>% summarise(predicted_av = mean(y_hat)) %>% data.frame()
options(repr.plot.width=4, repr.plot.height=3)
ggplot(group_by_pick, aes(Pick, predicted_av)) + geom_point(color='blue')


# Quarterback Draft Analysis

qb <- read.csv("./Nfl Draft.csv")
qb <- qb %>% filter(Position.Standard=='QB') %>% select(Pick, DrAV) %>% filter(Pick<257) %>% na.omit()
fit.5 <- lm(DrAV ~ poly(Pick,5), data=qb)
new <- data.frame(Pick = seq_len(256))
y_hat <- predict(fit.5, new, se.fit = TRUE)
df <- data.frame(y_hat = matrix(unlist(y_hat)))
qb <- inner_join(qb, df, by=)
group_by_pick <- qb %>% group_by(Pick) %>% summarise(predicted_av = mean(y_hat)) %>% data.frame()
options(repr.plot.width=4, repr.plot.height=3)
ggplot(group_by_pick, aes(Pick, predicted_av)) + geom_point()


qb <- read.csv("./Nfl Draft.csv")
qb <- filter(qb, Position.Standard=="QB")
qb <- qb %>% select(Pick, DrAV) %>% filter(Pick<257) %>% na.omit()
fit.5 <- lm(DrAV ~ poly(Pick,5), data=qb)
new <- data.frame(Pick = seq_len(256))
y_hat <- predict(fit.5, new, se.fit = TRUE)
df <- data.frame(y_hat = matrix(unlist(y_hat)))
Predicted_AV <- df[seq(1,256),]
df <- data.frame(Pick = new$Pick, Predicted_AV)
ggplot(df, aes(Pick, Predicted_AV)) + geom_point(color='orange')



# Running Backs in Draft

rb <- read.csv("./Nfl Draft.csv")
rb <- filter(rb, Position.Standard=="RB")
rb <- qb_df %>% select(Pick, DrAV) %>% filter(Pick<257) %>% na.omit()
fit.5 <- lm(DrAV ~ poly(Pick,5), data=rb)
new <- data.frame(Pick = seq_len(256))
y_hat <- predict(fit.5, new, se.fit = TRUE)
df <- data.frame(y_hat = matrix(unlist(y_hat)))
Predicted_AV <- df[seq(1,256),]
df <- data.frame(Pick = new$Pick, Predicted_AV)
ggplot(df, aes(Pick, Predicted_AV)) + geom_point(color='red')
head(df)



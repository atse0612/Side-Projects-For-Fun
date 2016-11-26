# Used Cars Database

library(ggplot2)
library(readr)
library(data.table)
library(lubridate)


# Results Redirected to the Output
usedautos <- read.csv("./usedautos.csv")
usedautos <- fread("./usedautos.csv")
attach(usedautos)
levels(brand)
summary(usedautos)
summary(brand)
brand_table <- summary(brand)
barplot(brand_table)
brand_name <- levels(brand)
barplot(brand_table, main= "Classification of used cars brandwise", xlab = "Car brand", ylab = "Qty", names.arg = c(brand_name))

# Picking Cars by Brand
porsche_cars <- usedautos[brand == "porsche" & yearOfRegistration >2000,] 
head(porsche_cars)

table(porsche_cars$gearbox)
porsche_gearbox <- table(porsche_cars$gearbox)
pie(porsche_gearbox)
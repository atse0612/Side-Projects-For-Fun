# Diamond Prices

### Load Libraries

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(magrittr)
library(caret)
library(plotly)
library(corrplot)


### Receiving Dataset and Change Working Directory
setwd('./Kaggle')

diamond <- read.csv('diamonds.csv')
head(diamond)

### Class and Missing Variables for Diamonds

data.frame(cbind(data.frame(VarType=sapply(diamond,class)),data.frame(Total_Missing=sapply(diamond,function(x){sum(is.na(x))}))))


## See Different Levels in Factor Variations
print("Cut Levels")
levels(diamond$cut)

print("Color Levels")
levels(diamond$color)

print("Clarity Levels")
levels(diamond$clarity)



### Density Plots

qplot(price, data=diamond, geom="density", fill=cut, alpha=I(.5), 
      main="Distribution of Carat", xlab="Different kinds of cut", 
      ylab="Density") + theme_minimal()

qplot(price, data=diamond, geom="density", fill=color, alpha=I(.5), 
      main="Distribution of Carat", xlab="Different Colors", 
      ylab="Density") + theme_minimal()

qplot(price, data=diamond, geom="density", fill=clarity, alpha=I(.5), 
      main="Distribution of Carat", xlab="Different clarity parameters", 
      ylab="Density") + theme_minimal()


### More Plots

ggplot(data=diamond,aes(x=cut))+geom_bar(fill="green")+theme_minimal()+ylab("Total Count")+ggtitle("Distribution of Diamonds by Cut Type")
ggplot(data=diamond,aes(x=color))+geom_bar(fill="khaki")+theme_minimal()+ylab("Total Count")+ggtitle("Distribution of Diamonds by Color Type")
ggplot(data=diamond,aes(x=clarity))+geom_bar(fill="violet")+theme_minimal()+ylab("Total Count")+ggtitle("Distribution of Diamonds by Clarity Type")


### Encoding

ohe_features<-c("cut","color","clarity")
dummies<-dummyVars(~cut + color + clarity ,data=diamond)

diamond_ohe<-as.data.frame(predict(dummies,newdata=diamond))
diamond_combined<-cbind(diamond,diamond_ohe)

newdiamond<-diamond_combined[,!names(diamond_combined)%in%ohe_features]

rm(diamond_combined)
rm(diamond_ohe)


### Looking at the New Data
head(newdiamond)

### Dropping all the Null Values

x.label<-newdiamond$X
y.label <-as.numeric(newdiamond$price)

newdiamond$X<-NULL
newdiamond$price<-NULL

## Correlation plot
corrplot(cor(cbind(newdiamond,Price=y.label)),type="upper")


### World Food Facts


setwd('./Kaggle')

# Load Libraries:

library(ggplot2)
library(dplyr)
library(data.table)
library(caret)
library(corrplot)

# Reading the Data
food = read.csv('./FoodFacts.csv')
head(food)
summary(food)

# Checking for NA Values
any(is.na(food))

## Cleaning the Data

# Meat and Vegan

meat = grep("meat|Meat", food$categories_en)
vegan = grep("vegan|Vegan", food$labels)


# New Dataframe for Countries and Products

Countries = as.data.frame(table(food$countries_en))
Countries = Countries[-1,]

# Countries With the Highest Value

Top = head(Countries[order(-Countries$Freq),],10)
Top = droplevels(Top)


# Adding Values to the Countries

i = 1

while (i < length(Top$Var1)+1) {
  
  Country = grep(Top[i,1], Countries$Var1)
  Country_totals = sum(Countries$Freq[Country])
  Top[i,2] = Country_totals
  i=i+1
}

# Plotting the Number of Products Per Country
barplot(Top$Freq, names.arg = Top$Var1, col = "lavender", main = "Number of Products by Country"
        , ylab = "Counts", las=1)

# New Dataframe for the Meat

Countries_meat = as.data.frame(table(food$countries_en[meat]))
Countries_meat = Countries_meat[-1,]

# Countries With the Highest Meat Products
Top_meat = head(Countries_meat[order(-Countries_meat$Freq),],10)
Top_meat = Top_meat[-grep(",", Top_meat$Var1),] #Get rid of any rows that are a combination of countries by looking for a comma
Top_meat = droplevels(Top_meat)


## Combining the Dataset

i = 1

while (i < length(Top_meat$Var1)+1) {
  
  Country = grep(Top[i,1], Countries_meat$Var1)
  Country_totals = sum(Countries_meat$Freq[Country])
  Top_meat[i,2] = Country_totals
  i=i+1
}

# New Dataframe for Vegan Products

Countries_vegan = as.data.frame(table(food$countries_en[vegan]))
Countries_vegan = Countries_vegan[-1,]


Top_vegan = head(Countries_vegan[order(-Countries_vegan$Freq),],10)
Top_vegan = Top_vegan[-grep(",", Top_vegan$Var1),] #Get rid of any rows that are a combination of countries by looking for a comma
Top_vegan = droplevels(Top_vegan)


i = 1

while (i < length(Top_vegan$Var1)+1) {
  
  Country = grep(Top_vegan[i,1], Countries_vegan$Var1)
  Country_totals = sum(Countries_vegan$Freq[Country])
  Top_vegan[i,2] = Country_totals
  i=i+1
}


rm(food)


#Change column names,
colnames(Top) = c("Country", "Count")
colnames(Top_meat) = c("Country", "Count")
colnames(Top_vegan) = c("Country", "Count")


#Do some merging to get overall results,
Results_meat = merge(Top, Top_meat, by = "Country")
Results_vegan = merge(Top, Top_vegan, by = "Country")
colnames(Results_meat) = c("Country", "Total no. of products", "No. of meat products")
colnames(Results_vegan) = c("Country", "Total no. of products", "No. of vegan products")

# % for Each Country for Meat Products
Results_meat$MeatPerc = Results_meat$`No. of meat products` / Results_meat$`Total no. of products` * 100
Results_vegan$VeganPerc = Results_vegan$`No. of vegan products` / Results_vegan$`Total no. of products` * 100


## Exploratory Data Analysis in Meat

# Meat
m = ggplot(Results_meat, aes(x=reorder(Country,-MeatPerc), y=MeatPerc))

m + geom_bar(stat = "identity", fill="yellow", colour="red") +
  ggtitle("Counties with Highest % of Meat Products \n (in terms of no. of products submitted)") +
  ylab("Percentage %") +
  theme_classic() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size=15, angle = 90)) +
  scale_x_discrete(name="")


# Vegan

v = ggplot(Results_vegan, aes(x=reorder(Country,-VeganPerc), y=VeganPerc))

v + geom_bar(stat = "identity", fill="dark blue", colour="green") +
  ggtitle("Counties with Highest % of Vegan-labelled Products \n (in terms of no. of products submitted)") +
  ylab("Percentage %") +
  theme_classic() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size=15, angle = 90)) +
  scale_x_discrete(name="")



### Removing Spain

Results_vegan = Results_vegan[-grep("Spain", Results_vegan$Country),]



v = ggplot(Results_vegan, aes(x=reorder(Country,-VeganPerc), y=VeganPerc))

v + geom_bar(stat = "identity", fill="orange", colour="red",alpha=0.5) +
  ggtitle("Counties with Highest % of Vegan-labelled Products - Spain omitted \n (in terms of no. of products submitted)") +
  ylab("Percentage %") +
  theme_classic() +
  theme(legend.position="none") +
  theme(axis.text.x = element_text(size=15, angle = 90)) +
  scale_x_discrete(name="")



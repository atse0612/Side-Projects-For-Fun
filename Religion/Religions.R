# Religion

## Libraries

library(data.table)
library(ggplot2)
library(bit64)
library(scales)

## Changing the Working Directory

setwd('./Kaggle/Religion')

## Reading the Christianity Data
global <- fread("./global.csv",select = c("year","christianity_romancatholic","romancatholic_percent"))
national <- fread("./national.csv",select = c("year","state","code","christianity_romancatholic","romancatholic_percent"))
regional <- fread("./regional.csv",select = c("year","region","christianity_romancatholic","romancatholic_percent"))


## Total Roman Catholics Visual

ggplot(global,aes(x = year,y = christianity_romancatholic))+
  geom_smooth()+labs(title="Roman Catholics",subtitle="By Year",caption="Donyoe")

## Gathering More Information

difere <- as.data.frame(diff(global$christianity_romancatholic))
colnames(difere) <- "Dif"
difere$year <- c("1945-1950","1950-1955","1955-1960","1960-1965","1965-1970","1970-1975","1975-1980","1980-1985","1985-1990","1990-1995","1995-2000","2000-2005","2005-2010")
ggplot(difere,aes(x = year,y=Dif))+
  geom_bar(stat = "identity", fill = "blue")+labs(title="Roman Catholics",subtitle="Difference By Years",caption="Donyoe")

## Percentage of Roman Catholics by Year

ggplot(global,aes(x = year,y = romancatholic_percent))+
  geom_smooth()+scale_y_continuous(labels=percent)+labs(title="Roman Catholics",subtitle="By Year",caption="Donyoe")


## Differences

difere <- as.data.frame(diff(global$romancatholic_percent))
colnames(difere) <- "Dif"
difere$year <- c("1945-1950","1950-1955","1955-1960","1960-1965","1965-1970","1970-1975","1975-1980","1980-1985","1985-1990","1990-1995","1995-2000","2000-2005","2005-2010")
ggplot(difere,aes(x = year,y=Dif))+
  geom_bar(stat = "identity", fill = "red")+scale_y_continuous(labels=percent)+labs(title="Roman Catholics",subtitle="Difference By Year",caption="Donyoe")


## Regional

ggplot(regional,aes(x = year,y = christianity_romancatholic,fill=region))+
  geom_smooth()+labs(title="Roman Catholics",subtitle="By Year",caption="Donyoe")


regional[,.("Dif"=diff(christianity_romancatholic)),by=.(region)]

## Percentage

ggplot(regional,aes(x = year,y = romancatholic_percent,fill=region))+
  geom_smooth()+scale_y_continuous(labels=percent)+labs(title="Roman Catholics",subtitle="By Year",caption="Donyoe")


regional[,.("Dif"=diff(romancatholic_percent)),by=.(region)]


## National Info

national[year==2010,.("N"=christianity_romancatholic),by=state][order(-N)][1:10]

national[year==2010,.("N"=romancatholic_percent),by=state][order(-N)][1:20]

ggplot(national[state%in%c("Brazil","Mexico","United States of America","Philippines","Italy","France","Colombia","Spain","Democratic Republic of the Congo","Poland")],aes(x = year,y = christianity_romancatholic,fill=state))+
  geom_smooth()+labs(title="Roman Catholics",subtitle="By Year",caption="Donyoe")

ggplot(national[state%in%c("Brazil","Mexico","United States of America","Philippines","Italy","France","Colombia","Spain","Democratic Republic of the Congo","Poland")],aes(x = year,y = romancatholic_percent,color=state,fill=state))+
  geom_smooth()+scale_y_continuous(labels=percent)+labs(title="Roman Catholics",subtitle="By Year",caption="Donyoe")

# Looking at Buddhism

## Reading the Data

global <- fread("./global.csv")
national <- fread("./national.csv")
regional <- fread("./regional.csv")


ggplot(global,aes(x = year,y = buddhism_all))+
  geom_smooth()+labs(title="Buddhism",subtitle="By Year")

bdife <- as.data.frame(diff(global$buddhism_all))
colnames(bdife) <- "Dif"
bdife$year <- c("1945-1950","1950-1955","1955-1960","1960-1965","1965-1970","1970-1975","1975-1980","1980-1985","1985-1990","1990-1995","1995-2000","2000-2005","2005-2010")
ggplot(bdife,aes(x = year,y=Dif))+
  geom_bar(stat = "identity", fill = "gold")+labs(title="Buddhism",subtitle="Difference By Years")


ggplot(global,aes(x = year,y = buddhism_percent))+
  geom_smooth()+scale_y_continuous()+labs(title="Buddhism",subtitle="By Year")


## Buddhism by Region

ggplot(regional,aes(x = year,y = buddhism_all,fill=region))+
  geom_smooth()+labs(title="Buddhist",subtitle="By Year")

## Aggregating the Information

buddhism_agg <-aggregate(buddhism_all ~ state, data = national, sum)
buddhism_agg2 <- buddhism_agg[order(-buddhism_agg$buddhism_all),] 
buddhism_agg2[1:10,]


## Looking at the Largest Population

ba3 <-subset(national,year==2010,select = c(state,buddhism_percent))
popper <- ba3[order(-ba3$buddhism_percent),] 
popper[1:10,]


ggplot(national[state%in%c('Cambodia','Thailand','Bhutan','Myanmar','Sri Lanka','Japan','Laos','Mongolia','Vietnam','Singapore')],aes(x = year,y = buddhism_all,fill=state))+
  geom_smooth()+labs(title="Buddhist Overall",subtitle="By Year")

ggplot(national[state%in%c('Cambodia','Thailand','Bhutan','Myanmar','Sri Lanka','Japan','Laos','Mongolia','Vietnam','Singapore')],aes(x = year,y = buddhism_percent,color=state,fill=state))+
  geom_smooth()+scale_y_continuous(labels=percent)+labs(title="Buddhist Percentages",subtitle="By Year")  

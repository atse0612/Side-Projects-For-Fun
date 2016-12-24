#### Norweigian Development Funds 


# Change Working Directory
setwd('./Kaggle')


# Load the Libraries

library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)


# Getting the Data
ndf <- read_csv("./funds.csv")
head(ndf)
summary(ndf)

# Checking for NA Values
any(is.na(ndf))

# Cleaning the Data
names(ndf) <- make.names(names(ndf))
ndf[, grep("NA.", colnames(ndf))] <- NULL

# Exploratory Data Analysis

ndf %>% group_by(Recipient.Region, Year) %>% 
  summarise(Disbursements=sum(Disbursements..1000...)/1000) %>% ungroup() %>% 
  ggplot(aes(x=Year, y=Disbursements)) + 
  geom_bar(stat = "identity",aes(fill=Recipient.Region)) + 
  facet_wrap(~Recipient.Region)

# Middle East is slowly picking up, Asia is on decline. 
# Interesting to see the same chart in terms of size of the contract. 
# Are certain regions getting bigger "support packages" then others? 
# Are there material differences in geographical vs non-geographical contracts? 
# Lets look at the quantity of those contracts by country.

ndf%>% group_by(Recipient.Region, Year) %>% 
  summarise(Mean_Disbursement=mean(Disbursements..1000...)/1000) %>% 
  ungroup() %>% 
  ggplot(aes(x=Year, y=Mean_Disbursement)) + 
  geom_bar(stat = "identity",aes(fill=Year)) + facet_wrap(~Recipient.Region)


# Non Geographical-Projects
ndf%>% dplyr::filter(Recipient.Region=="Not geographically allocated") %>% 
  group_by(Main.Sector) %>% 
  summarise(Disbursements=sum(Disbursements..1000...)/1000) %>% 
  ungroup() %>% 
  ggplot(aes(x=Main.Sector, y=Disbursements)) + 
  geom_bar(stat = "identity",aes(fill=Disbursements)) + coord_flip()

# It has been determined that administration costs are very high up for the 
# disbursements.


ndf %>% 
  dplyr::filter(grepl("910 - Administration", Main.Sector)) %>% 
  group_by(Budget.Post..Chapter) %>% 
  summarise(Disbursements=sum(Disbursements..1000...)/1000) %>% 
  ungroup() %>% 
  ggplot(aes(x=Budget.Post..Chapter, y=Disbursements)) + 
  geom_bar(stat = "identity",aes(fill=Disbursements)) + coord_flip()


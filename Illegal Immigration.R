## Illegal Immigration

## Loading the Libraries and Changing the Working Directory
library(tidyverse)
library(d3Network)
setwd('./Kaggle')

## Reading the Dataset & Rearranging Them

arrests <- read.csv('./illegal immigration.csv')
names(arrests) <- gsub("[.]", " ", names(arrests))
arrests.clns <-
  arrests %>% gather(key, value, -Border, -Sector, -`State Territory`) %>% separate(key,
                                                                                    into = c("Year", "Type"),
                                                                                    sep = " ",
                                                                                    extra = "merge")%>%na.omit()

arrests.clns$Year <- gsub("X", "", arrests.clns$Year)
arrests.clns$Type<-trimws(tolower(arrests.clns$Type))

arrests.net <-
  arrests.clns %>% filter(Border != "United States" &
                            Sector != "All") %>% select(Type, Sector, value) %>% rename(source =
                                                                                          Type, target = Sector) %>% group_by(source, target) %>%
  summarize(value = mean(value)) %>% na.omit() %>% mutate(rank = rank(desc(value), source)) %>%
  arrange(rank)%>%filter(rank<=10)

Nodes <-
  rbind(data.frame(name = unique(arrests.net$source)), data.frame(name = unique(arrests.net$target)))
Links <- arrests.net
Links$source <- match(Links$source, Nodes$name) - 1
Links$target <- match(Links$target, Nodes$name) - 1

# Graphs
arrests.plot<-arrests.clns%>%group_by(Year,Type)%>%summarize(avg.value=mean(value))


ggplot(arrests.plot)+aes(Year,avg.value,color=Type)+
  geom_point(size=2)+theme(plot.title = element_text(hjust = 0.5, face = "bold"),
  axis.text.y=element_blank(),axis.text.x=element_blank())+
  labs(title="Average arrests over the years",y="Average Arrests")

## Based on the information that is given in the graph, the arrests
## of Mexicans have greatly reduced in comparison to all illegal immigrants. It
## has been considered that illegal immigration with Mexicans is not the biggest factor
## that is contributing to the problems in the United States. 

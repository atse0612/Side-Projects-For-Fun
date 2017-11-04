# Barbeque Wordcloud

## Loading the Libraries

library(readr)
library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(knitr)
library(DT)
library(tm)
library(wordcloud)


## Changing the Working Directory

setwd('./Kaggle/Barbeque')

## Reading the Dataset

contest <- read_csv("./contest_data.csv")
results_brisket <- read_csv("./results_brisket.csv")
results_chicken <- read_csv("./results_chicken.csv")
results_pork <- read_csv("./results_pork.csv")
results_ribs <- read_csv("./results_ribs.csv")


## Where Do Competitions Happen More Frequently?

most_happening <- contest %>% 
  group_by(state_full) %>% 
  summarise(count=n()) %>%
  arrange(desc(count))

datatable(most_happening, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))


## Where are the Big Prize Money?

big_prize_money <- contest %>% 
  group_by(state_full) %>% 
  summarise(avg_prize= mean(prize, na.rm = TRUE)) %>% 
  arrange(desc(avg_prize))

datatable(big_prize_money, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))


## Combining the Results
combined_result <- rbind(results_brisket,results_chicken,results_pork,results_ribs)

## Total Score
Total_score<-combined_result %>% 
  group_by(contest_key, team_name) %>% 
  summarise(tscore =sum(score))  %>% 
  data.frame() %>%
  arrange(contest_key, desc(tscore))

datatable(Total_score, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))


## Number of Participants

participants <- Total_score %>%
  group_by(contest_key)%>%
  summarise(No_of_teams = n())

datatable(participants, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))

rm(participants)
gc()


## Number of Appearances by a Team

number_of_apperance <- Total_score %>%
  group_by(team_name)%>%
  summarise(appearances = n())%>%
  arrange(desc(appearances))
datatable(number_of_apperance, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))


## Who's the Best Team Out There?

avg_total_team_score <- Total_score %>%
  group_by(team_name)%>%
  summarise(appearances = n(), avg_total_score = mean(tscore))%>%
  arrange(desc(avg_total_score))
datatable(avg_total_team_score, class="table-condensed", style="bootstrap", options = list(dom = 'tp'))


## Total Score Out of 800

library(tm)
library(wordcloud)
makeWordCloud <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]],
            colors=brewer.pal(8, "Dark2"),
            random.color=TRUE) 
}  


top_score<-Total_score %>%
  group_by(contest_key, team_name) %>%
  filter(tscore >600)

makeWordCloud(top_score[["team_name"]][1:2000])

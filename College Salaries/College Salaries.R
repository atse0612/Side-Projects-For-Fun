# College Salaries

## Loading the Libraries

library(tidyverse)
library(stringr)
library(gridExtra)
library(plotly)
library(readr)

## Changing the Working Directory

setwd('./Kaggle/College Salaries')

## Reading the First Dataset

type <- read_csv("./salaries-by-college-type.csv")


## Reformat the Salary

salary_reform <- function(salary) {
  
  if(is.na(salary)) return(NA)
  
  extract <- str_replace_all(salary, "\\$|,", "")
  num <- as.integer(extract)
  return(num)
}


## Fixing Up the Dataset

type <- type %>%
  select(1:4) %>%
  mutate(
    `Starting Median Salary` = salary_reform(`Starting Median Salary`),
    `Mid-Career Median Salary` = salary_reform(`Mid-Career Median Salary`)
  )


## Calculating the Mid-Career

type <- type %>%
  mutate(
    `Percentage Change` = round((`Mid-Career Median Salary`-`Starting Median Salary`)/`Starting Median Salary`,3)*100
  )

knitr::kable(head(type))


## Visualization of the Salary Distribution

type %>% 
  ggplot(aes(`Starting Median Salary`)) +
  geom_histogram(fill="indianred", color="lightgrey", binwidth=2000) +
  geom_histogram(aes(`Mid-Career Median Salary`),
                 fill="navyblue", color="lightgrey", binwidth=2000, alpha=0.6) +
  ggtitle("Broader Distribution for Mid-Career Salaries") +
  xlab("Salary") + ylab("Count")

## Boxplots of the Starting and Median Salaries


# Median values for starting and mid-career salaries
median_start <- median(type$`Starting Median Salary`)
mid <- median(type$`Mid-Career Median Salary`)

# Box Plot for Starting Salaries by School Type
school_type <- type %>% 
  ggplot(aes(`School Type`, `Starting Median Salary`, fill=`School Type`)) +
  geom_jitter(color="darkgrey", alpha=0.8) +
  geom_boxplot(alpha=0.6) +
  geom_abline(slope=0, intercept=median_start, color="red", linetype=2, alpha=0.5) +
  ggtitle("Engineering and Ivy League Lead the Way in Starting Salaries") +
  xlab("") + ylab("Starting Salary") +
  theme_bw() +
  theme(legend.position = "none")

# Box Plot for Mid-Career Salaries by School Type
mid_school <- type %>% 
  ggplot(aes(`School Type`, `Mid-Career Median Salary`, fill=`School Type`)) +
  geom_jitter(colour="darkgrey", alpha=0.8) +
  geom_boxplot(alpha=0.6) +
  geom_abline(slope=0, intercept=mid, colour="red", linetype=2, alpha=0.5) +
  ggtitle("Higher Upward Mobility for Ivy League Over Engineering Schools Over Time") +
  xlab("") + ylab("Mid-Career Salary") +
  theme_bw() +
  theme(legend.position = "none")

grid.arrange(school_type, mid_school, ncol=1)


## Looking at the Top 10 Salaries

type %>% 
  top_n(10, wt = `Mid-Career Median Salary`) %>% 
  gather("Career", "Salary", 3:4) %>% 
  mutate(Career = factor(Career, levels=c("Starting Median Salary","Mid-Career Median Salary"))) %>% 
  plot_ly(
    x=~Career, y=~Salary, color=~`School Name`, type="scatter", mode="lines+markers",
    text=~paste(`School Name`,"<br>",`School Type`,"<br>Change:",`Percentage Change`, "%"),
    colors="Paired"
  ) %>% 
  layout(
    title="Dartmouth with the Largest Salary Increase from Number 10 to Number 1",
    showlegend=FALSE,
    xaxis=list(showticklabels=FALSE,
               title="Universities with the Top Median Salaries"),
    yaxis=list(title="")
  )

# Focusing on the Region

## Loading the Dataset

region <- read_csv("./salaries-by-region.csv")


## Reformat Salary by Region

region <- region %>% 
  mutate(
    `Starting Median Salary` = salary_reform(`Starting Median Salary`),
    `Mid-Career Median Salary` = salary_reform(`Mid-Career Median Salary`)
  )

## Barplot of the Region

region %>% 
  group_by(Region) %>% 
  ### rank by mid-career and starting salary combined to break ties
  top_n(7, wt=`Mid-Career Median Salary` + `Starting Median Salary`) %>%
  mutate(Rank = rank(desc(`Mid-Career Median Salary`), ties.method="first")) %>% 
  plot_ly(x=~Region, y=~`Mid-Career Median Salary`, color=~factor(Rank),
          type="bar", colors="Set3",
          text=~paste(`School Name`, "<br>Rank:", Rank)) %>% 
  layout(showlegend = FALSE,
         title="Universities with the Highest Mid-Career Salaries by Region",
         yaxis=list(title="Mid-Career Median Salary"),
         xaxis=list(title=""))


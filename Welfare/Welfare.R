# Welfare


### Loading the Libraries
library(tidyverse)
library(readr)
library(ggmap)
library(highcharter)
data(usgeojson)


### Setting the Working Directory

setwd('./Kaggle/Welfare')

## Reading the Libraries
welfare <- read.csv('./UIerror.csv')
snap <- read.csv('./SNAPerror.csv')
welfare< - welfare %>%mutate(Fraud= as.numeric(unlist(strsplit(welfare$Fraud,'%'))))

## Reading NA 

any(is.na(welfare))
any(is.na(snap))

## Welfare Fraud By State

highchart() %>%
  hc_title(text = "Welfare Fraud Rate by State", align= "right") %>%
  hc_add_series_map(usgeojson, df = welfare,
                    value = "Fraud", joinBy =c("postalcode","ST")) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_538())%>% 
  hc_credits(enabled = TRUE, text = "https://www.dol.gov/general/maps/data",
             href = "https://www.dol.gov/general/maps/data")


highchart() %>%
  hc_title(text = "SNAP Payment Error Rate", align="right") %>%
  hc_add_series_map(usgeojson, df =snap,
                    value = "Error", joinBy =c("postalcode","ST")) %>%
  hc_mapNavigation(enabled = TRUE) %>%
  hc_add_theme(hc_theme_538()) %>% 
  hc_credits(enabled = TRUE, text = "https://www.fns.usda.gov/sites/default/files/snap/2014-rates.pdf",
             href = "https://www.fns.usda.gov/sites/default/files/snap/2014-rates.pdf")
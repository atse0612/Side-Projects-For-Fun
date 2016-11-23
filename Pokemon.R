# Pokemon

## In this report, we are going to analyze the different types of Pokemon to check on types and frequencies.



# Checking Frequencies
setwd("./Kaggle")
pokemon <- read.csv('./Pokemon.csv', header = T)
pokemon$Name <- as.character(pokemon$Name)
rev(sort(table(pokemon$Type.1)))
rev(sort(table(pokemon$Type.2)))

library(ggplot2)
library(ggthemes)
library(corrplot)
library(reshape2)

# In this section, we are going to generate a linear model to determine which Pokemon is the strongest in combination.


colnames(pokemon) <- c("number", "name", "type1", "type2", "total", "hp", 
                       "attack", "defense", "sp.atk", "sp.def", "speed", 
                       "generation", "legendary")
head(pokemon)
poke <- lm(total ~ hp + attack + defense + sp.atk + sp.def + speed, pokemon)
par(mfrow = c(2,2))
plot(poke)


pokemon[c(1,3,6), 2]







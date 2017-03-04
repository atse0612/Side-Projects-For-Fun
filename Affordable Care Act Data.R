# Affordable Health Care Act Data


### Loading the Libaries

library(ggplot2)
library(maps)
library(zipcode)
library(ggmap)
library(choroplethrMaps)
library(choroplethr)
data("state.map")


### Changing the Working Directory

setwd('./Kaggle')


### Reading the Data

acadat <- read.csv(file='./states.csv', header = TRUE, sep = ",", strip.white = TRUE )
m <- colnames(acadat)
length(m)
acadat <- read.csv(file='./states.csv', header = FALSE, skip = 1, sep = ",", strip.white = TRUE )
n <- colnames(acadat)

head(acadat)

### Reading the Dictionary and Cleaning the Data

any(is.na(acadat))

dictionary <- cbind(n, m)

str(acadat)
# Strip percentages and dollars first
acadat[,c("V2", "V3", "V4")] <- (sapply(acadat[, c("V2", "V3", "V4")], function(x) as.numeric(gsub("%","", x))))
acadat$V9 <- gsub("\\$","", acadat$V9)
acadat$region  = tolower(acadat$V1)



us_state_map = map_data('state');
map_data = merge(acadat, us_state_map, by = 'region') 
map_data = arrange(map_data, order)

### Maps for the United States

ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(V2, 6))) +
  geom_path(colour = 'red') +  labs(title = "State level insurance coverage in 2010") +
  scale_fill_brewer('Uninsured Percent, 2010') + coord_map()

# Uninsured percent in 2015
ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(V3, 6))) +
  geom_path(colour = 'black', alpha = 0.5) + labs(title = "State level insurance coverage in 2015") +
  scale_fill_brewer('Uninsured Percent, 2015') + coord_map()

# Uninsured rate change, 2010-15
ggplot(map_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = cut_number(V4, 6))) +
  geom_path(colour = 'orange') + labs(title = "State level change in insurance coverage, 2010-15") +
  scale_fill_brewer('Uninsured rate change, 2010-15') + coord_map()

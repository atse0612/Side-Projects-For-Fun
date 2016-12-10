# Data Visualization Project

# Loading Data:
# In this section, we are going to load the libraries and data as needed in order to perform The Economist data graph as made on the internet.

```{r}
library(ggplot2)
library(data.table)
library(ggthemes)
df <- fread('Economist_Assignment_Data.csv', drop=1)
summary(df)
head(df)
tail(df)
```


# Creating a Scatterplot:
# Based on the information that is given, we are going to create a scatterplot based on the region that is given. 

```{r}
plot <- ggplot(df,aes(x=CPI,y=HDI,color=Region)) + geom_point(aes(color=factor(Region)))
print(plot)
```


# Linear Models:

# In this section, we are going to use different trend lines to determine the API and HDI differences.
```{r}
plot <- ggplot(df,aes(x=CPI,y=HDI,color=Region)) + geom_point(size=4,shape=1)
print(plot)
plot2 <- plot + geom_smooth(aes(group=1))
print(plot2)
plot2 <- plot + geom_smooth(aes(group=1),method='lm', formula = y~log(x),se=F,color='red')
print(plot2)
```


# Adding Text:

# For this section, we are going to add the labels of the countries that are represented in the graph. With the result, the labels have overlapped in the graph.

```{r}
plot <- ggplot(df,aes(x=CPI,y=HDI,color=Region)) + geom_point(size=4,shape=1)
print(plot)
plot2 <- plot + geom_smooth(aes(group=1),method='lm', formula = y~log(x),se=F,color='red')
plot3 <- plot2 + geom_text(aes(label=Country))
print(plot3)
```

# Subsetting the Labels:

# Continuing from the previous section, we are only going to pick a select group of countries that will be displayed in the plot we created earlier. 

```{r}
pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spain",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway",
                   "Japan","New Zealand", "Singapore")

plot3 <- plot2 + geom_text(aes(label = Country), color = "gray20", 
                data = subset(df, Country %in% pointsToLabel),check_overlap = TRUE)

print(plot3 + theme_economist_white())

plot4 <- plot3 + scale_x_continuous(limits=c(.9,10.5),breaks = 1:10)
print(plot4 + theme_economist_white())
```

# Adding a Title to the Graph and Fixing the Aesthetics on the X and Y Axis
```{r}
plot5 <- plot4 + scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",limits = c(.9, 10.5),breaks=1:10) 
plot5



plot6 <- plot5 + scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",limits = c(0.2, 1.0))
plot6
```



# Adding a Title and Theme to Finish Off the Look of the Graph
```{r}
plot6 + ggtitle("Corruption and Human Development")
plot6 + theme_economist_white()
```

# Fake News

## Loading the Libraries

library(rpart)
library(rpart.plot)


setwd('./Kaggle')


### Reading the Fake News Dataset

fake <- read.csv('fake.csv')
print(table(fake$type))
head(fake)

### Checking for NA Values

any(is.na(fake))

### Running the Analysis


mini_model <- rpart(formula = type ~ ord_in_thread + language + country + spam_score + replies_count + participants_count + likes + comments + shares,
                    data = fake,
                    method = "class", # Classification
                    parms = list(split = "information"), # Use Information Gain as splitting criterion
                    control = rpart.control(cp = 0.01, # Minimum loss decrease complexity param
                                            maxcompete = 3, # Competition by split for debugging
                                            maxsurrogate = 3, # Competition per surrogate for debugging
                                            xval = 20, # 10 cross-validation
                                            maxdepth = 4)) # Maximum Depth for easy interpretation
plotcp(mini_model)



### Decision Tree
rpart.plot(mini_model, main = "Decision Tree", box.palette = list("Gy", "Gn", "Bu", "Bn", "Or", "Rd", "Gy", "Pu"))


print(summary(mini_model))

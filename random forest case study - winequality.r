setwd('')
#Load data into R
wine  <- read.csv("winequality-white.csv", stringsAsFactors = FALSE)

#confirm the particulars of the data
str(wine)

#check the contents
head(wine)

#check values of variable of interest
barplot(table(wine$quality))

#As we can see, there are a lot of wines with a 
#quality of 6 as compared to the others. The dataset 
#description states that there are a lot more normal 
#wines than excellent or poor ones. For the purpose 
#of this discussion, let's classify the wines into 
#good, bad, and normal based on their quality.
wine$taste <- ifelse(wine$quality < 6, 'bad', 'good')
wine$taste[wine$quality == 6] <- 'normal'
wine$taste <- as.factor(wine$taste)

table(wine$taste)


#Before we build our model, let's separate our data into testing and training sets.
set.seed(123)
samp <- sample(nrow(wine), 0.6 * nrow(wine))
train <- wine[samp, ]
test <- wine[-samp, ]

head(train)
install.packages("randomForest")
library(randomForest)

#building the model now
#take all the predictors except quality
model <- randomForest(taste ~ . - quality, data = train,ntree=500,mtry=3)

#check the confusion matrix
model

#We can see that 500 trees were built, and the model 
#randomly sampled 3 predictors at each split. It also shows 
#a matrix containing prediction vs actual, as well as 
#classification error for each class. Let's test the model 
#on the test data set.
pred <- predict(model, newdata = test)
table(pred, test$taste)

diag <- diag(table(pred, test$taste))
sumdiag <- sum(diag)
sumtotal <- sum(table(pred, test$taste))
accuracy <- sumdiag/sumtotal
accuracy


#we can futher improve the model by tweaking paramters like number of 
#trees and feature selection

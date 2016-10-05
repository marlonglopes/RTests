library(caTools)

# Randomly split the data into training and testing sets
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.50)

# Split up the data using subset
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

table(train$isB) 
table(test$isB) 
1175/(1175+383) = 0.754172  baseline accuracy for test set

######

# Load CART packages
library(rpart)
library(rpart.plot)

# CART model
CARTb = rpart(isB ~ . - letter, data=train, method="class")
prp(CARTb)

predictions = predict(CARTb, newdata=test, type="class")

#We can use the following command to build our confusion matrix:

table(test$isB, predictions)

(1118+340)/nrow(test) = 0.9358151


#Randon Forest

set.seed(1000)
RFb = randomForest(isB ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar + xybar + x2ybar + xy2bar + xedge + xedgeycor + yedge + yedgexcor, data=train)
RFb = randomForest(isB ~ . - letter, data=train)
predictions = predict(RFb, newdata=test)
table(test$isB, predictions)

(1165+374)/nrow(test) = 0.9878049




#Multiclass randonForest
letters$letter = as.factor( letters$letter )

set.seed(2000)

split = sample.split(letters$letter, SplitRatio = 0.50)

# Split up the data using subset
train = subset(letters, split==TRUE)
test = subset(letters, split==FALSE)

table(train$letter) 
table(test$letter) 

 A   B   P   R 
395 383 401 379

401/nrow(test) = 0.2573813

#Predicting the letters A, B, P, R

#You can build the CART tree with the following command:

CARTletter = rpart(letter ~ . - isB, data=train2, method="class")

#Then, you can make predictions on the test set with the following command:

predictLetter = predict(CARTletter, newdata=test2, type="class")

#Looking at the confusion matrix, table(test2$letter, predictLetter), we want to sum the main diagonal (the correct predictions) and divide by the total number of observations in the test set:

table(test2$letter, predictLetter)

 predictLetter
      A   B   P   R
  A 348   4   0  43
  B   8 318  12  45
  P   2  21 363  15
  R  10  24   5 340

(348+318+363+340)/nrow(test2) = 0.8786906


#randonForest Multiclass

#Multiclass randonForest

#First set the seed, and then build the random forest model:

set.seed(1000)

RFletter = randomForest(letter ~ . - isB, data=train2)

#Make predictions using the predict function:

predictLetter = predict(RFletter, newdata=test2)

#And then we can compute the test set accuracy by looking at the confusion matrix table(test2$letter, predictLetter). The test set accuracy is the sum of the numbers on the main diagonal, divided by the total number of observations in the test set:

table(test2$letter, predictLetter)
    A   B   P   R
  A 390   0   3   2
  B   0 380   1   2
  P   0   5 393   3
  R   3  12   0 364
  
(390+380 +393+364)/nrow(test2) = 0.9801027

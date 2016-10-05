
data(state)
statedata = data.frame(state.x77)

RegModel = lm(Life.Exp ~ . , data=statedata)
summary(RegModel)


#Calculate the sum of squared errors (SSE) between the predicted life expectancies using this model and the actual life expectancies

Predictions = predict(RegModel)
sum((statedata$Life.Exp - Predictions)^2)
#The SSE is 23.29714.

sum(RegModel$residuals^2)





RegModel = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad , data=statedata)
summary(RegModel)

#SSE
sum(RegModel$residuals^2)


#Let's now build a CART model to predict Life.Exp using all of the other variables as independent variables (Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area).
#We'll use the default minbucket parameter, so don't add the minbucket argument. 
#Remember that in this problem we are not as interested in predicting life expectancies for new observations as we are understanding how they relate to the other variables we have, 
#so we'll use all of the data to build our model. You shouldn't use the method="class" argument since this is a regression tree.


# Load CART packages
library(rpart)
library(rpart.plot)

#Estimate the CART tree:

statetree = rpart( Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area , data = statedata)
statetree = rpart( Life.Exp ~ . , data = statedata)

#Plot the tree:

prp(statetree)


PredictionsCART = predict(statetree)

###############

#SSE
sum((statedata$Life.Exp - PredictionsCART)^2)

#new minbucket

# Load CART packages
library(rpart)
library(rpart.plot)

#Estimate the CART tree:

statetree = rpart( Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area , data = statedata)
statetree = rpart( Life.Exp ~ . , data = statedata, minbucket = 5)

#Plot the tree:

prp(statetree)


PredictionsCART = predict(statetree)

###############

#SSE
sum((statedata$Life.Exp - PredictionsCART)^2)



CARTmodel3 = rpart(Life.Exp ~ Area, data=statedata, minbucket=1)
prp(CARTmodel3)
#Then to compute the SSE, first make predictions:

PredictionsCART3 = predict(CARTmodel3)

#And then compute the sum of squared differences between the actual values and the predicted values:

sum((statedata$Life.Exp - PredictionsCART3)^2)



#CV

library(caret)

set.seed(111)

#Then, you can set up the cross-validation controls by typing:

fitControl = trainControl(method = "cv", number = 10)

cartGrid = expand.grid(.cp = seq(0.01, 0.5, 0.01) )

#You can then use the train function to find the best value of cp by typing:

train(Life.Exp ~ ., data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid)

#At the bottom of the output, it says that the best value of cp is 0.12.


#You can create a new tree with cp=0.12 by typing:
#Then, if you plot the tree using prp(CARTmodel4), you can see that the life expectancy is predicted to be 70 if Murder is greater than or equal to 6.6 (the first split) 
# and less than 11 (the second split).

CARTmodel4 = rpart(Life.Exp ~ ., data=statedata, cp=0.12)

prp(CARTmodel4)







set.seed(111)

train(Life.Exp ~ Area, data=statedata, method="rpart", trControl = fitControl, tuneGrid = cartGrid )

#Then, build a new CART tree by typing:

CARTmodel5 = rpart(Life.Exp ~ Area, data=statedata, cp=0.02)

#You can plot the tree with prp(CARTmodel5), and see that the tree has 4 splits.









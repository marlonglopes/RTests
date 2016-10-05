
census = read.csv("census.csv")

#We now need to split the data. Load the caTools package, and set the seed to 2000:

library(caTools)

set.seed(2000)

#Split the data set according to the over50k variable:

spl = sample.split(census$over50k, SplitRatio = 0.6)

train = subset(census, spl==TRUE)

test = subset(census, spl==FALSE)

#We are now ready to run logistic regression. Build the logistic regression model:

censusglm = glm( over50k ~ . , family="binomial", data = train)

#Finally, look at the model summary to identify the significant factors:

#summary(censusglm)

# Predictions on the test set
predictTest = predict(censusglm, type="response", newdata=test)

# Confusion matrix with threshold of 0.5
table(test$over50k, predictTest > 0.5)

##############

#baseline

table(train$over50k)

#"<=50K" is the more frequent outcome (14570 observations), so this is what the baseline predicts. To generate the accuracy of the baseline on the test set, we can table the dependent variable in the test set:

table(test$over50k)

#The baseline accuracy is

9713/(9713+3078) = 0.7593621


# Test set AUC 
library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)


#########



# Load CART packages
library(rpart)
library(rpart.plot)

#Estimate the CART tree:

censustree = rpart( over50k ~ . , method="class", data = train)

#Plot the tree:

prp(censustree)

###############

#First, generate predictions on the test set using the CART tree:

predictTest = predict(censustree, newdata = test, type = "class")

#Then create the confusion matrix:

table(test$over50k, predictTest)

#To compute the accuracy, sum the diagonal entries and divide by the sum of all of the terms:

(9243+1596)/(9243+470+1482+1596) = 0.8473927


################

library(ROCR)
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)



#AUC for test set

library(ROCR)

#Generate the predictions for the tree. Note that unlike the previous question, when we call the predict function, we leave out the argument type = "class" from the function call. Without this extra part, we will get the raw probabilities of the dependent variable values for each observation, which we need in order to generate the AUC. We need to take the second column of the output:

predictTest = predict(censustree, newdata = test)
predictTest = predictTest[,2]

#Compute the AUC:

ROCRpred = prediction(predictTest, test$over50k)

as.numeric(performance(ROCRpred, "auc")@y.values)


#RandonForest

library(caTools)

set.seed(1000)

#Split the data set according to the over50k variable:

spl = sample.split(census$over50k, SplitRatio = 0.6)
train = subset(census, spl==TRUE)
test = subset(census, spl==FALSE)

#small training set
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

RFb = randomForest(over50k ~ . , data=trainSmall)
predictions = predict(RFb, newdata=test)
table(test$over50k, predictions)
(1165+374)/nrow(test) = 0.9878049



#To generate the random forest model with all of the variables, just run:

set.seed(1)
censusrf = randomForest(over50k ~ . , data = trainSmall)

#And then you can make predictions on the test set by using the following command:
predictTest = predict(censusrf, newdata=test)

#And to compute the accuracy, you can create the confusion matrix:
table(test$over50k, predictTest)

#The accuracy of the model should be around
(9614+1050)/nrow(test) = 0.8337112


####################################


#This code produces a chart that for each variable measures the number of times that variable was selected for splitting 
#(the value on the x-axis).

vu = varUsed(censusrf, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))



#A different metric we can look at is related to "impurity", which measures how homogenous each bucket or leaf of the tree is. 
#In each tree in the forest, whenever we select a variable and perform a split, the impurity is decreased. Therefore, 
#one way to measure the importance of a variable is to average the reduction in impurity, 
#taken over all the times that variable is selected for splitting in all of the trees in the forest.
# To compute this metric, run the following command in R (replace "MODEL" with the name of your random forest model):

varImpPlot(censusrf)

#Crossvalidation

set.seed(2)

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

#Before doing anything, load the caret package:

library(caret)

#Set the seed to 2:

set.seed(2)

#Specify that we are going to use k-fold cross validation with 10 folds:

fitControl = trainControl( method = "cv", number = 10 )

#Specify the grid of cp values that we wish to evaluate:

cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))

#Finally, run the train function and view the result:

train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

#The final output should read

#Accuracy was used to select the optimal model using the largest value.

#The final value used for the model was cp = 0.002.

#In other words, the best value was cp = 0.002, 
#corresponding to the lowest cp value. If we look more closely at the accuracy at different cp values,
# we can see that it seems to be decreasing steadily as the cp value increases. Often, 
#the cp value needs to become quite low before the accuracy begins to deteriorate.


#After find the best cp

#You can create a CART model with the following command:

model = rpart(over50k~., data=train, method="class", cp=0.002)

#You can make predictions on the test set using the following command (where "model" is the name of your CART model):

predictTest = predict(model, newdata=test, type="class")

#Then you can generate the confusion matrix with the command

table(test$over50k, predictTest)

#The accuracy is (9178+1838)/(9178+535+1240+1838) = 0.8612306.




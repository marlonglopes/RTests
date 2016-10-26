# Fit lm model: 
model <- lm(price ~ ., diamonds)

# Predict on full data: 

p = predict(model, diamonds)

# Compute errors: 

error = p - diamonds$price

# Calculate RMSE

RMSE =sqrt((1/length(error)) *  (sum(error ^ 2)))




# Fit lm model: model
model <- lm(price ~ ., diamonds)

# Predict on full data: p
p <- predict(model, diamonds)

# Compute errors: error
error <- p - diamonds[["price"]]

# Calculate RMSE
sqrt(mean(error^2))






> library(caret)
> library(mlbench)
> data(Sonar)
> set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class, 
## the outcome data are needed
p = .75,
## The percentage of data in the
## training set
list = FALSE)
## The format of the results
> ## The output is a set of integers for the rows of Sonar
> ## that belong in the training set.
> str(inTrain)
	int [1:157, 1] 1 2 3 6 7 9 10 11 12 13

plsFit <- train(Class ~ .,
 data = training,
 method = "pls",
 ## Center and scale the predictors for the training
 ## set and all future samples.
 preProc = c("center", "scale"))



#Predictive reather than explanatory modeling

models that dont overfitting and generalized well




# Set seed

set.seed(42)


# Shuffle row indices: rows

rows <- sample(nrow(diamonds))

# Randomly order data

diamonds <- diamonds[rows, ]



# Determine row to split on: split

split <- round(nrow(diamonds) * .80)


# Create train

train = diamonds[1:split, ]

# Create test

test = diamonds[(split+1):nrow(diamonds), ]


# Fit lm model on train: model

model <- lm(price ~ ., train)

# Predict on test: p

p = predict(mod, test)

# Compute errors: error

error = p - test$price

# Calculate RMSE

RMSE = sqrt(mean(error^2))

RMSE





#Multiple test sets

#N Folds
#CrossValidation

#Caret does bootstrap validtion
library(caret)
data(mtcars)
set.seed(42)
model = train(mpg ~ hp, mtcars, method = "lm", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))


# Fit lm model using 10-fold CV: model
model <- train(
  price ~. , diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Print model to console

model



# Fit lm model using 5-fold CV: model
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = TRUE
  )
)

# Print model to console

model


# Fit lm model using 5 x 5-fold CV: model
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)

# Print model to console

model


# Predict on full Boston dataset

predict(model, Boston)


library(mlbench)
data(Sonar)

rows = sample(nrow(Sonar))
Sonar = Sonar[rows,]


split = round(nrow(Sonar) * .60)
train = Sonar[1:split,]
test = Sonar[(split+1):nrow(Sonar),]

# Shuffle row indices: 

rows = sample(nrow(Sonar))

# Randomly order data: Sonar

Sonar = Sonar[rows,]

# Identify row to split on: split
split <- round(nrow(Sonar) * .60)

train = Sonar[1:split,]
test = Sonar[(split+1):nrow(Sonar),]





# Fit glm model: model

model = glm(Class ~ ., family = "binomial", train)

# Predict on test: p

p = predict(model, test, type = "response")


# Fit glm model: model

model = glm(Class ~ ., family = binomial(link = "logit"), train)

# Predict on test: p

p = predict(model, test, type = "response")

p_class = ifelse(p > .50, "M", "R")
table(p_class)

table(p_class, test[["Class"]])


#Usefull tool to evaluate classification models is confusion matrix

confusionMatrix(p_class, test[["Class"]])

Confusion Matrix and Statistics

          Reference
Prediction  M  R
         M 13 25
         R 27 18
                                          
               Accuracy : 0.3735 *******         
                 95% CI : (0.2697, 0.4866)
    No Information Rate : 0.5181          
    P-Value [Acc > NIR] : 0.9971          
                                          
                  Kappa : -0.2568         
 Mcnemar's Test P-Value : 0.8897          
                                          
            Sensitivity : 0.3250          
            Specificity : 0.4186          
         Pos Pred Value : 0.3421          
         Neg Pred Value : 0.4000          
             Prevalence : 0.4819          
         Detection Rate : 0.1566          
   Detection Prevalence : 0.4578          
      Balanced Accuracy : 0.3718          
                                          
       'Positive' Class : M         





# Calculate class probabilities: p_class

p_class <-
  ifelse(p > 0.50,
         "M",
         "R"
  )

# Create confusion matrix

confusionMatrix(p_class, test$Class)


#Calculating true postive rate or sensitivity
#Calculating true negative rate or Specificity



#Class probabilities and class predictions
 - Predicted classes are based off of predicted probabilities plus a classification threshold.



# Apply threshold of 0.9: p_class

p_class <- ifelse(p > .9 , "M", "R")

# Create confusion matrix

confusionMatrix(p_class, test[["Class"]])



#Introducing the ROC curve

ROC Curves is a plot between  Sensitivity and Specificity  

library(caTools)
colAUC(p, test$Class, plotROC = TRUE)

each point along the curve represents a differente threshold



# Predict on test: p

p = predict(model, test, type = "response")


# Make ROC curve

colAUC(p, test$Class, plotROC = TRUE)

#Area under the curve (AUC)

it is a single-number summary of model Accuracy


Caret functions
# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = ___,
  summaryFunction = defaultSummary,
  classProbs = ___, # IMPORTANT!
  verboseIter = TRUE
)



# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = ___,
  summaryFunction = twoClassSummary ,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)




# Train glm with custom trainControl: model

trainCongtrol = trainControl(
    method = "cv", number = 5,
    repeats = 5, verboseIter = TRUE
  )

model <- train(
  Class ~ ., Sonar,
  method = "glm",
  trControl = myControl
)

# Print model to console

model


#Random forests


very accurate non-linear model


library(caret)
library(mlbench)
data(Sonar)
set.seed(42)

model = train(Class ~ . , data = Sonar, method = "ranger")

plot(model)


# Fit random forest: model
model <- train(
  quality ~ .,
  tuneLength = 1,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model


# All ok but randon forest requires tunning

select better hiperparameters
Grid Search  - caret does it for you


What's the advantage of a longer tuneLength
- You explore more potential models and can potentially find a better model.


# Fit random forest: model
model <- train(
  quality ~ .,
  tuneLength = 3,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console

model

# Plot model


plot(model)


#Custom tunning grids

Why use a custom tuneGrid?
 - It gives you more fine-grained control over the tuning parameters that are explored.


 # Fit random forest: model
model <- train(
   quality ~ . ,
  tuneGrid = data.frame(mtry = 2:13),
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Plot model

plot(model)


# Fit random forest: model
model <- train(
   quality ~ . ,
  tuneGrid = data.frame(mtry = c(2,3,7)),
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Plot model

plot(model)


#GLMNET models
#lasso and ridge regression or mix of both
#alpha [0,1] 0 pure lasso 1 pure ridge
#lambda (o,inf) size of penalty

#### places constraints on the magnitude of the coefficients to prevent overfitting  ####

#dont overfit dataset
#https://www.kaggle.com/c/overfitting/data

data_link = "https://www.kaggle.com/c/overfitting/download/overfitting.csv"
data_link = "C://RData/overfitting.csv"

overfit = read.csv(data_link)

myControl = trainControl(
    method = "cv",
    number = 10,
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE
  )

set.seed(42)

model = train(
    y ~ .,
    overfit,
    method = "glmnet",
    trControl = myControl
  )
 

# Make a custom trainControl 
# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary ,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)



# Fit glmnet model: model
model <- train(
  y ~ ., overfit,
  method = "glmnet",
  trControl = myControl
)

# Print model to console

model

# Print maximum ROC statistic

max(model$results)



#glmnet with custom tuning grid

CUstom tugging grig

myGrid = expand.grid( alpha = 0:1 , lambda = seq(0.001, 0.1, length = 10) )

set.seed(42)

model = train(
    Class ~ .,
    Sonar,
    method = "glmnet",
    tuneGrid = myGrid,
    trControl = myControl
  )

plot(model)

plot(model$finalModel)


As you saw in the video, the glmnet model actually fits many models at once (one of the great things about the package). You can exploit this by passing a large number of lambda values, which control the amount of penalization in the model. train() is smart enough to only fit one model per alpha value and pass all of the lambda values at once for simultaneous fitting.

expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 100))


If you want to explore fewer models, you can use a shorter lambda sequence. For example, lambda = seq(0.0001, 1, length = 10) would fit 10 models per value of alpha.

You also look at the two forms of penalized models with this tuneGrid: ridge regression and lasso regression. alpha = 0 is pure ridge regression, and alpha = 1 is pure lasso regression. You can fit a mixture of the two models (i.e. an elastic net) using an alpha between 0 and 1. For example, alpha = .05 would be 95% ridge regression and 5% lasso regression.

In this problem you'll just explore the 2 extremes--pure ridge and pure lasso regression--for the purpose of illustrating their differences

# Train glmnet with custom trainControl and tuning: model
model <- train(
  y ~., overfit,
  tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 100)),
  method = "glmnet",
  trControl = myControl
)

# Print model to console

model

# Print maximum ROC statistic

max(model[["results"]][["ROC"]])


# Train glmnet with custom trainControl and tuning: model
model <- train(
  y ~., overfit,
  tuneGrid = expand.grid(alpha = 0:1, lambda = seq(0.0001, 1, length = 20)),
  method = "glmnet",
  trControl = myControl
)

# Print model to console

model

# Print maximum ROC statistic

max(model[["results"]][["ROC"]])


#Median Imputation
#Missing Data problem

data(mtcars)
set.seed(42)
mtcars[sample(1:nrow(mtcars),10), "hp"] = NA

X = mtcars$mpg
Y = mtcars[, 2:4]

library(caret)
model = train(X, Y)

model = train(X, Y, preProcess = "medianImpute")



# Apply median imputation: model
model <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = "medianImpute"
)

# Print model to console

model


#Median imputation is not always good

Knn imputation


data(mtcars)
mtcars[mtcars$disp < 140 , "hp"] = NA
Y = mtcars$mpg
X = mtcars[, 2:4]
set.seed(42)
model = train(
      X, Y,
      method = "glm",
      preProcess = "medianImpute"
    )

print(min(model$result$RMSE))


  data(mtcars)
  mtcars[mtcars$disp < 140 , "hp"] = NA
  Y = mtcars$mpg
  X = mtcars[, 2:4]
  set.seed(42)
  model = train( X, Y, method = "glm", preProcess = "knnImpute")

  print(min(model$result$RMSE))


  # Apply KNN imputation: model2
model2 <- train(
  x = breast_cancer_x , y = breast_cancer_y,
  method =  "glm",
  trControl = myControl,
  preProcess = "knnImpute"
)

# Print model to console

model2



#preProcess  parameters

do more than just imputation

imputation -> center -> scale fit glm model

For instance PCA always happens after center and scaling


data(mtcars)
mtcars[sample(1:nrow(mtcars), 10) , "hp"] = NA
Y = mtcars$mpg
X = mtcars[, 2:4]
set.seed(42)

model = train( X, Y, method = "glm", preProcess = c("medianImpute", "center", "scale"))

print(min(model$result$RMSE))


data(mtcars)
mtcars[sample(1:nrow(mtcars), 10) , "hp"] = NA
Y = mtcars$mpg
X = mtcars[, 2:4]
set.seed(42)

model = train( X, Y, method = "glm", preProcess = c("medianImpute", "center", "scale", "pca"))

print(min(model$result$RMSE))


data(mtcars)
mtcars[sample(1:nrow(mtcars), 10) , "hp"] = NA
Y = mtcars$mpg
X = mtcars[, 2:4]
set.seed(42)

model = train( X, Y, method = "glm", preProcess = c("medianImpute", "center", "scale", "spatialSign"))

print(min(model$result$RMSE))

Sheet cheat
 - medianImpute  and try Knn
 - for linear models (lm, glm, glmnet) always center and scale
 - try pca and spatialSign
 tree based models dont need much preprossing (just median imputation is ok)



# Fit glm with median imputation: model1
model1 <- train(
  x = breast_cancer_x, y = breast_cancer_y,
  method = "glm",
  trControl = myControl,
  preProcess = c("medianImpute")
)

# Print model1

model1


# Fit glm with median imputation and standardization: model2
model2 <- train(
  x = breast_cancer_x , y = breast_cancer_y  ,
  method = "glm",
  trControl = myControl,
  preProcess = c("medianImpute", "center", "scale")
)

# Print model2
model2


#handling Low Information predictors

Remove constant columns
Remove nearly constant columns

Constant value, SD = 0 , and the try to scale by divide by SD u got missing values


data(mtcars)
mtcars[sample(1:nrow(mtcars), 10) , "hp"] = NA
Y = mtcars$mpg
X = mtcars[, 2:4]
set.seed(42)

model = train( X, Y, method = "glm", preProcess = c("zv", "medianImpute", "center", "scale", "pca"))

print(min(model$result$RMSE))

Remove near zero variance predictors

log((concentration of compound in brain) / (concentration of compound in blood))


# Identify near zero variance predictors: remove_cols
remove_cols <- nearZeroVar(bloodbrain_x, names = TRUE, freqCut = 2, uniqueCut = 20)

# Get all column names from bloodbrain_x: all_cols

all_cols = names(bloodbrain_x)

# Remove from data: bloodbrain_x_small
bloodbrain_x_small <- bloodbrain_x[ , setdiff(all_cols, remove_cols)]

bloodbrain_x_small




#Principle components analysis (PCA)

very IMPORTANT preprossing step on LM like models

combines low variance and correlated variables


data(BloodBrain)

names(bbbDescr)

names(bbbDescr)[nearZeroVar(bbbDescr)]

data(BloodBrain)
set.seed(42)
model = train( bbbDescr, logBBB, method = "glm",
    trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
    preProcess = c("zv", "center", "scale")
   )
print(min(model$result$RMSE))

data(BloodBrain)
set.seed(42)
model = train( bbbDescr, logBBB, method = "glm",
    trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
    preProcess = c("nzv", "center", "scale")
   )
print(min(model$result$RMSE))


data(BloodBrain)
set.seed(42)
model = train( bbbDescr, logBBB, method = "glm",
    trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
    preProcess = c("zv", "center", "scale", "pca")
   )
print(min(model$result$RMSE))



# Fit glm model using PCA: model
model <- train(
  x = bloodbrain_x , y = bloodbrain_y,
  method = "glm", preProcess = "pca"
)

# Print model to console


model



#Reusing a trainControl


# Create custom indices: myFolds
myFolds <- createFolds(churn_y, k = 5)

# Create reusable trainControl object: myControl
myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)


#GLMNET model
linear  model with built-in variable selection
very good baseline model

library(caret)
library(C50)
data(churn)
set.seed(42)

myFolds = createFolds(
    churnTrain$churn, k = 5
  )

myControl <- trainControl(
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = TRUE,
  index = myFolds
)


model_glmnet = train(
  churn ~. , 
  churnTrain,
  metric = "ROC",
  method = "glmnet",
  tuneGrid = expand.grid(alpha = 0:1, lambda = 0:10/10 ),
  trControl = myControl
  ) 

plot(model_glmnet)



# Fit glmnet model: model_glmnet
model_glmnet <- train(
  x = churn_x, y = churn_y,
  metric = "ROC",
  method = "glmnet",
  trControl = myControl
)



#Reintroduce Randon Forest
after glmnet is the Second Model to always try
less interpretable
but more Accuracy
easier to tune
Require little preProcessing
Capture threshold effects and variable interactions



set.seed(42)
churnTrain$churn = factor(churnTrain$churn, levels = c("no", "yes"))
model_rf = train(
  churn ~. , 
  churnTrain,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
  ) 

plot(model_rf)



# Fit random forest: model_rf
model_rf <- train(
  x = churn_x , y = churn_y,
  metric = "ROC",
  method = "ranger",
  trControl = myControl
)

#Comparing models

Highest average AUC
Lowest SD in AUC

resamples() function


# Create model_list
model_list <- list(model_glmnet  = model_glmnet, model_rf  = model_rf)

# Pass model_list to resamples(): 

resamples = resamples(model_list)

# Summarize the results


summary(resamples)

# Create model_list
model_list <- list(item1 = model_glmnet, item2 = model_rf)

# Pass model_list to resamples(): resamples
resamples <- resamples(model_list)

# Summarize the results
summary(resamples)


#More on Resamples

caretEnsemble package

install.packages("caretEnsemble")
library(caretEnsemble)
bwplot(resamps, metric = "ROC")
dotplot(resamps, metric = "ROC")
densityplot(resamps, metric = "ROC")
xyplot(resamps, metric = "ROC")


#Create a box-and-whisker plot
 - allows you to compare the distribution of predictive accuracy (in this case AUC) for the two models.
 - In general, you want the model with the higher median AUC, as well as a smaller range between min and max AUC.
 - bwplot()


 # Create bwplot
bwplot(resamples, metric = "ROC")
# Create xyplot
xyplot(resamples, metric = "ROC")


#Ensembling models

caretStack()

# Create ensemble model: stack
stack <- caretStack(model_list , method = "glm")

# Look at summary
summary(stack)





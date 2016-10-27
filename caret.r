caret reference: http://topepo.github.io/caret/pre-processing.html#dummy

inTrain <- createDataPartition(y = Sonar$Class, p = .75, list = FALSE)

training <- Sonar[ inTrain,]
testingng <- Sonar[ -inTrain,]

plsFit <- train(Class ~ ., data = training, method = "pls", preProc = c("center", "scale"))

plsFit <- train(Class ~ ., data = training, method = "pls", preProc = c("center", "scale"), tuneLength = 15)

tuneLength

tuneGrid

trainControl

method

ctrl <- trainControl(method = "repeatedcv", repeats = 3)

plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15, trControl = ctrl, preProc = c("center", "scale"))

summaryFunction
  - defaultSummary and twoClassSummary

classProbs = TRUE

ctrl <- trainControl(method = "repeatedcv", repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

plsFit <- train(Class ~ ., data = training, method = "pls", tuneLength = 15, trControl = ctrl, metric = "ROC", preProc = c("center", "scale"))

plsFit

plot(plsFit)


plsClasses <- predict(plsFit, newdata = testing)
str(plsClasses)


plsProbs <- predict(plsFit, newdata = testing, type = "prob")
head(plsProbs)

confusionMatrix(data = plsClasses, testing$Class)


to fit a regularized discriminant model to these data,

rdaGrid = 15
rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ ., data = training, method = "rda", tuneGrid = rdaGrid, trControl = ctrl, metric = "ROC")
rdaFit


rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

xyplot(resamps, what = "BlandAltman")

diffs <- diff(resamps)
summary(diffs)


#CARET

library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)
featurePlot(x = iris[, 1:4], y = iris$Species, plot = "pairs", auto.key = list(columns = 3))



featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))



transparentTheme(trans = .9)
featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))


featurePlot(x = iris[, 1:4],  
            y = iris$Species, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))



#Regression


library(mlbench)
data(BostonHousing)
regVar <- c("age", "lstat", "tax")
str(BostonHousing[, regVar])


featurePlot(x = BostonHousing[, regVar], 
            y = BostonHousing$medv, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 1))



#Zero- and Near Zero-Variance Predictors

data(mdrr)
data.frame(table(mdrrDescr$nR11))

nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

dim(mdrrDescr)


nzv <- nearZeroVar(mdrrDescr)
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)



Identifying Correlated Predictors


highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])


#The preProcess Function

set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,]
trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)

# Imputation


library(AppliedPredictiveModeling)
data(schedulingData)
str(schedulingData)


pp_hpc <- preProcess(schedulingData[, -8], 
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc

transformed <- predict(pp_hpc, newdata = schedulingData[, -8])
head(transformed)



library(randomForest)
library(mlbench)
library(caret)
 
# Load Dataset
data(Sonar)
dataset <- Sonar
x <- dataset[,1:60]
y <- dataset[,61]

# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
ranger_default <- train(Class~., data=dataset, method="ranger", metric=metric, tuneGrid=tunegrid, trControl=control)

print(rf_default)

defaults for each parameter and mtry=floor(sqrt(ncol(x))) or mtry=7 and ntree=500.

#Tune Using Caret

# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(Class~., data=dataset, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


#Grid Search

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


#Tune Using Algorithm Tools


# Algorithm Tune (tuneRF)
set.seed(seed)
bestmtry <- tuneRF(x, y, stepFactor=1.5, improve=1e-5, ntree=500)
print(bestmtry)


You can see that the most accurate value for mtry was 10 with an OOBError of 0.1442308.

#Craft Your Own Parameter Search

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(x))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(Class~., data=dataset, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)




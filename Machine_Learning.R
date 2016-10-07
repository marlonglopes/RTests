# The Wage dataset is available

# Build Linear Model: lm_wage (coded already)
lm_wage <- lm(wage ~ age, data = Wage)

# Define data.frame: unseen (coded already)
unseen <- data.frame(age = 60)

# Predict the wage for a 60-year old worker
predict(lm_wage, unseen)


# The emails dataset is already loaded into your workspace

# Show the dimensions of emails
dim(emails)

# Inspect definition of spam_classifier()
spam_classifier <- function(x){
  prediction <- rep(NA, length(x)) # initialize prediction vector
  prediction[x > 4] <- 1
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(prediction) # prediction is either 0 or 1
}

# Apply the classifier to the avg_capital_seq column: spam_pred

spam_pred = spam_classifier(emails$avg_capital_seq)

# Compare spam_pred to emails$spam. Use ==

emails$spam == spam_pred


# linkedin is already available in your workspace

# Create the days vector

days = (1:21)
linkedin = c(5  7  4  9 11 10 14 17 13 11 18 17 21 21 24 23 28 35 21 27 23)
# Fit a linear model called on the linkedin views per day: linkedin_lm

 linkedin_lm = lm(linkedin ~ days)

# Predict the number of views for the next three days: linkedin_pred
future_days <- data.frame(days = 22:24)
linkedin_pred <- predict.lm(linkedin_lm, future_days)

# Plot historical data and predictions
plot(linkedin ~ days, xlim = c(1, 24))
points(22:24, linkedin_pred, col = "green")



# Set random seed. Don't remove this line.
set.seed(1)

# Chop up iris in my_iris and species
my_iris <- iris[-5]
species <- iris$Species

# Perform k-means clustering on my_iris: kmeans_iris


kmeans_iris = kmeans(my_iris, 3)

# Compare the actual Species to the clustering using table()

table(species, kmeans_iris$cluster)

# Plot Petal.Width against Petal.Length, coloring by cluster
plot(Petal.Length ~ Petal.Width, data = my_iris, col = kmeans_iris$cluster)


ggplot(my_iris, aes(Petal.Length , Petal.Width, col = kmeans_iris$cluster))


# Set random seed. Don't remove this line.
set.seed(1)

# Take a look at the iris dataset

str(iris)
summary(iris)

# A decision tree model has been built for you
tree <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
              data = iris, method = "class")

# A dataframe containing unseen observations
unseen <- data.frame(Sepal.Length = c(5.3, 7.2),
                     Sepal.Width = c(2.9, 3.9),
                     Petal.Length = c(1.7, 5.4),
                     Petal.Width = c(0.8, 2.3))

# Predict the label of the unseen observations. Print out the result.

predict(tree, unseen, type = "class")


# The cars data frame is pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Explore the cars dataset

str(cars)
summary(cars)

# Group the dataset into two clusters: km_cars

km_cars = kmeans(cars, 2)

# Print out the contents of each cluster
km_cars$cluster



# The cars data frame is pre-loaded

# Set random seed. Don't remove this line
set.seed(1)

# Group the dataset into two clusters: km_cars
km_cars <- kmeans(cars, 2)

# Add code: color the points in the plot based on the clusters
plot(cars, col = km_cars$cluster)

# Print out the cluster centroids
km_cars$centers

# Replace the ___ part: add the centroids to the plot
points(km_cars$centers, pch = 22, bg = c(1, 2), cex = 2)


# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Have a look at the structure of titanic
str(titanic)

# A decision tree classification model is built on the data
tree <- rpart(Survived ~ ., data = titanic, method = "class")

# Use the predict() method to make predictions, assign to pred

pred = predict(tree, titanic, type = "class")

# Use the table() method to make the confusion matrix

table(titanic$Survived, pred)


# The confusion matrix is available in your workspace as conf

# Assign TP, FN, FP and TN using conf
TP <- conf[1, 1] # this will be 212
FN <- conf[1, 2] # this will be 78
FP <- conf[2, 1] # fill in
TN <- conf[2, 2] # fill in

# Calculate and print the accuracy: acc


acc = (TP + TN) / sum(conf)
acc

# Calculate and print out the precision: prec

prec = TP / (TP + FP)
prec

# Calculate and print out the recall: rec

rec = TP / (TP + FN)
rec 


# The air dataset is already loaded into your workspace

# Take a look at the structure of air
str(air)

# Inspect your colleague's code to build the model
fit <- lm(dec ~ freq + angle + ch_length, data = air)

# Use the model to predict for all values: pred

pred = predict(fit, air)

# Use air$dec and pred to calculate the RMSE 

rmse = sqrt((1/nrow(air)) * sum( (air$dec - pred) ^ 2))

# Print out rmse

rmse

# The air dataset is already loaded into your workspace

# Previous model
fit <- lm(dec ~ freq + angle + ch_length, data = air)
pred <- predict(fit)
rmse <- sqrt(sum( (air$dec - pred) ^ 2) / nrow(air))
rmse

# Your colleague's more complex model
fit2 <- lm(dec ~ freq + angle + ch_length + velocity + thickness, data = air)

# Use the model to predict for all values: pred2

pred2 = predict(fit2, air)

# Calculate 
rmse2 = sqrt(sum( (air$dec - pred2) ^ 2) / nrow(air))


# Print out 
rmse2


# The seeds dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Explore the structure of the dataset
str(seeds)

# Group the seeds in three clusters
km_seeds <- kmeans(seeds, 3)

# Color the points in the plot based on the clusters
plot(length ~ compactness, data = seeds, col = km_seeds$cluster)

# Print out the ratio of the WSS to the BSS

km_seeds

km_seeds$tot.withinss 
km_seeds$betweenss

km_seeds$tot.withinss / km_seeds$betweenss



# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(titanic)
shuffled <- titanic[sample(n),]

# Split the data in train and test

train_indices <- 1:round(0.7 * n)
train <- shuffled[train_indices, ]

test_indices <- (round(0.7 * n) + 1):n
test <- shuffled[test_indices, ]

# Print the structure of train and test
str(test)
str(train)


# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset; build train and test
n <- nrow(titanic)
shuffled <- titanic[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Fill in the model that has been learned.
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the outcome on the test set with tree: pred

pred = predict(tree, test, type = "class")

# Calculate the confusion matrix: conf

conf = table(test$Survived, pred)

# Print this confusion matrix

conf

   pred
      1   0
  1  58  31
  0  23 102

accuracy of (58+102)/(58+31+23+102) = 74.76%


# The shuffled dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs <- rep(0,6)

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Survived ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree

  pred = predict(tree, test, type = "class")
  
  # Assign the confusion matrix to conf

  conf = table(test$Survived, pred)
  
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs

accs
mean(accs)


# The shuffled dataset is already loaded into your workspace
library(caret)

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs <- rep(0,6)

folds <- createFolds(shuffled$Survived, 6)

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- folds[[i]]
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Survived ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree

  pred = predict(tree, test, type = "class")
  
  # Assign the confusion matrix to conf

  conf = table(test$Survived, pred)
  
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs

accs
mean(accs)


# The shuffled dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs <- rep(0,6)

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Survived ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree

  pred = predict(tree, test, type = "class")
  
  # Assign the confusion matrix to conf

  conf = table(test$Survived, pred)
  
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs

accs
mean(accs)



BIAS
wrong assumptions
different predictions and truth


VARIANCE
error due to the sample of the training set
it fits training set closely




# The spam filter that has been 'learned' for you
spam_classifier <- function(x){
  prediction <- rep(NA, length(x)) # initialize prediction vector
  prediction[x > 4] <- 1 
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(factor(prediction, levels = c("1", "0"))) # prediction is either 0 or 1
}

# Apply spam_classifier to emails_full: pred_full

pred_full = spam_classifier(emails_full$avg_capital_seq)

# Build confusion matrix for emails_full: conf_full

conf_full = table(emails_full$spam, pred_full)

# Calculate the accuracy with conf_full: acc_full

acc_full = sum(diag(conf_full)) / sum(conf_full)

# Print 
acc_full






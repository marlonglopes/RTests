t test

are two groups diffferents???


model =  representation of a purpose


test_scores(school = "private",  acad_motivation = 1,  relig_motivation = 3)


# Baseline run
test_scores(school = "public", acad_motivation = 0, relig_motivation = 0)

# Change school input, leaving others at baseline
test_scores(school = "private", acad_motivation = 0, relig_motivation = 0)

# Change acad_motivation input, leaving others at baseline
test_scores(school = "public", acad_motivation = 1, relig_motivation = 0)

# Change relig_motivation input, leaving others at baseline
test_scores(school = "public", acad_motivation = 0, relig_motivation = 1)

# Use results above to estimate output for new inputs
my_prediction <- 100 - 5 + 2 * 15 + 2 * 0
my_prediction

# Check prediction by using test_scores() directly
test_scores(school = "private", acad_motivation = 2, relig_motivation = 2)



#In this course, you'll work with datasets drawn from the statisticalModeling and mosaicData packages. These packages will usually have been preloaded in your workspace on DataCamp, but when working on your own machine you will need to load them explicitly with library(). Of course, the techniques covered in this course are applicable to data broadly, not just the examples used in the course.

# Find the variable names in AARP
names(AARP)

# Find the mean cost broken down by sex
mosaic::mean(Cost ~ Sex, data = AARP)


library(statisticalModeling)

#Steps to evaluate models
1 aquiring data (a suitable training data set)
2 specify response and explanatory variables
3 Select  a model architecture (lm, glm, cluster, tree (rpart - recursive partitioning) ...)



# Find the variable names in Runners 

names(Runners)

# Build models: handicap_model_1, handicap_model_2, handicap_model_3 
handicap_model_1 <- lm(net ~ age, data = Runners)
handicap_model_2 <- lm(net ~ sex, data = Runners)
handicap_model_3 <- lm(net ~ age + sex, data = Runners)

# For now, here's a way to visualize the models
fmodel(handicap_model_1)
fmodel(handicap_model_2)
fmodel(handicap_model_3)


# Load rpart
library(rpart)

# Build rpart model: model_2

model_2 = rpart(net ~ age + sex, data = Runners, cp = 0.002)

# Examine graph of model_2 (don't change)
fmodel(model_2, ~ age + sex)


# Create 
model_1 = lm(runs_again ~ . ,data = Ran_twice)

run_again_model = rpart(runs_again ~  age + sex + net, data = Ran_twice, cp = 0.005)

# Visualize the model (don't change)
fmodel(run_again_model, ~ age + net, data = Ran_twice)



#Evaluating Models

# Display the variable names in the AARP data frame

names(AARP)

# Build a model: 

insurance_cost_model = lm(Cost ~ Age + Sex + Coverage, data = AARP)

# Construct a data frame: example_vals 

example_vals = data.frame(Age = 60 , Sex = "F" , Coverage = 200 )

# Predict insurance cost using predict()

predict(insurance_cost_model, example_vals)

# Load statisticalModeling

library(statisticalModeling)

# Calculate model output using evaluate_model()

evaluate_model(insurance_cost_model, example_vals)



# Build a model: insurance_cost_model
insurance_cost_model <- lm(Cost ~ Age + Sex + Coverage, data = AARP)

# Create a data frame: new_inputs_1
new_inputs_1 <- data.frame(Age = c(30, 90), Sex = c("F", "M"), 
                           Coverage = c(0, 100))

# Use expand.grid(): new_inputs_2
new_inputs_2 <- expand.grid(Age = c(30, 90), Sex = c("F", "M"), 
                           Coverage = c(0, 100))

# Use predict() for new_inputs_1 and new_inputs_2
predict(insurance_cost_model, newdata = new_inputs_1)
predict(insurance_cost_model, newdata = new_inputs_2)

# Use evaluate_model() for new_inputs_1 and new_inputs_2
evaluate_model(insurance_cost_model, data = new_inputs_1)
evaluate_model(insurance_cost_model, data = new_inputs_2)


# Evaluate insurance_cost_model
evaluate_model(insurance_cost_model)

# Use fmodel() to reproduce the graphic
fmodel(insurance_cost_model, ~ Coverage + Age + Sex)

# A new formula to highlight difference in sexes
new_formula <- ~ Age + Sex + Coverage

# Make the new plot (don't change)
fmodel(insurance_cost_model, new_formula)





# Build a model of net running time
base_model <- lm(net ~ age + sex, data = Runners_100)

# Evaluate base_model on the training data
base_model_output <- predict(base_model, newdata = Runners_100)

# Build the augmented model
aug_model <- lm(net ~ age + sex + previous, data = Runners_100)

# Evaluate aug_model on the training data
aug_model_output <- predict(aug_model, newdata = Runners_100)

# How much do the model outputs differ?
mean((base_model_output - aug_model_output) ^ 2, na.rm = TRUE)



# Build and evaluate the base model on Runners_100
base_model <- lm(net ~ age + sex, data = Runners_100)
base_model_output <- predict(base_model, newdata = Runners_100)

# Build and evaluate the augmented model on Runners_100
aug_model <- lm(net ~ age + sex + previous, data = Runners_100)
aug_model_output <- predict(aug_model, data = Runners_100)

# Find the case-by-case differences
base_model_differences <- with(Runners_100, net - base_model_output)
aug_model_differences <- with(Runners_100, net - aug_model_output)

# Calculate mean square errors
mean(base_model_differences ^ 2)
mean(aug_model_differences ^ 2)



# Add bogus column to CPS85 (don't change)
CPS85$bogus <- rnorm(nrow(CPS85)) > 0

# Make the base model
base_model <- lm(wage ~ educ + sector + sex, data = CPS85)

# Make the bogus augmented model
aug_model <- lm(wage ~ educ + sector + sex + bogus, data = CPS85)

# Find the MSE of the base model
mean((CPS85$wage - predict(base_model, newdata = CPS85)) ^ 2)

# Find the MSE of the augmented model
mean((CPS85$wage - predict(aug_model, newdata = CPS85)) ^ 2)


# Generate a random TRUE or FALSE for each case in Runners_100
Runners_100$training_cases <- rnorm(nrow(Runners_100)) > 0

# Build base model net ~ age + sex with training cases
base_model <- lm(net ~ age + sex, data = subset(Runners_100, training_cases))

# Evaluate the model for the testing cases
Preds <- evaluate_model(base_model, data = subset(Runners_100, !training_cases))

# Calculate the MSE on the testing data
with(data = Preds, mean((net - model_output)^2))


# The base model
base_model <- lm(net ~ age + sex, data = Runners_100)

# An augmented model adding previous as an explanatory variable
aug_model <- lm(net ~ age + sex + previous, data = Runners_100)

# Run cross validation trials on the two models
trials <- cv_pred_error(base_model, aug_model)

# Compare the two sets of cross-validated errors
t.test(mse ~ model, data = trials)



#Prediction Error for categorical response variables

model_1 	= rpart(Species ~ Sepal.Length + Sepal.Width, data = iris)
result 		= predict(model_1, data = iris, type = "class")
with(data = iris, mean(Species != result))
result_prob = predict(model_1, data = iris, type = "prob")
res_prob_1 	= data.frame(actual = iris$Species, result_prob)

summarizing all cases with likelihood

likelihood_a = with(res_prob_1, ifelse(actual == "virginica", Maried, Sindle))
sum(log(likelihood_a))


# Build the null model with rpart()
Runners$all_the_same <- 1 # null "explanatory" variable
null_model <- rpart(start_position ~ all_the_same, data = Runners)

# Evaluate the null model on training data
null_model_output <- evaluate_model(null_model, data = Runners, type = "class")

# Calculate the error rate
with(data = null_model_output, mean(start_position != model_output, na.rm = TRUE))

# Generate a random guess...
null_model_output$random_guess <- mosaic::shuffle(Runners$start_position)

# ...and find the error rate
with(data = null_model_output, mean(start_position != random_guess, na.rm = TRUE))


# Train the model
model <- rpart(start_position  ~ age + sex, data = Runners, cp = 0.001)

# Get model output with the training data as input
model_output <- evaluate_model(model, data = Runners, type = "class")

# Find the error rate
with(data = model_output, mean(start_position != model_output, na.rm = TRUE))

# Train the models 
null_model <- rpart(start_position ~ all_the_same,
                    data = Training_data, cp = 0.001)
model_1 <- rpart(start_position ~ age, 
                 data = Training_data, cp = 0.001)
model_2 <- rpart(start_position ~ age + sex, 
                 data = Training_data, cp = 0.001)

# Find the out-of-sample error rate
null_output <- evaluate_model(null_model, data = Testing_data, type = "class")
model_1_output <- evaluate_model(model_1, data = Testing_data, type = "class")
model_2_output <- evaluate_model(model_2, data = Testing_data, type = "class")

# Calculate the error rates
null_rate    <- with(data = null_output, mean(start_position != null_output, na.rm = TRUE))
model_1_rate <- with(data = Testing_data, mean(start_position != model_1_output, na.rm = TRUE))
model_2_rate <- with(data = Testing_data, mean(start_position != model_2_output, na.rm = TRUE))

# Display the error rates
null_rate
model_1_rate
model_2_rate


# Train the models 
null_model 	<- rpart(start_position ~ all_the_same, data = Training_data, cp = 0.001)
model_1 	<- rpart(start_position ~ age, data = Training_data, cp = 0.001)
model_2 	<- rpart(start_position ~ age + sex, data = Training_data, cp = 0.001)

# Find the out-of-sample error rate
null_output 	<- evaluate_model(null_model, data = Testing_data, type = "class")
model_1_output 	<- evaluate_model(model_1, data = Testing_data, type = "class")
model_2_output 	<- evaluate_model(model_2, data = Testing_data, type = "class")

# Calculate the error rates
null_rate 		<- with(data = null_output, mean(start_position != model_output, na.rm = TRUE))
model_1_rate 	<- with(data = model_1_output, mean(start_position != model_output, na.rm = TRUE))
model_2_rate 	<- with(data = model_2_output, mean(start_position != model_output, na.rm = TRUE))

# Display the error rates
null_rate
model_1_rate
model_2_rate
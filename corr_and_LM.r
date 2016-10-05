# The vectors `A` and `B` have already been loaded

# Take a quick peek at both vectors
A = c(1,2,4)
B = c(3,6,7)

N = length(A)-1

# Save the differences of each vector element with the mean in a new variable
diff_A <- A - mean(A)
diff_B <- B - mean(B)

# Do the summation of the elements of the vectors and divide by N-1 in order to acquire the covariance between the two vectors
cov <- sum(diff_A*diff_B) / N
cov
cov(A,B)  #R function

# Your workspace still contains the results of the previous exercise

# Square the differences that were found in the previous step
sq_diff_A <- diff_A^2
sq_diff_B <- diff_B^2

# Take the sum of the elements, divide them by N-1 and consequently take the square root to acquire the sample standard deviations
sd_A <- sqrt(sum(sq_diff_A)/(N))
sd_B <- sqrt(sum(sq_diff_B)/(N))

# Your workspace still contains the results of the previous exercise

# Combine all the pieces of the puzzle
correlation <- cov / (sd_A * sd_B)
correlation

# Check the validity of your result with the cor() command

cor(A,B) #R function



Regression 

y = m + bx + e

y = linear function  of x
m = interception
b = slope
e = error (residual)

Regression 

Y = B0 + b1X1 + e

y = linear function  of X1
B0 = interception = Regression CONSTANT
B1 = slope = Regression COEFFICIENT
e = error (residual)




###################################

#build regression equation

#For our simple regression model we will take Symptom Score (sym2) as our dependent variable
#and Impulse Control (ic2) as our independent variable. These are variables from the impact dataset, which is still loaded in your workspace.

#The regression equation would then be

# The dataset `impact` is already loaded.

# Calculate the required means, standard deviations and correlation coefficient
mean_sym2 <- mean(impact$sym2)
mean_ic2 <- mean(impact$ic2)
sd_sym2 <- sd(impact$sym2)
sd_ic2 <- sd(impact$ic2)
r <- cor(impact$ic2,impact$sym2)

# Calculate the slope
B_1 <- r * (sd_sym2 )/( sd_ic2 )

# Calculate the intercept
B_0 <- mean_sym2 - B_1 * mean_ic2

# Plot of ic2 against sym2
plot(impact$ic2,impact$sym2, main = "Scatterplot", ylab = "Symptoms", xlab = "Impulse Control")

# Add the regression line
abline(B_0, B_1, col = "red")

######Equivalente###############################################################################################

# The dataset impact is still loaded

# Construct the regression model
model_1 <- lm(sym2 ~ ic2, data = impact)

# Look at the results of the regression by using the summary function

summary(model_1)

# Create a scatter plot of Impulse Control against Symptom Score
plot(impact$sym2 ~ impact$ic2, main = "Scatterplot", ylab = "Symptoms", xlab = "Impulse Control")

# Add a regression line
abline(model_1, col = "red")


##################################################


# The impact dataset is already loaded 

# Multiple Regression
model_2 <- lm(impact$sym2 ~ impact$ic2 + impact$vermem2)

# Examine the results of the regression
summary(model_2)

# Extract the predicted values
predicted <- fitted(model_2)

# Plotting predicted scores against observed scores
plot(predicted ~ impact$sym2, main = "Scatterplot", ylab = "Predicted Scores", xlab = "Observed Scores")
abline(lm(predicted ~ impact$sym2), col = "green")



##########################################


# The `impact` dataset is already loaded 

# Create a linear regression with `ic2` and `vismem2` as regressors
model_1 <- lm(impact$sym2 ~ impact$ic2 + impact$vismem2)

# Extract the predicted values
predicted_1 <- fitted(model_1)

# Calculate the squared deviation of the predicted values from the observed values 
deviation_1 <- (impact$sym2 - predicted_1)^2

# Sum the squared deviations
SSR_1 <- sum(deviation_1)


# Create a linear regression with `ic2` and `vermem2` as regressors
model_2 <- lm(impact$sym2 ~ impact$ic2 + impact$vermem2)

# Extract the predicted values
predicted_2 <- fitted(model_2)

# Calculate the squared deviation of the predicted values from the observed values 
deviation_2 <- (impact$sym2 - predicted_2)^2

# Sum the squared deviations
SSR_2 <- sum(deviation_2)

#Compare the sum of squared residuals of these two models
SSR_1
SSR_2


######

Assumptions

assumption of homoscedasticity.

Anscombes quartet

Test 
save residuals
 plot against original x
should not have any relationship     =    Homosquedasticity



# Extract the residuals from the model
residual <- resid(model_2)

# Draw a histogram of the residuals
hist(residual)

# Extract the predicted symptom scores from the model
predicted <- fitted(model_2)

# Plot the predicted symptom scores against the residuals
plot(residual ~ predicted, main = "Scatterplot", xlab = "Model 2 Predicted Scores", ylab = "Model 2 Residuals")
abline(lm(residual ~ predicted), col="red")

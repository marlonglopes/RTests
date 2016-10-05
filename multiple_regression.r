# fs is available in your working environment

# Perform the two single regressions and save them in a variable
model_years <- lm(fs$salary ~ fs$years)
model_pubs <- lm(fs$salary ~ fs$pubs)

# Plot both enhanced scatter plots in one plot matrix of 1 by 2
par(mfrow = c(1, 2))
plot(fs$salary ~ fs$years, main = "plot_years", xlab = "years", ylab = "salary")
abline(model_years)
plot(fs$salary ~ fs$pubs, main = "plot_pubs", xlab = "pubs", ylab = "salary")
abline(model_pubs)




# fs is available in your working environment

# Do a single regression of salary onto years of experience and check the output
model_1 <- lm(fs$salary ~ fs$years)
summary(model_1)

# Do a multiple regression of salary onto years of experience and numbers of publications and check the output
model_2 <-lm(fs$salary ~ fs$years + fs$pubs)
summary(model_2)

# Save the R squared of both models in preliminary variables
preliminary_model_1 <- summary(model_1)$r.squared
preliminary_model_2 <- summary(model_2)$r.squared

# Round them off while you save them in new variables
r_squared <- c()
r_squared[1] <- round(preliminary_model_1, digits=3)
r_squared[2] <- round(preliminary_model_2, digits=3)

# Print out the vector to see both R squared coefficients

r_squared


#standardized coeficient

library(QuantPsyc)

lm.beta(model_3)

fs$years   fs$pubs 
0.3227385 0.4798728

#pubs because is the highest


# The raw dataframe `X` is already loaded in.
X

# Construction of 1 by 10 matrix I of which the elements are all 1
I <- matrix(1, 1, 10)

# Compute the row vector of sums
t_mat <- I %*% X


# The data matrix `X` and the row vector of sums (`t_mat`) are saved and can be used.

# Number of observations
n = 10

# Compute the row vector of means 
M <- t_mat * 10^-1

# Construction of 10 by 1 matrix J of which the elements are all 1
J <- matrix(1, 10, 1)

# Compute the matrix of means 
MM <- J %*% M


# The previously generated matrices X, M and MM do not need to be constructed again but are saved and can be used.

# Matrix of deviation scores D 
D <- X - MM


# The previously generated matrices X, M, MM and D do not need to be constructed again but are saved and can be used.

# Sum of squares and sum of cross products matrix
S <-  t(D) %*% D


# The previously generated matrices X, M, MM, D and S do not need to be constructed again but are saved and can be used.
n = 10

# Construct the variance-covariance matrix 
C <- S * (1/n)

# Generate the standard deviations matrix 
SD <- diag(x = diag(C)^(1/2), nrow = 3, ncol = 3)

# Compute the correlation matrix
R <- solve(SD) %*% C  %*% solve(SD)

solve(X) = matrix inversa de X 
B = solve((t(X) * X)) * (t(X) * Y)



# `fs` is available in your working environment

# Create the dummy variables
dept_code <- dummy.code(fs$dept)
  
 dept_code
 
# Merge the dataset in an extended dataframe
extended_fs <- cbind(fs, dept_code)
  
# Look at the extended dataframe
extended_fs
  
# Provide summary statistics
summary(extended_fs)


# `fs` is available in your working environment

# Regress salary against years and publications
model <- lm(fs$salary ~ fs$years + fs$pubs)

# Apply the summary function to get summarized results for model
summary(model)

# Compute the confidence intervals for model
confint(model) 

# Create dummies for the categorical variable fs$dept by using the C() function
dept_code <- C(fs$dept, treatment)

# Regress salary against years, publications and department 
model_dummy <- lm(fs$salary ~ fs$years + fs$pubs + dept_code)

# Apply the summary function to get summarized results for model_dummy
summary(model_dummy)

# Compute the confidence intervals for model_dummy
confint(model_dummy)


# The dataset `fs` and regressions `model` and `model_dummy` are available in your workspace

# Compare model 4 with model3
anova(model, model_dummy)


# The dataframe `fs` is still loaded in

# Number of levels
fs$dept

# Factorize the categorical variable fs$dept and name the factorized variable dept.f
dept.f <- factor(fs$dept)

# Assign the 3 levels generated in step 2 to dept.f
contrasts(dept.f) <-contr.sum(3)

# Regress salary against dept.f
model_unweighted <- lm(fs$salary ~ dept.f)

# Apply the summary() function
summary(model_unweighted)

# Factorize the categorical variable fs$dept and name the factorized variable dept.g
dept.g <- factor(fs$dept)

# Assign the weights matrix to dept.g 
contrasts(dept.g) <-weights

# Regress salary against dept.f and apply the summary() function
model_weighted <- lm(fs$salary ~ dept.g)

# Apply the summary() function
summary(model_weighted)

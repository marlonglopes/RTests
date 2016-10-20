https://github.com/dtkaplan/statisticalModeling


# Build your model
my_model <- rpart(price ~ living_area + bathrooms + pct_college,
                data = Houses_for_sale)

# Graph the model
fmodel(my_model, ~ living_area + bathrooms + pct_college)




# Calculating the GPA
gpa_mod_1 <- lm(gradepoint ~ sid, data = College_grades)

# The GPA for two students
evaluate_model(gpa_mod_1, sid = c("S32115", "S32262"))

# Use effect_size()
effect_size(gpa_mod_1, ~ sid)

# Specify from and to levels to compare
effect_size(gpa_mod_1, ~ sid, sid = "S32115", to = "S32262")

# A better model?
gpa_mod_2 <- lm(gradepoint ~ sid + dept + level, data = College_grades)

# Find difference between the same two students as before
effect_size(gpa_mod_2, ~ sid, sid = "S32115", to = "S32262")

# Calculating the GPA
gpa_mod_1 <- lm(gradepoint ~ sid, data = College_grades)

# The GPA for two students
evaluate_model(gpa_mod_1, sid = c("S32115", "S32262"))

# Use effect_size()
effect_size(gpa_mod_1, ~ sid)

# Specify from and to levels to compare
effect_size(gpa_mod_1, ~ sid, sid = "S32115", to = "S32262")

# A better model?
gpa_mod_2 <- lm(gradepoint ~ sid + dept + level, data = College_grades)

# Find difference between the same two students as before
effect_size(gpa_mod_2, ~ sid, sid = "S32115", to = "S32262")



# Build the model
mod <- lm(Pulse ~ Height + BMI + Gender, data = NHANES)

# Confirm by reconstructing the graphic provided
fmodel(mod, ~ BMI) + ggplot2::ylab("Pulse")

# Find effect size
effect_size(mod, ~ BMI)

# Replot the model
fmodel(mod, ~ BMI) + ggplot2::ylab("Pulse")


#Model output  for categorical response

as classes
as probabilies

married_model = rpart(married ~ educ + sex + age ,  data = CPS85, cp = 0.005)
evaluate_model(married_model, type = "prob", age = c(25, 30), educ = 12, sex = "F")
  educ sex age model_output.Married model_output.Single
1   12   F  25            0.6333333           0.3666667
2   12   F  30            0.7425743           0.2574257

5 years more in age increase in 11% prob of being maried



model_1 <- rpart(start_position ~ age + sex + nruns, 
                 data = Runners, cp = 0.001)
Then, you'll evaluate the model in two different ways:

as_class <- evaluate_model(model_1, type = "class")
as_prob  <- evaluate_model(model_1)

> as_class
   age sex nruns model_output
1   20   M     0        eager
2   40   M     0       mellow
3   60   M     0       mellow
4   20   F     0       mellow
5   40   F     0       mellow
6   60   F     0       mellow
7   20   M     5       mellow
8   40   M     5        eager
9   60   M     5       mellow
10  20   F     5       mellow
11  40   F     5       mellow
12  60   F     5       mellow
13  20   M    10        eager
14  40   M    10        eager
15  60   M    10        eager
16  20   F    10       mellow
17  40   F    10       mellow
18  60   F    10       mellow
> as _prob
unexpected input
1: as _
       ^
> as_prob
   age sex nruns model_output.calm model_output.eager model_output.mellow
1   20   M     0         0.2487047          0.4300518           0.3212435
2   40   M     0         0.2943962          0.3393844           0.3662194
3   60   M     0         0.3390294          0.2515379           0.4094327
4   20   F     0         0.2978723          0.1750172           0.5271105
5   40   F     0         0.2978723          0.1750172           0.5271105
6   60   F     0         0.2978723          0.1750172           0.5271105
7   20   M     5         0.2233010          0.3203883           0.4563107
8   40   M     5         0.3041030          0.4006436           0.2952534
9   60   M     5         0.2987616          0.3219814           0.3792570
10  20   F     5         0.2978723          0.1750172           0.5271105
11  40   F     5         0.2978723          0.1750172           0.5271105
12  60   F     5         0.2978723          0.1750172           0.5271105
13  20   M    10         0.2556008          0.4877800           0.2566191
14  40   M    10         0.2556008          0.4877800           0.2566191
15  60   M    10         0.2556008          0.4877800           0.2566191
16  20   F    10         0.2978723          0.1750172           0.5271105
17  40   F    10         0.2978723          0.1750172           0.5271105
18  60   F    10         0.2978723          0.1750172           0.5271105
> 



# Train this model of start_position
model_1 <- rpart(start_position ~ age + sex + nruns, 
                 data = Runners, cp = 0.001)

# Calculate effect size with respect to sex
effect_size(model_1, ~ sex)

# Calculate effect size with respect to age

effect_size(model_1, ~ age)


# Calculate effect size with respect to nruns
effect_size(model_1, ~ nruns)



  change.calm change.eager change.mellow sex to:sex age nruns
1  0.01281487   -0.2192357     0.2064208   M      F  40     4
> 
> # Calculate effect size with respect to age
> 
> effect_size(model_1, ~ age)
  slope.calm slope.eager slope.mellow age   to:age sex nruns
1 0.00497811 -0.01316334  0.008185229  40 50.84185   M     4
> 
> 
> # Calculate effect size with respect to nruns
> effect_size(model_1, ~ nruns)
   slope.calm slope.eager slope.mellow nruns to:nruns age sex
1 0.004900487  0.02725955  -0.03216004     4 5.734239  40   M


# An rpart model
mod1 <- rpart(outcome ~ age + smoker, data = Whickham)

# Logistic regression
mod2 <- glm(outcome == "Alive" ~ age + smoker, 
            data = Whickham, family = "binomial")

# Logistic regression
mod3 <- glm(outcome ~ age + smoker, 
            data = Whickham, family = "binomial")

# Visualize the models with fmodel()

fmodel(mod1)

fmodel(mod2)

fmodel(mod3)


# Find the effect size of smoker
effect_size(mod1, ~ smoker)
effect_size(mod2, ~ smoker)
effect_size(mod3, ~ smoker)



A dataset with nn cases has nn degrees of freedom.
The explanatory side of a model formula has a number of degrees of freedom that depends on the terms in the formula. Start counting at 1; every model has at least 1 degree of freedom even if there are no explanatory variables.
A single variable has 1 degree of freedom if it is quantitative. If it is categorical with kk different levels, it has k−1k−1 degrees of freedom.
If variables are connected with +, add together their individual degrees of freedom.
If variables are connected with *, add together their individual degrees of freedom and then add in the product of their individual degrees of freedom.
There are special rules that apply in some special circumstances, but you don't need to worry about those here.
The difference between the degrees of freedom of the dataset and the degrees of freedom of the model formula is called the residual degrees of freedom. Models with zero residual degrees of freedom are not generally at all useful. Models with a handful of residual degrees of freedom have statistical properties that are not reliable.


lm(wage ~ age + sector + sex + married * exper, data = CPS85)

13 degrees of freedom. 
1 degree of freedom (as has every model) 
plus 1 for age 
plus 7 for sector 
plus 1 for sex 
plus 1 for married 
plus 1 for exper 
plus 1 * 1 for the interaction between married and exper. Altogether 1 + 1 + 7 + 1 + 1 + 1 + 1 gives 13 degrees of freedom.


# Train the four models
model_0 <- lm(wage ~ NULL, data = CPS85)
model_1 <- lm(wage ~ mosaic::rand(100), data = CPS85)
model_2 <- lm(wage ~ mosaic::rand(200), data = CPS85)
model_3 <- lm(wage ~ mosaic::rand(300), data = CPS85)

# Evaluate the models on the training data
output_0 <- evaluate_model(model_0, on_training = TRUE)
output_1 <- evaluate_model(model_1, on_training = TRUE)
output_2 <- evaluate_model(model_2, on_training = TRUE)
output_3 <- evaluate_model(model_3, on_training = TRUE)



# Compute R-squared for each model
with(output_0, var(model_output) / var(wage))
with(output_1, var(model_output) / var(wage))
with(output_2, var(model_output) / var(wage))
with(output_3, var(model_output) / var(wage))

# Compare the null model to model_3 using cross validation
cv_results <- cv_pred_error(model_0 , model_3, ntrials = 3)
boxplot(mse ~ model, data = cv_results)


# Train this model with 24 degrees of freedom
model_1 <- lm(hdd ~ year * month, data = HDD_Minneapolis)

# Calculate R-squared
output_1 <- evaluate_model(model_1, data = HDD_Minneapolis)
with(output_1, var(model_output) / var(hdd))

# Oops! Numerical year changed to categorical
HDD_Minneapolis$categorical_year <- as.character(HDD_Minneapolis$year)

# This model has many more degrees of freedom
model_2 <- lm(hdd ~ categorical_year * month, data = HDD_Minneapolis)

# Calculate R-squared
output_2 <- evaluate_model(model_2, data = HDD_Minneapolis)
with(output_2, var(model_output) / var(hdd))

#Framework for thinking about precision

Population
  randon sample
  sample statistic
    effect size
    mse
  ... sampling distribution

  Bootstraping
    use one randon sample to simulate many randon samples
    resampling distribution



# Three starting elements
model <- lm(wage ~ age + sector, data = CPS85) # data frame and model
effect_size(model, ~ age, data = CPS85) # quantity

# For practice
my_test_resample <- sample(1:10, replace = TRUE)
my_test_resample # view it

# Construct a resampling of CPS85
trial_1_indices <- sample(1:nrow(CPS85), replace = TRUE)
trial_1_data <- CPS85[trial_1_indices, ]

# Train the model to that resampling
trial_1_model <- lm(wage ~ age + sector, data = trial_1_data)

# Calculate the quantity 
effect_size(trial_1_model, ~ age, data = trial_1_data)



# Model and effect size from the "real" data
model <- lm(wage ~ age + sector, data = CPS85)
effect_size(model, ~ age)

# Generate 10 resampling trials
my_trials <- ensemble(model, nreps = 10)

# Find the effect size for each trial
effect_size(my_trials, ~ age)

# Re-do with 100 trials
my_trials <- ensemble(model, nreps = 100)
trial_effect_sizes <- effect_size(my_trials, ~ age, data = CPS85)

# Calculate the standard deviation of the 100 effect sizes
sd(trial_effect_sizes$slope)



# An estimate of the value of a fireplace
model <- lm(price ~ land_value + fireplaces + living_area, 
            data = Houses_for_sale)
effect_size(model, ~ fireplaces)

# Generate 100 resampling trials
trials <- ensemble(model, nreps = 100)

# Calculate the effect size in each of the trials
effect_sizes_in_trials <- effect_size(trials, ~ fireplaces)

# Show a histogram of the effect sizes
hist(effect_sizes_in_trials$slope)

# Calculate the standard error
sd(effect_sizes_in_trials$slope)



CPS85$log_wage = log(CPS85$wage)
model = lm(log_wage ~ age + exper + sex + sex + sector, data = CPS85)
effect = effect_size(model, ~ age)
exp(effect$slope) -1
0.07786872  #Wages go up 7.7 percent per year of age

rank transformation

CPS85$log_wage = log(CPS85$wage)
model = lm(log_wage ~ age + exper + sex + sex + sector, data = CPS85)
effect = effect_size(model, ~ age)
exp(effect$slope) -1

CPS85$rank_wage = rank(CPS85$wage)
ggplot(data = CPS85, aes(age, rank_wage, col = sex)) + geom_point() +  geom_smooth(method = "lm", se = FALSE) + facet_wrap(~sex)




# Make model with log(Cost)
mod_0 <- lm(Cost ~ Age + Sex + Coverage, data = AARP)

mod_1 <- lm(log(Cost) ~ Age + Sex + Coverage, data = AARP)
mod_2 <- lm(log(Cost) ~ Age * Sex + Coverage , data = AARP)
mod_3 <- lm(log(Cost) ~ Age * Sex + log(Coverage) , data = AARP)
mod_4 <- lm(log(Cost) ~ Age * Sex * log(Coverage) , data = AARP)

# To display each model in turn 
fmodel(mod_1, ~ Age + Sex + Coverage, 
       Coverage = c(10, 20, 50)) +
  ggplot2::geom_point(data = AARP, alpha = 0.5,
                      aes(y = log(Cost), color = Sex))

# Use cross validation to compare mod_4 and mod_1
results <- cv_pred_error(mod_1, mod_4) 
boxplot(mse ~ model, data = results)

You might be wondering why it helps to use the log(Coverage) term in the model rather than just plain Coverage. The reason is that there is an approximately linear relationship between Cost and Coverage (twice the coverage incurs about twice the cost). But because of the exponential relationship between Cost and Age, you used log(Cost) in the model. If using plain Coverage on the explanatory side, this would imply an exponential relationship between Cost and Coverage. Using log(Coverage) along with log(Cost) lets you approximate the linear relationship between Cost and Coverage.


# Model of oil production in mbbl
model_1 <- lm(mbbl ~ year, data = Oil_production)
fmodel(model_1, data = Oil_production) + 
  geom_point(data = Oil_production)

# Effect size of year
effect_size(model_1, ~ year, data = Oil_production)

# Model of log-transformed production
model_2 <- lm(log_mbbl ~ year, data = Oil_production)
fmodel(model_2, data = Oil_production) + 
  geom_point(data = Oil_production)

# And the effect size on log-transformed production
effect_size(model_2, ~ year, data = Oil_production)

        slope year  to:year  mbbl log_mbbl
1  0.06636971 1880 1907.324    30 3.401197
2  0.06636971 1890 1917.324    77 4.343805
3  0.06636971 1900 1927.324   149 5.003946
4  0.06636971 1905 1932.324   215 5.370638
5  0.06636971 1910 1937.324   328 5.793014
6  0.06636971 1915 1942.324   432 6.068426
7  0.06636971 1920 1947.324   689 6.535241
8  0.06636971 1925 1952.324  1069 6.974479
9  0.06636971 1930 1957.324  1412 7.252762
10 0.06636971 1935 1962.324  1655 7.411556
11 0.06636971 1940 1967.324  2150 7.673223
12 0.06636971 1945 1972.324  2595 7.861342
13 0.06636971 1950 1977.324  3803 8.243546
14 0.06636971 1955 1982.324  5626 8.635154
15 0.06636971 1960 1987.324  7674 8.945593
16 0.06636971 1962 1989.324  8882 9.091782
17 0.06636971 1964 1991.324 10310 9.240870
18 0.06636971 1966 1993.324 12016 9.393994
19 0.06636971 1968 1995.324 14104 9.554214

# Annual growth
100 * (exp(0.066) - 1)

6.822672

Doubling time is a great way to summarize the rate of exponential growth. Another way to summarize exponential growth is as a percent per year. You can translate the effect size into a percent growth per year with this formula: 100 * (exp(__effect_size__) - 1).

 Next Exercise




 # A model of price
model_1 <- lm(Price ~ Mileage + Age, data = Used_Fords)

# A model of logarithmically transformed price
Used_Fords$log_price <- log(Used_Fords$Price)
model_2 <- lm(log_price ~ Mileage + Age, data = Used_Fords)

# The model values on the original cases
preds_1 <- evaluate_model(model_1, data = Used_Fords)

# The model output for model_2 - giving log price
preds_2 <- evaluate_model(model_2, data = Used_Fords)

# Transform predicted log price to price
preds_2$model_price <- exp(preds_2$model_output)

# Mean square errors in price
mean((preds_1$Price - preds_1$model_output)^2, na.rm = TRUE)
mean((preds_2$Price - preds_2$model_price)^2, na.rm = TRUE)


> # Mean square errors in price
> mean((preds_1$Price - preds_1$model_output)^2, na.rm = TRUE)
[1] 6026231
> mean((preds_2$Price - preds_2$model_price)^2, na.rm = TRUE)
[1] 3711549  # BETTER PREDICTION  lower mse

#Confidence interval on log log-transformed models








# A model of logarithmically transformed price
model <- lm(log(Price) ~ Mileage + Age, data = Used_Fords)

# Create the bootstrap replications
bootstrap_reps <- ensemble(model, nreps = 100, data = Used_Fords)

# Find the effect size
age_effect <- effect_size(bootstrap_reps, ~ Age)

# Change the slope to a percent change
age_effect$percent_change <- 100 * (exp(age_effect$slope) - 1)

# Find confidence interval
with(age_effect, mean(age_effect$percent_change) + c(-2, 2) * sd(age_effect$percent_change))

[1] -9.197254 -7.315252

Right! The 95% confidence interval corresponds to a decrease in price each year between about 9 and 7 percent.


#Confidence and collinearity


# A model of wage
model_1 <- lm(wage ~ educ + sector + exper + age, data = CPS85)

# Effect size of educ on wage
effect_size(model_1, ~ educ)

# Examine confidence interval on effect size
ensemble_1 <- ensemble(model_1, nreps = 100)
effect_from_1 <- suppressWarnings(effect_size(ensemble_1, ~ educ))
with(effect_from_1, mean(slope) + c(-2, 2) * sd(slope))

# Collinearity inflation factor on standard error
collinearity( ~ educ + sector + exper + age, data = CPS85)

# Leave out covariates one at a time
collinearity( ~ educ + sector + exper, data = CPS85) # leave out age
collinearity( ~ educ + sector + age, data = CPS85) # leave out exper
collinearity( ~ educ + exper + age, data = CPS85) # leave out sector


> collinearity( ~ educ + sector + exper + age, data = CPS85)
       expl_vars      SeIF
1           educ 15.273900
2    sectorconst  1.090245
3    sectormanag  1.215769
4    sectormanuf  1.252303
5    sectorother  1.239831
6     sectorprof  1.405901
7    sectorsales  1.137992
8  sectorservice  1.274175
9          exper 71.980564
10           age 68.116772
> 
> # Leave out covariates one at a time
> collinearity( ~ educ + sector + exper, data = CPS85) # leave out age
      expl_vars     SeIF
1          educ 1.380220
2   sectorconst 1.090245
3   sectormanag 1.215761
4   sectormanuf 1.252303
5   sectorother 1.239814
6    sectorprof 1.402902
7   sectorsales 1.137990
8 sectorservice 1.274174
9         exper 1.092803
> collinearity( ~ educ + sector + age, data = CPS85) # leave out exper
      expl_vars     SeIF
1          educ 1.311022
2   sectorconst 1.090245
3   sectormanag 1.215754
4   sectormanuf 1.252302
5   sectorother 1.239801
6    sectorprof 1.402764
7   sectorsales 1.137990
8 sectorservice 1.274174
9           age 1.034143
> collinearity( ~ educ + exper + age, data = CPS85) # leave out sector
  expl_vars     SeIF
1      educ 15.15169
2     exper 71.74900
3       age 67.90730


Leaving out either age or exper brings a huge reduction in the variance inflation factor. The reason: it's pretty much true that 5 + educ + exper == age — only two of the three variables are independent.

 Next Exercise


#Confidence interval

ensemble_1 <- ensemble(model_1, nreps = 100)
effect_from_1 <- effect_size(ensemble_1, ~ educ)
with(effect_from_1, mean(slope) + c(-2, 2) * sd(slope))



# Improved model leaving out worst offending covariate
model_2 <- lm(wage ~ educ + sector + age, data = CPS85)

# Confidence interval of effect size of educ on wage
ensemble_2 <- ensemble(model_2, nreps = 100)
effect_from_2 <- effect_size(ensemble_2, ~ educ)
with(effect_from_2, mean(slope) + c(-2, 2) * sd(slope))

[1] 0.4581053 0.8507996

It seems odd that leaving out a covariate can improve the confidence interval, but in the presence of collinearity, this can be the case.



# Train a model Price ~ Age + Mileage
model_1 <- lm(Price ~ Age + Mileage, data = Used_Fords)

# Train a similar model including the interaction
model_2 <- lm(Price ~ Age * Mileage, data = Used_Fords)

# Compare cross-validated prediction error
cv_pred_error(model_1, model_2)

# Use bootstrapping to find conf. interval on effect size of Age  
ensemble_1 <- ensemble(model_1, nreps = 100)
ensemble_2 <- ensemble(model_2, nreps = 100)
effect_from_1 <- effect_size(ensemble_1, ~ Age)
effect_from_2 <- effect_size(ensemble_2, ~ Age)
with(effect_from_1, mean(slope) + c(-2, 2) * sd(slope))
with(effect_from_2, mean(slope) + c(-2, 2) * sd(slope))

# Compare inflation for the model with and without interaction
collinearity( ~ Age + Mileage, data = Used_Fords)
collinearity( ~ Age * Mileage, data = Used_Fords)



# Train models
mod_1 <- lm(sat ~ expend, data = SAT)
mod_2 <- lm(sat ~ expend + frac, data = SAT)
mod_3 <- lm(sat ~ expend * frac, data = SAT)

# Bootstrap replications of effect size
sizes_1 <- effect_size(ensemble(mod_1, nreps = 100), ~ expend)
sizes_2 <- effect_size(ensemble(mod_2, nreps = 100), ~ expend)
sizes_3 <- effect_size(ensemble(mod_3, nreps = 100), ~ expend)

Right! mod_2 also has the narrowest confidence interval on the effect size of expend. This is because the large inflation in mod_3 obscures the effect size. So which is the best model? We have a theoretical reason to prefer mod_2 to mod_1: the covariate frac reduces the prediction error substantially. And we have a theoretical reason to question mod_3: the high inflation due to collinearity. mod_2 provides the best trade-off.



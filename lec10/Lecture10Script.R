################################################################################
### set up
# necessary packages
# install.packages(c("glmnet", "caret", "latex2exp"))
library(tidyverse)
library(data.table)
library(latex2exp)
set.seed(2025)

################################################################################
# Simulation
## Monte Carlo Simulation
# LLN: Simulating Sample Mean from Uniform(0,1)
set.seed(2025)  # Set seed for reproducibility

Nsim       = 1000                    # Number of simulations / sample size
samp_means = cumsum(runif(Nsim))/(1:Nsim)  # Compute cumulative average of uniform(0,1) draws

# Create data table for plotting
plot_data = data.table(x=1:Nsim, y=samp_means)

# Plot the Law of Large Numbers (LLN) demonstration for Uniform distribution
ggplot(plot_data,aes(x=x,y=y)) + 
  geom_line(size=1.5) +
  coord_cartesian(ylim=c(0.25,0.75)) +
  geom_hline(yintercept=1/2, linetype='dotted', col = 'red',size=1.5) +
  ylab("Sample Mean") +
  xlab("Sample Size") +
  theme_minimal()



# LLN: Simulating Sample Mean from Exponential(1/2)
set.seed(2025)  # Set seed for reproducibility

Nsim       = 1000           # Number of simulations / sample size
lambda     = 1/2            # Rate parameter for Exponential distribution
samp_means = cumsum(rexp(Nsim, lambda)) / (1:Nsim)  # Cumulative average of exponential draws

# Create data table for plotting
plot_data = data.table(x = 1:Nsim, y = samp_means)

# Plot the Law of Large Numbers (LLN) demonstration for Exponential distribution
ggplot(plot_data,aes(x=x,y=y)) + 
  geom_line(size=1.5) +
  ylab("Sample Mean") +
  xlab("Sample Size") +
  geom_hline(yintercept=2, linetype='dotted', col = 'red',size=1.5) +
  theme_minimal()



# LLN: Simulating Sample Mean from Cauchy(0,1)
set.seed(2025)  # Set seed for reproducibility

Nsim       = 1000                          # Number of simulations / sample size
samp_means = cumsum(rcauchy(Nsim)) / (1:Nsim)  # Cumulative average of Cauchy draws

# Create data table for plotting
plot_data = data.table(x = 1:Nsim, y = samp_means)

# Plot the "sample mean" of Cauchy distribution (note: LLN does not hold)
ggplot(plot_data,aes(x=x,y=y)) + 
  geom_line(size=1.5) +
  ylab("Sample Mean: Cauchy") +
  xlab("Sample Size") +
  geom_hline(yintercept=0, linetype='dotted', col = 'red',size=1.5)+
  theme_minimal()


# CLT: Simulating Sample Mean from Unif(0,1) for Nsamp = 100
set.seed(2025)                       # Set seed for reproducibility

Nsim         = 1000                  # Number of Monte Carlo simulations
Nsamp        = 100                   # Sample size for each simulation
sample_means = rep(0, Nsim)          # Preallocate vector to store sample means

# Run simulations
for(sim in 1:Nsim){
  draws = runif(Nsamp)               # Draw Nsamp values from Uniform(0,1)
  sample_means[sim] = mean(draws)    # Compute sample mean
}

# Create density curve for theoretical distribution of the sample mean
xs = seq(min(sample_means), max(sample_means), length.out = 100)
ys = dnorm(xs, mean = 1/2, sd = sqrt(1/12/Nsamp))  # Normal approximation using CLT

# Store density data in a data.table for plotting
den_data = data.table(x = xs, y = ys)

# Store Monte Carlo sample means for plotting
MC_data = data.table(x = sample_means)

# Create the color for density label
den_cols        = wesanderson::wes_palette("Darjeeling1",5)[c(2,4)]
names(den_cols) = paste(c("Empirical","Theoretical"),"Density")

# Plot the density
ggplot(data=MC_data,aes(x=x))+
  geom_histogram(aes(y = after_stat(!!str2lang("density")),fill="Histogram"),
                 color="black",bins = 20) +
  geom_density(aes(colour="Empirical Density"),size=0.8)+
  geom_line(aes(x = x, y = y,color = "Theoretical Density"),size=0.8,data=den_data) + 
  scale_color_manual(name="Densities",values=den_cols) +
  scale_fill_manual(name="",values=c("Histogram"=alpha("darkgray",0.5))) +
  labs(title=paste0("Monte Carlo Simulation for Sample Mean: Nsamp = ",Nsamp))+
  xlab(TeX("$\\hat{\\mu}$"))+
  theme_minimal()+theme(legend.position = "top",
                        plot.title = element_text(hjust=0.5))



# CLT: Simulating Sample Variance from Unif(0,1) for Nsamp = 10
set.seed(2025)
Nsamp         = 10          # set sample size
Nsim          = 10000       # set number of MC simulations
sample_varN   = rep(0,Nsim) # preallocate vector to store MC estimates
sample_varNm1 = rep(0,Nsim) # preallocate vector to store MC estimates

for(sim in 1:Nsim){
  draws              = runif(Nsamp)                # draw Nsamp values from U(0,1)
  sample_varN[sim]   = mean((draws-mean(draws))^2) # sample var dividing by 1/N
  sample_varNm1[sim] = var(draws)                  # sample var dividing by 1/(N-1)
}

# Combine sample variances into one data.table for plotting
MC_data_var = data.table(
  x = c(sample_varN, sample_varNm1),                   # Sample variances
  Estimator = rep(c("N","N-1"), each = Nsim)          # Label for each estimator
)

# Define colors for the two estimators using Wes Anderson palette
dencols = wesanderson::wes_palette("Darjeeling1", 5)[c(2,4)]
names(dencols) = c("N", "N-1")

# Define semi-transparent fill colors for histogram/density
denfills = alpha(dencols, 0.25)

# Plot distribution
ggplot(data=MC_data_var,aes(x=x,color=Estimator,fill=Estimator))+
  geom_density(size=0.8) +
  geom_vline(xintercept=1/12,linetype="dashed",color="black")+
  scale_color_manual(values=dencols)+
  scale_fill_manual(values=denfills)+
  xlab(TeX("$\\hat{\\sigma}^2_X$"))+
  theme_minimal()+theme(legend.position = "top",
                        plot.title = element_text(hjust=0.5))


# OLS Blue
set.seed(2025)
Nsamp         = 100         # set sample size
Nsim          = 10000       # set number of MC simulations
sample_OLS    = rep(0,Nsim) # preallocate vector to store MC estimates
sample_lazy   = rep(0,Nsim) # preallocate vector to store MC estimates

for(sim in 1:Nsim){
  e                 = rnorm(Nsamp)                # draw e values from N(0,1)
  x                 = rnorm(Nsamp)                # draw x values from N(0,1)
  y                 = e                           # create y based on x, e, and the model
  sample_OLS[sim]   = coef(lm(y ~ x - 1))         # Estimate beta use ols without constant
  sample_lazy[sim]  = coef(lm(y[1:10] ~ x[1:10] - 1)) # Estimate beta use lazy
}

# Plot the result
MC_data_ols = data.table(x=c(sample_OLS,sample_lazy),
                         Estimator=rep(c("OLS","Lazy"),each=Nsim))

dencols        = wesanderson::wes_palette("Darjeeling1",5)[c(2,4)]
names(dencols) = c("OLS","Lazy")
denfills       = alpha(dencols,0.25)

ggplot(data=MC_data_ols,aes(x=x,color=Estimator,fill=Estimator))+
  geom_density(size=0.8) +
  geom_vline(xintercept=0,linetype="dashed",color="black")+
  scale_color_manual(values=dencols)+
  scale_fill_manual(values=denfills)+
  xlab(TeX("$\\hat{\\beta}$"))+
  theme_minimal()+theme(legend.position = "top",
                        plot.title = element_text(hjust=0.5))



## Residual Bootstrap

# Bootstrap for Ridge
library(glmnet)

# Response variable
y = mtcars$hp

# Predictor matrix
x = data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])

# Ridge regression example with lambda = 5 (alpha=0)
ridge_model = glmnet(x, y, alpha = 0, lambda = 5)

# Original ridge coefficient
orig_coef = as.numeric(coef(ridge_model)["mpg", , drop=FALSE])

# Get fitted values at lambda = 5
y_hat = predict(ridge_model, newx = x, s = 5)

# Residuals
resid_ridge = y - y_hat

# --- Bootstrapping to get sampling distribution for mpg coefficient ---
set.seed(2025)
nboot = 1000
boot_coefs = numeric(nboot)

for(i in 1:nboot){
  error = sample(resid_ridge, nrow(x), replace = TRUE)  # resample rows with replacement
  
  model_boot = glmnet(x, y_hat + error, alpha = 0, lambda = 5)
  boot_coefs[i] = coef(model_boot)["mpg",]
}

# Create a data frame for plotting
df_boot = data.frame(coef_mpg = boot_coefs)

# Inspect bootstrapped sampling distribution
ggplot(df_boot, aes(x = coef_mpg)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = orig_coef, color = "red", linetype = "dashed", size = 1.2) +
  labs(title = "Bootstrap Distribution of Ridge Coef for mpg",
       x = "Coefficient",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))




# Bootstrap for OLS
# Original OLS coefficient
ols_model = lm(y ~ ., data = mtcars[, c("mpg", "wt", "drat", "qsec")])
orig_ols = coef(ols_model)["mpg"]

# Get fitted values for ols
y_hat = ols_model$fitted.values

# Residuals
resid_ols = ols_model$residuals

# --------------------------
# Bootstrap OLS coefficients
boot_ols = numeric(nboot)
for(b in 1:nboot){
  error = sample(resid_ols, nrow(x), replace = TRUE)  # resample rows with replacement
  new_y = y_hat + error
  
  boot_model = lm(new_y ~ ., data = mtcars[, c('mpg', 'wt', 'drat', 'qsec')])
  boot_ols[b] = coef(boot_model)["mpg"]
}

# --------------------------
# Combine into one data.table for ggplot
plot_dt = data.table(
  coef = c(boot_ols, boot_coefs),
  Estimator = rep(c("OLS", "Ridge"), each = nboot)
)

# --------------------------
# Plot histogram
ggplot(plot_dt, aes(x = coef, fill = Estimator)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 50, color = "black") +
  geom_vline(xintercept = orig_ols, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = as.numeric(orig_coef), color = "red", linetype = "dashed", size = 1) +
  # Add text labels for the vertical lines
  annotate("text", x = orig_ols, y = 0.35, 
           label = "OLS", color = "blue", hjust = -0.2) +
  annotate("text", x = as.numeric(orig_coef), y = 0.35, 
           label = "Ridge", color = "red", hjust = 1.2) +
  scale_fill_manual(values = c("orange", "skyblue")) +
  xlab("Coefficient for mpg") +
  ylab("Frequency") +
  ggtitle("Bootstrap OLS vs Ridge Coefficient for mpg") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))


################################################################################
# Descriptive Statistics and Tests: LM
## OLS regression results summary
summary(lm(hp ~ mpg + wt + drat + qsec, data = mtcars))



## Inference - Std. Error
# Response variable
y = mtcars$hp

# Predictor matrix
x = data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])

# --- Bootstrapping to get sampling distribution for mpg coefficient ---
set.seed(2025)
nboot = 1000

# --------------------------
# Bootstrap OLS coefficients
boot_ols = numeric(nboot)
boot_t = numeric(nboot)
for(b in 1:nboot){
  error = sample(resid_ols, nrow(x), replace = TRUE)  # resample rows with replacement
  new_y = y_hat + error
  
  boot_model = lm(new_y ~ ., data = mtcars[, c('mpg', 'wt', 'drat', 'qsec')])
  boot_ols[b] = coef(boot_model)["mpg"]
  boot_t[b] = summary(boot_model)$coefficients["mpg", "t value"]
}

# from bootstrap
sd(boot_ols)



## Inference - t value
# Create a data frame for plotting
t_boot = data.frame(t_mpg = boot_t)

# Create a density curve for t distribution
xs2 = seq(min(boot_t) - mean(boot_t),max(boot_t)-mean(boot_t),length.out = 100)
ys2 = dt(xs2, df = (nrow(x) - ncol(x) - 1))
den_data2 = data.table(x=xs2 + mean(boot_t),y=ys2)
ys3 = dnorm(xs2)
den_data3 = data.table(x=xs2 + mean(boot_t),y=ys3)

den_cols        = wesanderson::wes_palette("Darjeeling1",5)[c(2,3,4)]
names(den_cols) = paste(c("Empirical","Theoretical", "Normal"),"Density")

ggplot(t_boot, aes(x = t_mpg)) +
  geom_histogram(aes(y = after_stat(!!str2lang("density")),fill="Histogram"),
                 color="black",bins=20) +
  geom_density(aes(color="Empirical Density"),size=0.8)+
  geom_line(aes(x = x, y = y,color = "Theoretical Density"),size=0.8,data=den_data2) + 
  geom_line(aes(x = x, y = y,color = "Normal Density"),size=0.8,data=den_data3) + 
  scale_color_manual(name="Densities",values=den_cols) +
  scale_fill_manual(name="",values=c("Histogram"=alpha("darkgray",0.5))) +
  labs(title=paste0("Bootstrap for t-statistics: dof = ", nrow(x) - ncol(x) - 1))+
  xlab("t-statistics")+
  theme_minimal()+theme(legend.position = "top",
                        plot.title = element_text(hjust=0.5))




## Inference - Pr(>|t|)
# Convert to data frame for ggplot
df = data.frame(t_stat = boot_t)

ggplot(df, aes(x = t_stat)) +
  geom_density(fill = "lightgray", alpha = 0.5) +
  # Shade area for t >= 0
  stat_density(
    geom = "area",
    fun = "density",
    fill = "steelblue",
    alpha = 0.5,
    xlim = c(0, max(boot_t))
  ) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Two-sided Bootstrap Test",
    subtitle = paste0("Shaded area: ", round(mean(boot_t>=0), 3), " for one tail"),
    x = "Bootstrap t-statistic",
    y = "Density"
  ) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

# Use mean of a logical to get the probability of true
# Also multiply by two due to two-sided test
paste0("The two-sided p-value for beta = 0 from bootstrap is ", round(2*mean(boot_t>=0),2), ".")

# Compare to the p-value for mpg
paste0("The two-sided p-value for beta = 0 from lm is ", round(summary(ols_model)$coefficients["mpg", "Pr(>|t|)"], 2), ".")



################################################################################
# Training/Testing: LM
## Data Splitting and Fitting
# Split data randomly
set.seed(2025) # do not forget to set seed as randomness is introduced!
# Note that we do not want replacement here. The entire testing data should not be seen when training
Ind_train = sample(x = 1:nrow(mtcars),
                   size = as.integer(nrow(mtcars)*0.7))
# Extract the training and testing data
Training_cars = mtcars[Ind_train,]
Testing_cars = mtcars[-Ind_train,]

# Model Fitting
Model1 = lm(Training_cars$hp ~ ., Training_cars[,c("mpg", "wt", "drat", "qsec")])
Model2 = lm(Training_cars$hp ~ ., Training_cars[,c("mpg", "wt", "qsec")])


## OOS Testing
# Use predict to get prediction from the fitted model but with testing data
Predict1 = predict(Model1, newdata = Testing_cars[, c("mpg", "wt", "drat", "qsec")])
Predict2 = predict(Model2, newdata = Testing_cars[, c("mpg", "wt", "qsec")])

# Actual hp values in the test set
Actual_hp = Testing_cars$hp

# RMSE function
rmse = function(actual, predicted) {
  return(sqrt(mean((actual - predicted)^2)))
}

# Compute RMSE
RMSE1 = rmse(Actual_hp, Predict1)
RMSE2 = rmse(Actual_hp, Predict2)

# Print result
paste0("IS R^2 for Model 1 is ", round(summary(Model1)$r.squared, 3), "; Adjusted R^2 is ", round(summary(Model1)$adj.r.squared, 3))
paste0("IS R^2 for Model 2 is ", round(summary(Model2)$r.squared, 3), "; Adjusted R^2 is ", round(summary(Model2)$adj.r.squared, 3))

paste0("OOS RMSE for Model 1 is ", round(RMSE1,2))
paste0("OOS RMSE for Model 2 is ", round(RMSE2,2))


## Error Plot
# Residuals for both models
Error1 = Actual_hp - Predict1
Error2 = Actual_hp - Predict2

# Build combined data frame
Error_df = data.frame(
  Predicted = c(Predict1, Predict2),
  Error = c(Error1, Error2),
  Model = rep(c("Model 1", "Model 2"),
              times = c(length(Predict1), length(Predict2)))
)

# Plot
ggplot(Error_df, aes(x = Predicted, y = Error, color = Model)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  labs(
    title = "OOS Prediction Errors Plots for Model 1 and Model 2",
    x = "Predicted hp",
    y = "Errors"
  ) +
  theme(plot.title = element_text(hjust=0.5))


## Diebold-Mariano test
# loss differences
d_i = (Error1)^2 - (Error2)^2

# t-test on loss differences
t.test(d_i, alternative = c("greater"))



################################################################################
# Training/Validation/Testing: Ridge

## For loop
# -------------------------
# Prepare data
# -------------------------
y = Training_cars$hp
x = data.matrix(Training_cars[, c("mpg", "wt", "drat", "qsec")])

# -------------------------
# Define lambda grid
# -------------------------
lambda_grid = 10^seq(-3, 3, length.out = 50)  # 0.001 to 1000
K = 10  # number of folds
set.seed(2025)

# -------------------------
# Create fold indices
# -------------------------
n = nrow(x)
folds = sample(rep(1:K, length.out = n))

# -------------------------
# Initialize storage for CV errors
# -------------------------
cv_errors = matrix(0, nrow = length(lambda_grid), ncol = K)

# -------------------------
# K-fold CV loop
# -------------------------
for(k in 1:K){
  test_idx  = which(folds == k)
  train_idx = setdiff(1:n, test_idx)
  
  x_train = x[train_idx, ]
  y_train = y[train_idx]
  x_test  = x[test_idx, ]
  y_test  = y[test_idx]
  
  for(i in seq_along(lambda_grid)){
    lambda = lambda_grid[i]
    model = glmnet(x_train, y_train, alpha = 0, lambda = lambda, standardize = TRUE)
    
    # Predict on the test fold
    y_pred = predict(model, newx = x_test)
    
    # Mean squared error
    cv_errors[i, k] = mean((y_test - y_pred)^2)
  }
}

# -------------------------
# Average CV error for each lambda
# -------------------------
mean_cv_error = rowMeans(cv_errors)
best_lambda = lambda_grid[which.min(mean_cv_error)]
paste0("Best Lambda from 10-fold CV: ", round(best_lambda, 2))

# -------------------------
# Plot CV curve
# -------------------------
# Example data
data.table(
  log_lambda = log(lambda_grid),
  mean_cv_error = mean_cv_error
) |>
  ggplot(aes(x = log_lambda, y = mean_cv_error)) +
  geom_point(size = 2) +
  geom_line() +
  geom_vline(xintercept = log(best_lambda), color = "red", linetype = "dashed") +
  xlab("log(lambda)") +
  ylab("Mean CV Error") +
  ggtitle("10-Fold CV for Ridge") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


## glmnet package
# Load package
library(glmnet)

# Response variable
y = Training_cars$hp

# Predictor matrix (exclude response)
x = data.matrix(Training_cars[, c("mpg", "wt", "drat", "qsec")])

# set seed before cv
set.seed(2025)
# Fit Ridge regression (alpha = 0)
# Cross-validation to choose optimal lambda
cv_ridge = cv.glmnet(x, y, alpha = 0, type.measure = "mse")
plot(cv_ridge)               # CV curve
best_lambda = cv_ridge$lambda.min
paste0("Best Lambda from 10-fold CV: ", round(best_lambda, 2))

# Refit with optimal lambda
ridge_best = glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(ridge_best)


## caret package
# Packages
library(caret)
library(glmnet)

# Response and predictors
y = Training_cars$hp
x = Training_cars[, c("mpg", "wt", "drat", "qsec")]

# set seed before cv
set.seed(2025)

# Define training control (e.g., 10-fold CV)
train_control = trainControl(method = "cv", number = 10)

# Train Ridge regression with caret
ridge_caret = train(
  x = x,
  y = y,
  method = "glmnet",
  metric = "RMSE",
  trControl = train_control,
  tuneGrid = expand.grid(alpha = 0,   # alpha=0 for ridge
                         lambda = seq(0, 20, length = 50))  # candidate lambdas
)

# View results
plot(ridge_caret)
paste0("The best tuning result is: ", round(ridge_caret$bestTune["lambda"], 2))

# Coefficients for best lambda
coef(ridge_caret$finalModel, s = ridge_caret$bestTune$lambda)



## Refit the model with best lambda
# x and y for the whole training + validation set
y = Training_cars$hp
x = data.matrix(Training_cars[, c("mpg", "wt", "drat", "qsec")])

# Refit with optimal lambda
ridge_best = glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(ridge_best)

# Coefficients for best lambda from caret
coef(ridge_caret$finalModel, s = ridge_caret$bestTune$lambda)



## Time-series CV
# Let's use the code from econ370_final_project.Rmd to extract return data. Use Ridge regression for current return and 1-lag return as X's and future return as Y.# Data for prices, I would like to extract slightly before 1985
library(tidyquant)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Get S&P500 daily data
SP500_simple_returns = tq_get("^GSPC",
                              from = "1984-01-01",
                              to   = "2024-12-31",
                              get  = "stock.prices") |>
  tq_transmute(
    select     = adjusted,       # adjusted price for accuracy
    mutate_fun = periodReturn,   # calculate period returns
    period     = "monthly",      # monthly returns
    type       = "arithmetic",   # simple return (percentage change)
    col_rename = "monthly_return"
  ) |>
  mutate(monthly_return = monthly_return*100) # Convert to percentage

# Creating predicting_date and predicting_return using lead
Total_data = SP500_simple_returns |>
  mutate(lag_return = lag(monthly_return),
         predicting_date = lead(date),
         predicting_return = lead(monthly_return))

# Subset predicting date from 1985-01-01 to 2024-12-01
# MAKE SURE IT IS predicting_date!
Total_data = Total_data |>
  filter(predicting_date >= "1985-01-01",
         predicting_date <= "2024-12-01")

# see the first and last 3 observations
head(Total_data, n = 3)
tail(Total_data, n = 3)

# Check if there is any NA in the data
anyNA(Total_data)
# Do not simply drop NA!


# Define lambda grid
# You should customize the grid to have the best lambda inside the range!
lambda_grid = 10^seq(-3, 20, length.out = 50)  # 0.001 to 10^20

# Initialize storage for CV errors
cv_errors = rep(0, length(lambda_grid))


# Preallocate the OOS fitted data from 2009 to 2024
OOS_result = Total_data |>
  filter(predicting_date >= "2009-01-01") |>
  # Only keep the predicting_date, predicting_return, and create fitted_return
  select(predicting_date, predicting_return) |>
  rename(actual_return= predicting_return) |>
  mutate(fitted_return = NA)


# Use a for loop to estimate each model and glmnet for Ridge regression
# Start from the end year 2009 to 2024 which are the end of training periods
# Note, i here is the testing data year
for (i in 2009:2024) {
  # Extract the Training data
  Training_data = Total_data |>
    # before i-8 year (i = 2009, training is before and not include 2001)
    filter(year(predicting_date) < (i-8)) |>
    # Only keep the predictors and the predicting_return
    select(c(predicting_return, monthly_return, lag_return))
  
  # Extract the validation data
  Validation_data = Total_data |>
    # between i-8 and i (not include i but include i-8)
    filter(year(predicting_date) >= (i-8),
           year(predicting_date) < i) |>
    # Only keep the predictors and the predicting_return
    select(c(predicting_return, monthly_return, lag_return))
  
  # New data to predict
  Testing_data = Total_data |>
    # at i
    filter(year(predicting_date) == i) |>
    # Only keep the predictors
    select(monthly_return, lag_return)
  
  # Note that we need to extract data as matrix for glmnet fitting
  x_train = data.matrix(Training_data[, c("monthly_return", "lag_return")])
  y_train = Training_data$predicting_return
  x_val  = data.matrix(Validation_data[, c("monthly_return", "lag_return")])
  y_val  = Validation_data$predicting_return
  x_test = data.matrix(Testing_data[, c("monthly_return", "lag_return")])
  
  # Use the for loop to find the best lambda
  for(j in seq_along(lambda_grid)){
    model = glmnet(x_train, y_train, alpha = 0, lambda = lambda_grid[j])
    
    # Predict on the validation fold
    y_pred = predict(model, newx = x_val)
    
    # Mean squared error
    cv_errors[j] = mean((y_val - y_pred)^2)
  }
  
  # Find the best lambda
  best_lambda = lambda_grid[which.min(cv_errors)]
  
  # Refit the model with best_lambda
  # For the total Train + Validation set!
  model = glmnet(rbind(x_train, x_val), rbind(y_train, y_val), alpha = 0, lambda = best_lambda)
  
  # Make predictions and save to year i using logical indexing
  # Use predict to estimate the fitted data based on model and test data
  Testing_result[year(Testing_result$predicting_date) == i, "fitted_return"] = predict(model, newx = x_test)
}

# Plot the result
ggplot(Testing_result) +
  geom_point(aes(predicting_date, fitted_return, color = "Fitted")) +
  geom_point(aes(predicting_date, actual_return, color = "Actual")) +
  labs(y = "Return",
       x = "date")

################################################################################
### set up
# necessary packages
# You need to set up torch seperately with Torch_setup.R
# install.packages(c("ISLR2", "luz", "torchvision", "torchdatasets", "rpart", "rpart.plot", "partykit", "ranger", "caret"))
library(fontawesome)
library(tidyverse)
library(data.table)

options(torch.threshold_call_gc = 4000) # garbage collector for torch
library(torch)        # Core PyTorch in R
library(luz)          # High-level interface for torch
library(torchvision)  # Useful for datasets and transformations
library(torchdatasets) 

library(rpart)
library(rpart.plot)
library(partykit)
library(ranger)

library(ISLR2) # wage data

set.seed(2025)

################################################################################
# Tree
## Regression Tree
# rpart for tree
# cp is the hyperparameter here!
rpart.plot(
  rpart(wage ~ age, data = Wage, method = "anova",
        control = rpart.control(cp = 0.001)),
  type = 2, extra = 101, fallen.leaves = TRUE,
  main = "Regression Tree: wage vs. age"
)


# Regression Tree with Linear Leaf
# Here, since there seems to be a trend, we will use partykit package
library(partykit)
## a simple basic fitting function (of type 1) for a ols regression
OLS <- function(y, x, start = NULL, weights = NULL, offset = NULL, ...) {
  lm(y ~ 0 + x, start = start, ...)
}
# mob control about minsplit, this can be seen as a hyperparameter
# More to check ?mob_control
ctrl <- mob_control(
  minsplit = 50
)
# Fit linear model in each node: wage ~ age (intercept + slope) in leaves
mob_fit <- mob(
  wage ~ age | age,         # formula: response ~ predictors | partitioning variables
  data = Wage,
  fit = OLS,               # linear model in each node
  control = ctrl
)
# Plot tree
plot(mob_fit, main = "Regression Tree with Linear Models in Leaves")



# Large Tree
# mob control using SSR as objective
ctrl2 <- mob_control(
  minsplit = 50,
  alpha = 1       # set alpha high to ignore parameter instability test (tree will grow very large)
)
# Fit linear model in each node: wage ~ age (intercept + slope) in leaves
mob_fit2 <- mob(
  wage ~ age | age,         # formula: response ~ predictors | partitioning variables
  data = Wage,
  fit = OLS,               # linear model in each node
  control = ctrl2
)
# Plot tree
plot(mob_fit2, main = "Large Regression Tree with Linear Models in Leaves")



# Pruning with CV
# Fit a tree (For demonstration, you can change cp to 0.0001)
fit1_RegTree <- rpart(wage ~ age, data = Wage, method = "anova",
                      control = rpart.control(cp = 0.001))
printcp(fit1_RegTree)      # prints CV table

# You then can use the following code to extract the best model
opt_index <- which.min(fit1_RegTree$cptable[,"xerror"])
best_cp <- fit1_RegTree$cptable[opt_index, "CP"]
paste0("The best value for cp is: ", best_cp)

# Find the pruned tree
fit1_pruned <- prune(fit1_RegTree, cp = best_cp)
rpart.plot(fit1_pruned)



# Tree with more Variables
# Fit an rpart tree
rpart_tree <- rpart(wage ~ .-logwage, data = Wage, method = "anova",
                    control = rpart.control(cp = 0.001))
# # plot the tree
# rpart.plot(
#   rpart_tree,
#   type = 2, fallen.leaves = TRUE,
#   main = "Regression Tree: wage vs. age"
# )

# # Plot the variable importance plot
# vip <- rpart_tree$variable.importance
# barplot(vip, las = 2, main = "Variable Importance")

# Plot the variable importance plot with ggplot
vip <- enframe(rpart_tree$variable.importance, 
               name = "variable", value = "importance")

ggplot(vip, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (rpart tree)",
       x = "", y = "Importance") +
  theme(plot.title = element_text(hjust=0.5))


# Testing Trees
# Create the training data for Wage
set.seed(2025)
Train_ind <- sample(1:nrow(Wage), floor(0.7*nrow(Wage)))
Wage_train <- Wage[Train_ind, ]
Wage_test <- Wage[-Train_ind, ]

# Fit a tree from training data (For demonstration, you can change cp to 0.0001)
fit2_RegTree <- rpart(wage ~ .-logwage, data = Wage_train, method = "anova",
                      control = rpart.control(cp = 0.001))
# printcp(fit2_RegTree)      # prints CV table

# You then can use the following code to extract the best model (via 10-fold CV)
opt_index <- which.min(fit2_RegTree$cptable[,"xerror"])
best_cp <- fit2_RegTree$cptable[opt_index, "CP"]
# paste0("The best value for cp is: ", round(best_cp,5))

# Refit the model
rpart_tree2 <- rpart(wage ~ .-logwage, data = Wage_train, method = "anova",
                     control = rpart.control(cp = best_cp))
# rpart_tree2

# predict with test data
pred <- predict(rpart_tree2, newdata = Wage_test)

# estimate the OOS MSE
oos_mse <- mean((Wage_test$wage - pred)^2)
paste0("The OOS MSE is: ", round(oos_mse,2))




################################################################################
# Bagging, Boosting and Random Forest
## Random Forest with `ranger`
library(ranger)

set.seed(123)
# use ranger to fit RF model
rf_model <- ranger(
  formula = wage ~ . - logwage,
  data = Wage_train,
  num.trees = 500,
  mtry = 5, # number of variables tried on each tree
  importance = "impurity",
  seed = 123   # ensures reproducibility
)

# OOB MSE for regression
paste0("OOB prediction error MSE: ", rf_model$prediction.error)


# Variable Importance
# Extract variable importance
vip <- data.frame(
  Variable = names(rf_model$variable.importance),
  Importance = rf_model$variable.importance
)

# Plot
ggplot(vip, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (Random Forest)", x = "", y = "Importance") +
  theme(plot.title = element_text(hjust=0.5))


# CV with `caret`
library(caret)
set.seed(123)

# Define train control with 5-fold CV
train_ctrl <- trainControl(method = "cv", number = 5)

# Train random forest using ranger via caret
rf_cv <- train(
  wage ~ . - logwage,
  data = Wage_train,
  method = "ranger",
  trControl = train_ctrl,
  tuneLength = 3
)

rf_cv





## Boosting with `xgboost`
library(xgboost)

# Set the model
X <- model.matrix(~ . - wage - logwage - 1, data = Wage_train)   # converts all factors/characters to dummies
y <- Wage_train$wage

# First seed
set.seed(123)
xgb_model <- xgboost(
  data = X,
  label = y,
  objective = "reg:squarederror",
  nrounds = 200,
  eta = 0.01,
  max_depth = 4,
  subsample = 0.8,
  colsample_bytree = 0.8,
  verbose = 0,   # silence the fitting message
  seed = 123     # main seed
)

# Variable Importance
# Get importance
vi_xgb <- xgb.importance(model = xgb_model)  # Returns a data.frame

# # Quick plot using xgboost built-in function
# xgb.plot.importance(vi_xgb, top_n = 10, rel_to_first = TRUE)

# Or use ggplot
ggplot(vi_xgb, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (XGBoost)", x = "", y = "Importance") +
  theme(plot.title = element_text(hjust=0.5))



# CV and Early Stopping for Boosting
set.seed(123)
# 5-fold CV for boosting
cv_res <- xgb.cv(
  data = X,
  label = y,
  nrounds = 2000,
  eta = 0.01,
  nfold = 5,
  objective = "reg:squarederror",
  early_stopping_rounds = 50,
  verbose = 0
)
# Find the best number of iterations
cv_res$evaluation_log[cv_res$best_iteration,]


# Model Evaluations
paste0("The mse of the pruned tree is: ", round(oos_mse,2))

# We can directly fit new data to rf
rf_pred <- predict(rf_cv, newdata = Wage_test)
paste0("The mse of the Random Forest model is: ", round(mean((Wage_test$wage - as.numeric(rf_pred))^2), 2))

# We have to refit the boosted tree though
set.seed(123)
xgb_model <- xgboost(
  data = X,
  label = y,
  objective = "reg:squarederror",
  nrounds = cv_res$best_iteration,
  eta = 0.01,
  verbose = 0,   # silence the fitting message
  seed = 123     # main seed
)

# fit new data to xgb_model
boost_pred <- predict(xgb_model, newdata = model.matrix(~ . - wage - logwage - 1, data = Wage_test))
paste0("The mse of the Boosting model is: ", round(mean((Wage_test$wage - as.numeric(boost_pred))^2), 2))



################################################################################
# Basic Neural Networks with `torch`
options(torch.threshold_call_gc = 4000) # garbage collector for torch
library(torch)        # Core PyTorch in R
library(luz)          # High-level interface for torch
library(torchvision)  # Useful for datasets and transformations

# Set random seed for reproducibility
torch_manual_seed(13)

# Define single-hidden-layer NN module
modnn <- nn_module(
  initialize = function(input_size) {
    # Hidden layer with 64 neurons
    self$hidden <- nn_linear(input_size, 64)
    
    # Activation function
    self$activation <- nn_relu()
    
    # Dropout for regularization (40% dropout)
    self$dropout <- nn_dropout(0.4)
    
    # Output layer (regression output)
    self$output <- nn_linear(64, 1)
  },
  forward = function(x) {
    x |>
      self$hidden() |>      # Linear transformation
      self$activation() |>  # Apply ReLU
      self$dropout() |>     # Apply dropout
      self$output()          # Final output
  }
)


# Set the training data
x <- model.matrix(~ . - wage - logwage - 1, data = Wage_train)   # converts all factors/characters to dummies
y <- Wage_train$wage
x_test <- model.matrix(~ . - wage - logwage - 1, data = Wage_test)   # converts all factors/characters to dummies
y_test <- Wage_test$wage

# find the scale for x numerical variables
x_mean <- colMeans(x[,1:2])
x_sd <- apply(x[,1:2], 2, sd)

# Scale x and x_test
x[,1:2] <- sweep(sweep(x[,1:2], 2, x_mean, "-"), 2, x_sd, "/")
x_test[,1:2] <- sweep(sweep(x_test[,1:2], 2, x_mean, "-"), 2, x_sd, "/")

# Find val_id for validation set
val_id <- sample(1:nrow(x), floor(nrow(x)*2/7))



# Set up model with luz
modnn <- modnn |>
  setup(
    loss = nn_mse_loss(),       # Regression MSE loss
    optimizer = optim_adam,
    metrics = list(luz_metric_mae())
  ) |>
  set_hparams(input_size = ncol(x)) # Specify input size


# Fit the model
fitted <- modnn |>
  fit(
    data = list(x, matrix(y, ncol = 1)), # training
    valid_data = list(x[val_id,], matrix(y[val_id], ncol = 1)), # validation
    epochs = 80,
    # call back for early stopping
    callbacks = list(
      luz_callback_early_stopping(
        monitor = "valid_loss",
        patience = 15
      )
    )
  )

# Show the model structure
fitted$model


# Plot training metrics
plot(fitted)



# Find the predicted with fitted model
pred_test <- predict(fitted, x_test) |>
  as.numeric()

# find the mse of neural networks
paste0("The mse of the Neural Networks model is: ", round(mean((y_test - as.numeric(pred_test))^2), 2))

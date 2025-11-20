################################################################################
# An outdated package for simple Neural Network model
# Example from: https://www.datacamp.com/tutorial/neural-network-models-r
library(tidyverse)
library(neuralnet)


# Convert all character variables to factors
iris = iris |>
  mutate(across(where(is.character), as.factor))

# Check the data
head(iris)

# Split data into training and testing 80/20
set.seed(2025)
data_rows = floor(0.80 * nrow(iris))
train_indices = sample(c(1:nrow(iris)), data_rows)
train_data = iris[train_indices,]
test_data = iris[-train_indices,]

set.seed(2025)
model = neuralnet(
  Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
  data=train_data,
  hidden=c(4,2),
  linear.output = FALSE
)


# Visualize the network
plot(model,rep = "best")


# Model Evaluation
pred = predict(model, test_data)
labels = c("setosa", "versicolor", "virginca")
prediction_label = data.frame(max.col(pred)) %>%     
  mutate(pred=labels[max.col.pred.]) %>%
  select(2) %>%
  unlist()

table(test_data$Species, prediction_label)


# Total prediction accuracy
check = as.numeric(test_data$Species) == max.col(pred)
accuracy = (sum(check)/nrow(test_data))*100
print(accuracy)


################################################################################
# Try to scale before feeding in the data
mean_sd = train_data |>
  # Use reframe instead of summarise since we are having multiple summarized values
  # you will get a warning message with this suggestion if you use summarize
  reframe(across(
    where(is.numeric),
    \(x) c(mean = mean(x, na.rm = TRUE),
           sd   = sd(x,   na.rm = TRUE))
  )) |>
  as.matrix()


# Find the numerical columns
num_cols = sapply(train_data, is.numeric)

# Scale training data
train_data_scaled = train_data
train_data_scaled[, num_cols] = sweep(train_data_scaled[, num_cols], 2, mean_sd[1,], "-")
train_data_scaled[, num_cols] = sweep(train_data_scaled[, num_cols], 2, mean_sd[2,], "/")

# Scale testing data
test_data_scaled = test_data
test_data_scaled[, num_cols] = sweep(test_data_scaled[, num_cols], 2, mean_sd[1,], "-")
test_data_scaled[, num_cols] = sweep(test_data_scaled[, num_cols], 2, mean_sd[2,], "/")


set.seed(2025)
# Now, let's work with the model again
model_Scaled = neuralnet(
  Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
  data=train_data_scaled,
  hidden=c(4,2),
  linear.output = FALSE
)


# Visualize the network
plot(model_Scaled,rep = "best")


# Model Evaluation
pred_scaled = predict(model_Scaled, test_data_scaled)
labels_sacaled = c("setosa", "versicolor", "virginca")
prediction_sacaled_label = data.frame(max.col(pred_scaled)) %>%     
  mutate(pred=labels_sacaled[max.col.pred_scaled.]) %>%
  select(2) %>%
  unlist()

table(test_data_scaled$Species, prediction_sacaled_label)


# Total prediction accuracy
check_scaled = as.numeric(test_data_scaled$Species) == max.col(pred_scaled)
accuracy_scaled = (sum(check_scaled)/nrow(test_data_scaled))*100
print(accuracy_scaled)


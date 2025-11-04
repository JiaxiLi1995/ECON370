################################################################################
### set up
# necessary packages
# install.packges(c("L1pack", "gapminder", "glmnet", "tidyverse", "ggrepel"))
library(tidyverse)
library(ggrepel)  # for non-overlapping labels
set.seed(123)

################################################################################
# Model Fitting: Simple Linear Regression
library(gapminder)
# Subset the Asia countries in year 2007
gp_subset = gapminder[gapminder$continent=="Asia"&gapminder$year==2007,]
g = ggplot(gp_subset,aes(x=gdpPercap, y=lifeExp)) + 
  geom_point()
g

# Plot a fitted line
g + geom_smooth(method='lm',se=FALSE)


# function to calculate MSE and option to plot
# Build a MSE function for gp_subset
MSE = function(b, Graph = F) {
  # b is a vector containing b0 and b1
  e = gp_subset$lifeExp - b[1] - b[2]*gp_subset$gdpPercap
  
  # plot
  if (Graph == T){
    (ggplot(gp_subset,aes(x=gdpPercap, y=lifeExp)) + 
       geom_point() + 
       geom_abline(slope = b[2], intercept = b[1]) +
       labs(title = paste("The MSE is", round(mean(e^2)))) + 
       theme(plot.title = element_text(hjust = 0.5))) |>
      print()
  }
  
  return(mean(e^2))
}


# Here, let's try $b_0 = 60$ and $b_1 = 0.0001$
MSE(b = c(60, 0.0001), Graph = T)


# Optimizing the values
optim(c(60, 0.0001), MSE)

# Existing function for linear regression
Result = lm(lifeExp~gdpPercap, data=gp_subset)
summary(Result)


# LAD regression
lad_res = lad(lifeExp~gdpPercap, data=gp_subset)
summary(lad_res)



################################################################################
# Other Models

# Multiple Linear Regression
Result = lm(lifeExp~gdpPercap + year, data=gapminder)
summary(Result)

# Polynomial Regressions
# Create new squared variable and run lm
Result = gp_subset |>
  mutate(GDPpc_sq = gdpPercap^2) |>
  lm(lifeExp~gdpPercap + GDPpc_sq, data=_)
summary(Result)

# There is another way. We can directly add variable in lm formula:
# Create new squared variable and run lm
Result = lm(lifeExp~gdpPercap + gdpPercap^2, data=gp_subset)
summary(Result)


# Log Regression
# Create new log variable and run lm
Result = gp_subset |>
  mutate(log_GDPpc = log(gdpPercap)) |>
  lm(lifeExp~log_GDPpc, data=_)
summary(Result)

# Ridge, LASSO and Elastic Net
library(glmnet)
#define response variable
y <- mtcars$hp

#define matrix of predictor variables
x <- data.matrix(mtcars[, c('mpg', 'wt', 'drat', 'qsec')])

# Ridge Example
coef(glmnet(x, y, alpha= 0, lambda = 1))
# LASSO Example
coef(glmnet(x, y, lambda = 1))

# Logistic Regression
# Convert 'am' to factor
mtcars$am <- factor(mtcars$am, labels = c("Automatic", "Manual"))

# Fit logistic regression
# Predict transmission based on mpg and hp
Result <- glm(am ~ mpg + hp, data = mtcars, family = binomial)

# View summary
summary(Result)


# K-Means Clustering
# Select two variables
mtcars2 <- mtcars |> select(mpg, hp)

# Scale the data (important for k-means)
mtcars_scaled <- scale(mtcars2)

# Set number of clusters
k <- 3


# Apply k-means clustering
set.seed(123)  # for reproducibility
km <- kmeans(mtcars_scaled, centers = k)

# Add cluster assignments to original data
mtcars2$cluster <- factor(km$cluster)
mtcars2$car <- rownames(mtcars)

# Plot with car names
ggplot(mtcars2, aes(x = mpg, y = hp, color = cluster)) +
  geom_point(size = 4) +
  geom_text_repel(aes(label = car), size = 3) +
  labs(title = paste("k-means clustering of mtcars (k =", k, ")"),
       x = "Miles per Gallon (mpg)", 
       y = "Horsepower (hp)",
       color = "Cluster") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")



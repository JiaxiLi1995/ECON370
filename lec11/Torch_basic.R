################################################################################
# Example from: https://torch.mlverse.org/docs/articles/examples
options(torch.threshold_call_gc = 40)
library(torch)



################################################################################
# Basic-autograd (derivatives for optimization)
# creates example tensors. x requires_grad = TRUE tells that
# we are going to take derivatives over it.
x <- torch_tensor(3, requires_grad = TRUE)
y <- torch_tensor(2)

# executes the forward operation x^2
o <- x^2

# computes the backward operation for each tensor that is marked with
# requires_grad = TRUE
o$backward()

# get do/dx = 2 * x (at x = 3)
x$grad


################################################################################
# Basic NN Module
# creates example tensors. x requires_grad = TRUE tells that
# we are going to take derivatives over it.
dense <- nn_module(
  clasname = "dense",
  # the initialize function tuns whenever we instantiate the model
  initialize = function(in_features, out_features) {
    
    # just for you to see when this function is called
    cat("Calling initialize!")
    
    # we use nn_parameter to indicate that those tensors are special
    # and should be treated as parameters by `nn_module`.
    self$w <- nn_parameter(torch_randn(in_features, out_features))
    self$b <- nn_parameter(torch_zeros(out_features))
    
  },
  # this function is called whenever we call our model on input.
  forward = function(x) {
    cat("Calling forward!\n")
    torch_mm(x, self$w) + self$b
  }
)

model <- dense(3, 1)

# you can get all parameters
model$parameters

# or individually
model$w

model$b

# create an input tensor
x <- torch_randn(10, 3)
y_pred <- model(x)

y_pred


################################################################################
# Dataset
# In deep learning models you don't usually have all your data in RAM
# because you are usually training using mini-batch gradient descent
# thus only needing a mini-batch on RAM each time.

# In torch we use the `datasets` abstraction to define the process of
# loading data. Once you have defined your dataset you can use torch
# dataloaders that allows you to iterate over this dataset in batches.

# Note that datasets are optional in torch. They are jut there as a
# recommended way to load data.

# Below you will see an example of how to create a simple torch dataset
# that pre-process a data.frame into tensors so you can feed them to
# a model.

df_dataset <- dataset(
  "mydataset",
  
  # the input data to your dataset goes in the initialize function.
  # our dataset will take a dataframe and the name of the response
  # variable.
  initialize = function(df, response_variable) {
    self$df <- df[,-which(names(df) == response_variable)]
    self$response_variable <- df[[response_variable]]
  },
  
  # the .getitem method takes an index as input and returns the
  # corresponding item from the dataset.
  # the index could be anything. the dataframe could have many
  # rows for each index and the .getitem method would do some
  # kind of aggregation before returning the element.
  # in our case the index will be a row of the data.frame,
  .getitem = function(index) {
    response <- torch_tensor(self$response_variable[index])
    x <- torch_tensor(as.numeric(self$df[index,]))
    
    # note that the dataloaders will automatically stack tensors
    # creating a new dimension
    list(x = x, y = response)
  },
  
  # It's optional, but helpful to define the .length method returning
  # the number of elements in the dataset. This is needed if you want
  # to shuffle your dataset.
  .length = function() {
    length(self$response_variable)
  }
  
)


# we can now initialize an instance of our dataset.
# for example
mtcars_dataset <- df_dataset(mtcars, "mpg")

# now we can get an item with
mtcars_dataset$.getitem(1)


# Given a dataset you can create a dataloader with
dl <- dataloader(mtcars_dataset, batch_size = 15, shuffle = TRUE)

# we can then loop trough the elements of the dataloader with
coro::loop(for(batch in dl) {
  cat("X size:  ")
  print(batch[[1]]$size())
  cat("Y size:  ")
  print(batch[[2]]$size())
})
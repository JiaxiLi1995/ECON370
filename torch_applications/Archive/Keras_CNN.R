################################################################################
# Convolutional Neural Network in R with Keras3 (Update: May 2025)
# Example from: https://keras3.posit.co/articles/getting_started.html
# For other examples: https://keras3.posit.co/articles/examples/index.html
# MNIST Example
# We can learn the basics of Keras by walking through a simple example: recognizing handwritten digits from the MNIST dataset. MNIST consists of 28 x 28 grayscale images of handwritten digits like these:
library(keras3)

################################################################################
# Clean Data
# The MNIST dataset is included with Keras and can be accessed using the dataset_mnist() function. Here we load the dataset then create variables for our test and training data:
mnist = dataset_mnist()
x_train = mnist$train$x
y_train = mnist$train$y
x_test = mnist$test$x
y_test = mnist$test$y

# The x data is a 3-d array (images, width, height) of grayscale values. To prepare the data for training we convert the 3-d arrays into matrices by reshaping width and height into a single dimension (28x28 images are flattened into length 784 vectors). Then, we convert the grayscale values from integers ranging between 0 to 255 into floating point values ranging between 0 and 1:
# reshape
x_train = array_reshape(x_train, c(nrow(x_train), 784))
x_test = array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train = x_train / 255
x_test = x_test / 255

# The y data is an integer vector with values ranging from 0 to 9. To prepare this data for training we one-hot encode the vectors into binary class matrices using the Keras to_categorical() function:
y_train = to_categorical(y_train, 10)
y_test = to_categorical(y_test, 10)

# Check the data visually
# Pick an image to visualize, e.g., the first one
K = 10
digit <- mnist$test$x[K,,]
correct <- max.col(y_test)[K]-1

# MNIST images are 28x28 pixels, invert axes for proper display
image(
  t(apply(digit, 2, rev)),  # rotate for correct orientation
  col = gray.colors(256),
  axes = FALSE,
  main = paste("Label:", correct) # add the correct number
)


################################################################################
# Defining the Model
# The core data structure of Keras is a model, a way to organize layers. The simplest type of model is the Sequential model, a linear stack of layers.
# We begin by creating a sequential model and then adding layers using the pipe (|>) operator:
model = keras_model_sequential(input_shape = c(784))
model |>
  layer_dense(units = 256, activation = 'relu') |>
  layer_dropout(rate = 0.4) |>
  layer_dense(units = 128, activation = 'relu') |>
  layer_dropout(rate = 0.3) |>
  layer_dense(units = 10, activation = 'softmax')

# The input_shape argument to the first layer specifies the shape of the input data (a length 784 numeric vector representing a grayscale image). The final layer outputs a length 10 numeric vector (probabilities for each digit) using a softmax activation function.
# Use the summary() function to print the details of the model:
summary(model)
# plot the model
plot(model)




# Next, compile the model with appropriate loss function, optimizer, and metrics:
model |> compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)


################################################################################
# Training and Evaluation
# Use the fit() function to train the model for 30 epochs using batches of 128 images:
history <- model |> fit(
  x_train, y_train,
  epochs = 30, batch_size = 128,
  validation_split = 0.2
)


# The history object returned by fit() includes loss and accuracy metrics which we can plot
plot(history)

################################################################################
# Testing
# Evaluate the model's performance on the test data:
model |> evaluate(x_test, y_test)


# Generate predictions on new data:
probs <- model |> predict(x_test)
max.col(probs) - 1L

# Keras provides a vocabulary for building deep learning models that is simple, elegant, and intuitive. Building a question answering system, an image classification model, a neural Turing machine, or any other model is just as straightforward.




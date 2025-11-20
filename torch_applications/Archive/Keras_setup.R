################################################################################
# Neural Network Models in R with Keras3 (Update: May 2025)
# Note that keras is deprecated. The new version is keras3
# The GPU support for Windows is suspended, we will talk about another way to do this
# This method uses python in the back, which is not nice. That is why I recommend torch
# Here is the link for keras3: https://keras3.posit.co/index.html
# Here is a old guide for keras (but not keras3) installation: https://tensorflow.rstudio.com/install/index.html
# You only need to install and set up once!


# Set up python for keras3 and tensorflow. Note, in 11/8/2025, the package only supports python version 3.9-3.11
# install.packages("remotes", "reticulate", "tensorflow")
# install.packages("remotes")
# remotes::install_github("rstudio/tensorflow")
# You can download directly in R
# install.packages("keras3")
# Or get the github version
# remotes::install_github("rstudio/keras3")

# Install Python 3.11
library(tensorflow)
library(reticulate)
# The current version is from: https://www.python.org/downloads/release/python-31114/
install_python(version = "3.11:latest")  # replace with the latest 3.11.x if needed

# Tell R to use the installed Python
# You will find the path after installation. Mine is: C:/Users/lijia/AppData/Local/r-reticulate/r-reticulate/pyenv/pyenv-win/versions/3.11.9/python.exe
use_python("C:/Users/lijia/AppData/Local/r-reticulate/r-reticulate/pyenv/pyenv-win/versions/3.11.9/python.exe", required = TRUE)
# Create a new virtual environment
virtualenv_create("r-tensorflow", python = "C:/Users/lijia/AppData/Local/r-reticulate/r-reticulate/pyenv/pyenv-win/versions/3.11.9/python.exe")
use_virtualenv("r-tensorflow", required = TRUE)

# Install a specific TensorFlow version compatible with Python 3.11
install_tensorflow(version = "2.20.0", envname = "r-tensorflow")  # latest compatible stable version

library(reticulate)
py_config()

# # Add kera
# install_keras(backend = "tensorflow", envname = "r-tensorflow")


# Install Graphviz
# Download Graphviz if you do not have it: https://graphviz.org/download/
# For Windows and a modern Keras3 setup, you should use the 64-bit EXE installer
# During installation, check “Add Graphviz to system PATH”.

# Install Plotting tools
reticulate::py_install("pydot", pip = TRUE, envname = "r-tensorflow")
reticulate::conda_install(packages = "graphviz", envname = "r-tensorflow")

# Install transformers and torch in your virtualenv
reticulate::py_install(
  packages = c("transformers", "torch", "tf-keras"),
  envname = "r-tensorflow",
  pip = TRUE
)

# Close all R program and reopen


################################################################################
# Also download the data for demonstration!
# Restart R session first
library(keras3)
mnist <- dataset_mnist()
################################################################################
library(reticulate)
# Use a Python virtualenv with transformers installed
use_virtualenv("r-tensorflow", required = TRUE)

# Import Python packages
transformers <- import("transformers")
torch <- import("torch")

# Load a Hugging Face pipeline (example: sentiment-analysis)
# You can also use some other models, such as: cardiffnlp/twitter-roberta-base-sentiment
pipeline <- transformers$pipeline(
  "sentiment-analysis",
  model = "distilbert-base-uncased-finetuned-sst-2-english",
  revision = "714eb0f"
)

# Run on text
result <- pipeline("I love using R with Hugging Face!")
print(result)
# Score is the model's predictive probability of that given label to be true.


################################################################################
# ChatGPT also gives this answer without using python in the background
# I have not tried it. Please try it if you are interested
library(httr)
library(jsonlite)

API_TOKEN <- "YOUR_HUGGINGFACE_API_TOKEN"

res <- POST(
  "https://api-inference.huggingface.co/models/distilbert-base-uncased-finetuned-sst-2-english",
  add_headers(Authorization = paste("Bearer", API_TOKEN)),
  body = toJSON(list(inputs = "I love R!")),
  encode = "json"
)

content <- content(res)
print(content)

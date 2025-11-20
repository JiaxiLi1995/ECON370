# This is the sentiment analysis example using bag or words
# Example from: https://github.com/trinker/sentimentr

if (!require("pacman")) install.packages("pacman")
pacman::p_load_current_gh("trinker/lexicon", "trinker/sentimentr")

mytext <- c(
  'do you like it?  But I hate really bad dogs',
  'I am the best friend.',
  'Do you really like it?  I\'m not a fan'
)

mytext <- get_sentences(mytext)
sentiment(mytext)

##    element_id sentence_id word_count  sentiment
## 1:          1           1          4  0.2500000
## 2:          1           2          6 -1.8677359
## 3:          2           1          5  0.5813777
## 4:          3           1          5  0.4024922
## 5:          3           2          4  0.0000000

# Sentiment analysis function
text <- "Not sure how to feel about my new washing machine. Great color, but hard to figure"
sentiment_result <- sentiment(text)
# Note that print does not work here
sentiment_result
sentiment_result$sentiment

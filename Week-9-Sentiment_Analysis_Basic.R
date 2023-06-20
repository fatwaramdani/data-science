# Install required packages
install.packages("tidyverse")
install.packages("syuzhet")

# Load required libraries
library(tidyverse)
library(syuzhet)

# Define a function for sentiment analysis
sentiment_analysis <- function(text) {
  # Perform sentiment analysis
  sentiment_scores <- get_sentiment(text)
  
  # Create a data frame with sentiment scores
  sentiment_data <- data.frame(
    Sentence = text,
    SentimentScore = sentiment_scores
  )
  
  # Return the sentiment data
  return(sentiment_data)
}

# Example text
text <- c(
  "I love this product, it's amazing!",
  "This movie is so boring, I hated it.",
  "The weather today is perfect.",
  "I feel so happy and excited."
)

# Perform sentiment analysis
results <- sentiment_analysis(text)

# Plot sentiment scores using ggplot2
ggplot(results, aes(x = Sentence, y = SentimentScore)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 0.5) +
  labs(title = "Sentiment Analysis", x = "Sentence", y = "Sentiment Score")

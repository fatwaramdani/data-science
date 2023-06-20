# Creating sample data
customer_reviews <- c("The product is amazing!", "I'm very happy with my purchase.", "The quality is poor.", "It's a great value for money.", "I would highly recommend it.", "I'm disappointed with the service.", "The product arrived damaged.")

# Install and load necessary packages
install.packages("tidyverse")
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("RColorBrewer")

library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Perform text processing
# Convert customer reviews to a corpus
corpus <- Corpus(VectorSource(customer_reviews))

# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Remove punctuation
corpus <- tm_map(corpus, removePunctuation)

# Remove numbers
corpus <- tm_map(corpus, removeNumbers)

# Remove whitespace
corpus <- tm_map(corpus, stripWhitespace)

# Remove common English stopwords
corpus <- tm_map(corpus, removeWords, stopwords("english"))

# Stemming (reducing words to their root form)
corpus <- tm_map(corpus, stemDocument)

# Perform text analysis
# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert term-document matrix to a matrix
matrix <- as.matrix(tdm)

# Get word frequencies
word_freq <- colSums(matrix)

# Convert word frequencies to a data frame
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

# Sort word frequencies in descending order
word_freq_df <- word_freq_df[order(word_freq_df$freq, decreasing = TRUE), ]

# Generate a word cloud
wordcloud(words = word_freq_df$word, freq = word_freq_df$freq, scale = c(4, 0.5),
          min.freq = 1, max.words = 100, random.order = FALSE, 
          colors = brewer.pal(8, "Dark2"))

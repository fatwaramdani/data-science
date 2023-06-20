# Load the necessary libraries
library(tm)  # For text mining
library(dplyr)  # For data manipulation
library(tidyr)  # For data cleaning

# Read the data
data <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8),
                   comment = c("Great product! I love it.",
                               "The service was terrible.",
                               "This is an awesome book.",
                               "I'm not satisfied with the quality.",
                               "The food was delicious.",
                               "What a great product!",
                               "This is great!",
                               "Awesome, I am satisfied"))

# Create a corpus of the comments column
corpus <- Corpus(VectorSource(data$comment))

# Preprocess the text
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Create a term document matrix
tdm <- TermDocumentMatrix(corpus)

# Inspect the document-term matrix
inspect(tdm)

# Convert the tdm to a matrix
m <- as.matrix(tdm)

# Get the frequency of each word
v <- sort(rowSums(m), decreasing=TRUE)

# Create a dataframe with the words and their frequency
df <- data.frame(word = names(v), freq = v)

set.seed(1234) # for reproducibility 

# Plot the word cloud
wordcloud(words = df$word, freq = df$freq, min.freq = 1, relative_scaling=1,
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))
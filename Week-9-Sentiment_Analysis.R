# Dowload data: https://github.com/finnstats/finnstats/blob/main/Data1.csv

# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/DATA/WEEK 9")

apple <- read.csv("Data1.csv", header = T)
str(apple)

# Preprocesing
library(tm)
corpus <- iconv(apple$text)
corpus <- Corpus(VectorSource(corpus))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub,
                   pattern = 'stocks',
                   replacement = 'stock')
cleanset <- tm_map(cleanset, stemDocument)
cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])

tdm <- TermDocumentMatrix(cleanset)
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]

# Sort by descreasing value of frequency
tdm_v <- sort(rowSums(tdm),decreasing=TRUE)
tdm_d <- data.frame(word = names(tdm_v),freq=tdm_v)
# Display the top 5 most frequent words
head(tdm_d, 10)

# Plot the most frequent words
barplot(tdm_d[1:10,]$freq, las = 2, names.arg = tdm_d[1:10,]$word,
        col ="lightgreen", main ="Top 10 most frequent words",
        ylab = "Word frequencies")

w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,
        las = 2,
        col = rainbow(50),
        ylab = 'Count',
        main = 'Words Frequency')

library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(123)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F,
          min.freq = 5,
          colors = brewer.pal(8, 'Dark2'),
          scale = c(5, 0.3),
          rot.per = 0.7)

library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'diamond', #'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', and 'star'.
           rotateRatio = 0.5,
           minSize = 1)

library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

apple <- read.csv("Data1.csv", header = T)

tweets <- iconv(apple$text)

s <- get_nrc_sentiment(tweets) #takes time

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores Tweets')

library(reshape2)
library(tidytext)

w %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "green"),
                   max.words = 100)

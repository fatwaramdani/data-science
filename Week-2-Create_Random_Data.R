# Install required libraries
install.packages(c("random", "data.table"))

# Load required libraries
library(random)
library(data.table)

# Set seed for reproducibility
set.seed(123)

# Generate random data
n <- 15  # Number of rows
age <- sample(18:65, n, replace = TRUE)  # Random ages between 18 and 65
gender <- sample(c("Male", "Female"), n, replace = TRUE)  # Random gender
height <- round(rnorm(n, mean = 170, sd = 10), 2)  # Random heights with mean 170 and sd 10
weight <- round(rnorm(n, mean = 70, sd = 10), 2)  # Random weights with mean 70 and sd 10
commute_type <- sample(c("Public transportation", "Car", "Walking"), n, replace = TRUE)  # Random commute types

# Create data frame
data <- data.frame(Age = age, Gender = gender, Height = height, Weight = weight, Commute_Type = commute_type)

# View data
View(data)

# Write data to CSV
write.csv(data, file = "random_data.csv", row.names = FALSE)

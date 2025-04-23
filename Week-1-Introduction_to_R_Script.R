# Check the current working directory
getwd()

# Set working directory
setwd("D:/DATA/DATA SCIENCE FOR SOCIAL SCIENCE/SPRING2025") # Replace with your directory

# Load libraries
library(tidyverse)
library(corrplot)

# Load dataset
data <- read.csv("education_health.csv")

# Explore data
head(data)
summary(data)
str(data)

# Convert to factors
data$education <- as.factor(data$education)
data$gender <- as.factor(data$gender)
data$self_reported_health <- as.factor(data$self_reported_health)

# Visualizations
# Self-reported health by education
ggplot(data, aes(x = education, fill = self_reported_health)) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Self-Reported Health by Education")

# BMI by education
ggplot(data, aes(x = education, y = bmi)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "BMI Distribution by Education Level")

# Chronic illness by education
ggplot(data, aes(x = education, fill = as.factor(chronic_illness))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", fill = "Chronic Illness", title = "Chronic Illness Rate by Education")

# Logistic regression
model <- glm(chronic_illness ~ education + age + gender + income + bmi, 
             data = data, family = binomial)
summary(model)

# Odds ratios and confidence intervals
exp(coef(model))
exp(confint(model))

# Predict probabilities
data$predicted_prob <- predict(model, type = "response")

# Predicted probability by education
ggplot(data, aes(x = education, y = predicted_prob)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Predicted Probability of Chronic Illness by Education Level",
       y = "Predicted Probability")

# Correlation matrix
numeric_data <- data %>% 
  select(age, income, bmi, predicted_prob)

cor_matrix <- cor(numeric_data)
print(cor_matrix)

# Correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black"





attach(mtcars)
head(mtcars)
plot(wt, mpg)
abline(lm(mpg~wt))
title("Regression of MPG on Weight")
install.packages("ggplot2")
library(ggplot2)
scatter_plot <- ggplot(mtcars, aes(x = wt, y = mpg, color = "#f7aa58")) +
  geom_point() +  # Add points to the plot
  labs(x = "Weight", y = "MPG", title = "Scatterplot of MPG vs Weight") +  # Add axis labels and plot title
  theme(plot.title = element_text(face = "bold")) +
  theme(legend.position = 'none') 
print(scatter_plot)


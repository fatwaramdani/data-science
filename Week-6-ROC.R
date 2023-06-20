# Load required libraries
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)


# Load the iris dataset
data(iris)
view(iris)

# Split the dataset into training and testing sets
set.seed(123)
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Decision Tree
# Train the decision tree model
dt_model <- rpart(Species ~ ., data = trainData, method = "class")
rpart.plot(dt_model)

# Predict using the decision tree model
dt_pred <- predict(dt_model, testData, type = "class")

# Calculate the confusion matrix for the decision tree
dt_confusion <- confusionMatrix(dt_pred, testData$Species)
print("Confusion Matrix - Decision Tree:")
print(dt_confusion$table)

# Random Forest
# Train the random forest model
rf_model <- randomForest(Species ~ ., data = trainData, ntree = 100)
print(rf_model)

# Predict using the random forest model
rf_pred <- predict(rf_model, testData)
print(rf_pred)

# Calculate the confusion matrix for the random forest
rf_confusion <- confusionMatrix(rf_pred, testData$Species)
print("Confusion Matrix - Random Forest:")
print(rf_confusion$table)

# fit model CART
fit <- rpart(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4], type="class")
# summarize accuracy
table(predictions, iris$Species)

# fit model
fit <- randomForest(Species~., data=iris)
# summarize the fit
summary(fit)
# make predictions
predictions <- predict(fit, iris[,1:4])
# summarize accuracy
table(predictions, iris$Species)

###############################################################################

library(pROC)

# convert the species column to a binary variable
iris$binary_species <- ifelse(iris$Species == "versicolor", 1, 0)

# create a model to predict the binary species variable
model <- glm(binary_species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, family = "binomial")

# predict the probabilities of the binary species variable
predicted_prob <- predict(model, type = "response")

# create a ROC curve object
roc_obj <- roc(iris$binary_species, predicted_prob)

# plot the ROC curve
plot(roc_obj, main = "ROC Curve - Versicolor", col = "blue", lwd = 2)

# add the area under the curve (AUC) to the plot
legend("bottomright", paste("AUC =", round(auc(roc_obj), 2)), bty = "n", cex = 1.2, col = "blue")

library(pROC)

# convert the species column to a binary variable
iris$binary_species <- ifelse(iris$Species == "setosa", 1, 0)

# create a model to predict the binary species variable
model <- glm(binary_species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, family = "binomial")

# predict the probabilities of the binary species variable
predicted_prob <- predict(model, type = "response")

# create a ROC curve object
roc_obj <- roc(iris$binary_species, predicted_prob)

# plot the ROC curve
plot(roc_obj, main = "ROC Curve - Setosa", col = "blue", lwd = 2)

# add the area under the curve (AUC) to the plot
legend("bottomright", paste("AUC =", round(auc(roc_obj), 2)), bty = "n", cex = 1.2, col = "blue")

library(pROC)

# convert the species column to a binary variable
iris$binary_species <- ifelse(iris$Species == "virginica", 1, 0)

# create a model to predict the binary species variable
model <- glm(binary_species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, family = "binomial")

# predict the probabilities of the binary species variable
predicted_prob <- predict(model, type = "response")

# create a ROC curve object
roc_obj <- roc(iris$binary_species, predicted_prob)

# plot the ROC curve
plot(roc_obj, main = "ROC Curve - Virginica", col = "blue", lwd = 2)

# add the area under the curve (AUC) to the plot
legend("bottomright", paste("AUC =", round(auc(roc_obj), 2)), bty = "n", cex = 1.2, col = "blue")

###############################################################################

library(randomForest)

# Set the seed for reproducibility
set.seed(123)

# Load the Iris dataset
data(iris)

# Number of Monte Carlo iterations
n_iterations <- 1000

# Initialize a vector to store the accuracy of each iteration
accuracy <- vector("numeric", n_iterations)

# Monte Carlo iterations
for (i in 1:n_iterations) {
  # Randomly split the dataset into training and testing sets
  indices <- sample(1:nrow(iris), nrow(iris) * 0.7)  # 70% for training
  train_data <- iris[indices, ]
  test_data <- iris[-indices, ]
  
  # Train a Random Forest model
  rf_model <- randomForest(Species ~ ., data = train_data, ntree = 100)
  print(rf_model)
  
  # Predict the classes for the test data
  predictions <- predict(rf_model, test_data)
  
  # Calculate the accuracy of the model
  correct_predictions <- sum(predictions == test_data$Species)
  accuracy[i] <- correct_predictions / nrow(test_data)
}

# Calculate the average accuracy across all iterations
avg_accuracy <- mean(accuracy)

# Print the results
cat("Average Accuracy:", avg_accuracy, "\n")


#############################################################################
# Load required libraries
library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(nnet)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Load iris dataset
data(iris)

# Split the dataset into training and testing sets
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Perform classification using decision tree
dtModel <- rpart(Species ~ ., data = trainData, method = "class")
dtPred <- predict(dtModel, testData, type = "class")
dtAcc <- mean(dtPred == testData$Species)

# Perform classification using random forest
rfModel <- randomForest(Species ~ ., data = trainData)
rfPred <- predict(rfModel, testData)
rfAcc <- mean(rfPred == testData$Species)

# Perform classification using support vector machines (SVM)
svmModel <- svm(Species ~ ., data = trainData)
svmPred <- predict(svmModel, testData)
svmAcc <- mean(svmPred == testData$Species)

# Perform classification using neural network (NN)
nnModel <- nnet(Species ~ ., data = trainData, size = 10)
nnPred <- predict(nnModel, testData, type = "class")
nnAcc <- mean(nnPred == testData$Species)

# Define Monte Carlo function for evaluating model accuracy
monteCarloAccuracy <- function(model, data, nIterations) {
  accuracy <- vector("numeric", length = nIterations)
  for (i in 1:nIterations) {
    trainIndex <- createDataPartition(data$Species, p = 0.7, list = FALSE)
    trainData <- data[trainIndex, ]
    testData <- data[-trainIndex, ]
    modelFit <- model(trainData)
    pred <- predict(modelFit, testData)
    accuracy[i] <- mean(pred == testData$Species)
  }
  return(accuracy)
}

# Perform Monte Carlo simulation for decision tree
dtAccMC <- monteCarloAccuracy(function(data) rpart(Species ~ ., data = data, method = "class"),
                              iris, nIterations = 100)

# Perform Monte Carlo simulation for random forest
rfAccMC <- monteCarloAccuracy(function(data) randomForest(Species ~ ., data = data),
                              iris, nIterations = 100)

# Perform Monte Carlo simulation for SVM
svmAccMC <- monteCarloAccuracy(function(data) svm(Species ~ ., data = data),
                               iris, nIterations = 100)

# Perform Monte Carlo simulation for NN
nnAccMC <- monteCarloAccuracy(function(data) nnet(Species ~ ., data = data, size = 10),
                              iris, nIterations = 100)

# Combine accuracy results
method <- c(rep("Decision Tree", 1),
            rep("Random Forest", 1),
            rep("SVM", 1),
            rep("NN", 1),
            rep("Decision Tree (MC)", 100),
            rep("Random Forest (MC)", 100),
            rep("SVM (MC)", 100),
            rep("NN (MC)", 100))

accuracy <- c(dtAcc, rfAcc, svmAcc, nnAcc, dtAccMC, rfAccMC, svmAccMC, nnAccMC)

# Create a data frame for plotting
results <- data.frame(Method = method, Accuracy = accuracy)

# Plot accuracy using ggplot
ggplot(results, aes(x = Method, y = Accuracy, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  xlab("Method") +
  ylab("Accuracy") +
  ggtitle("Classification Accuracy Comparison") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


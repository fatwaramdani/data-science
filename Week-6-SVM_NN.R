# Load required packages
library(e1071)   # For SVM
library(nnet)    # For Neural Networks
library(ggplot2) # For plotting

# Load iris dataset
data(iris)

# Split the dataset into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(iris), nrow(iris) * 0.7) # 70% for training
train_data <- iris[train_indices, ]
test_data <- iris[-train_indices, ]

# Support Vector Machines (SVM)
# Train SVM model
svm_model <- svm(Species ~ ., data = train_data)

# Make predictions on test data
svm_predictions <- predict(svm_model, newdata = test_data)

# Calculate accuracy, precision, recall, F1 score
svm_confusion <- table(Actual = test_data$Species, Predicted = svm_predictions)
svm_accuracy <- sum(diag(svm_confusion)) / sum(svm_confusion)
svm_precision <- diag(svm_confusion) / colSums(svm_confusion)
svm_recall <- diag(svm_confusion) / rowSums(svm_confusion)
svm_f1_score <- 2 * (svm_precision * svm_recall) / (svm_precision + svm_recall)

# Print SVM performance metrics
cat("Support Vector Machines (SVM):\n")
cat("Accuracy:", svm_accuracy, "\n")
cat("Precision:", svm_precision, "\n")
cat("Recall:", svm_recall, "\n")
cat("F1 Score:", svm_f1_score, "\n\n")

# Neural Networks
# Train Neural Networks model
nn_model <- nnet(Species ~ ., data = train_data, size = 10)

# Make predictions on test data
nn_predictions <- predict(nn_model, newdata = test_data, type = "class")

# Calculate accuracy, precision, recall, F1 score
nn_confusion <- table(Actual = test_data$Species, Predicted = nn_predictions)
nn_accuracy <- sum(diag(nn_confusion)) / sum(nn_confusion)
nn_precision <- diag(nn_confusion) / colSums(nn_confusion)
nn_recall <- diag(nn_confusion) / rowSums(nn_confusion)
nn_f1_score <- 2 * (nn_precision * nn_recall) / (nn_precision + nn_recall)

# Print Neural Networks performance metrics
cat("Neural Networks:\n")
cat("Accuracy:", nn_accuracy, "\n")
cat("Precision:", nn_precision, "\n")
cat("Recall:", nn_recall, "\n")
cat("F1 Score:", nn_f1_score, "\n\n")

# Plot using ggplot
# Combine actual and predicted values for test data
test_data$Predicted_SVM <- svm_predictions
test_data$Predicted_NN <- nn_predictions

# Plot SVM predictions
ggplot(test_data, aes(Petal.Width, Petal.Length, color = Predicted_SVM)) +
  geom_point() +
  labs(title = "SVM Predictions", color = "Predicted") +
  theme_minimal()

# Plot Neural Networks predictions
ggplot(test_data, aes(Petal.Width, Petal.Length, color = Predicted_NN)) +
  geom_point() +
  labs(title = "Neural Networks Predictions", color = "Predicted") +
  theme_minimal()


# Create a data frame for performance metrics
metrics_df <- data.frame(
  Method = c(rep("SVM", length(svm_accuracy)), rep("NN", length(nn_accuracy))),
  Metric = rep(c("Accuracy", "Precision", "Recall", "F1 Score"), each = length(svm_accuracy)),
  Value = c(svm_accuracy, svm_precision, svm_recall, svm_f1_score, nn_accuracy, nn_precision, nn_recall, nn_f1_score)
)

# Plot the performance metrics
ggplot(metrics_df, aes(x = Metric, y = Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Metrics Comparison", x = "Metric", y = "Value") +
  scale_fill_manual(values = c("blue", "red"), labels = c("SVM", "NN")) +
  theme_minimal()
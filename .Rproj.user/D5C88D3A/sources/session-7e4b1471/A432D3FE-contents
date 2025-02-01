library(dplyr)
library(randomForest)
library(caret)
library(scales)

# Load  featured dataset
data_frame <- read.csv("feature_extraction_data.csv")
head(data_frame)

# Step 1: Train-Test Split
set.seed(123)
n <- nrow(data_frame)
train_indices <- sample(1:n, size = 0.8 * n)
train_data <- data_frame[train_indices, ]
test_data <- data_frame[-train_indices, ]

# Step 2: Train Random Forest Model

rf_model <- randomForest(AskPrice ~ ., data = train_data,ntree=300,mtry=9, importance = TRUE)

# Step 3: Make Predictions

predictions_rf <- predict(rf_model, newdata = test_data)

# Step 4: Model Evaluation

evaluate <- function(actual, predicted) {
  mae <- mean(abs(actual - predicted))  # Mean Absolute Error
  mse <- mean((actual - predicted)^2)  # Mean Squared Error
  r2 <- cor(actual, predicted)^2       # Coefficient of Determination (R²)
  mae <- format(mae, scientific = FALSE, digits = 2)
  mse <- format(mse, scientific = FALSE, digits = 2)
  r2 <- format(r2, scientific = FALSE, digits = 3)
  return(c(MAE = mae, MSE = mse, R2 = r2))
}

# Actual values from test set
actual_values <- test_data$AskPrice

# Evaluate Random Forest Model
rf_metrics <- evaluate(actual_values, predictions_rf)

cat("\nRandom Forest Evaluation Metrics:\n")

cat("MAE: ", rf_metrics["MAE"], "\n")
cat("MSE: ", rf_metrics["MSE"], "\n")
cat("R²: ", rf_metrics["R2"], "\n")

# Accuracy Calculation (using MAPE)
calculate_accuracy <- function(actual, predicted) {
  mape <- mean(abs((actual - predicted) / actual)) * 100  # Mean Absolute Percentage Error
  accuracy <- 100 - mape  # Accuracy as 100 - MAPE
  return(accuracy)
}

# Calculate Random Forest Accuracy
rf_accuracy <- calculate_accuracy(actual_values, predictions_rf)
cat("\nRandom Forest Accuracy (%): ", rf_accuracy, "\n")

# Step 5: Visualization
# 1. Actual vs Predicted Scatter Plot
# Creating a dataframe for ggplot
df <- data.frame(Actual = actual_values, Predicted = predictions_rf)

ggplot(df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter points
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 1) +  # Reference line y = x
  labs(title = "Actual vs Predicted Prices (Random Forest)",
       x = "Actual Prices",
       y = "Predicted Prices") +
  scale_x_continuous(labels = comma) +  # Format x-axis numbers
  scale_y_continuous(labels = comma) +  # Format y-axis numbers
  theme_minimal()

# Creating a dataframe for ggplot
df_residuals <- data.frame(Predicted = predictions_rf, Residuals = residuals_rf)

# Residual Plot using ggplot2
ggplot(df_residuals, aes(x = Predicted, y = Residuals)) +
  geom_point(color = "green", alpha = 0.6) +  # Scatter points
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +  # Reference line at residual = 0
  labs(title = "Residual Plot (Random Forest)",
       x = "Predicted Prices",
       y = "Residuals") +
  scale_x_continuous(labels = comma) +  # Format x-axis numbers
  scale_y_continuous(labels = comma) +  # Format y-axis numbers
  theme_minimal()

# Step 6: Check Actual vs Predicted Values
comparison_df <- data.frame(Actual = actual_values, Predicted = predictions_rf)
cat("\nComparison of Actual vs Predicted Values:\n")
print(head(comparison_df))  # Show the first few rows of the comparison

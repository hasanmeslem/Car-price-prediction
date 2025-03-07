# Load libraries
library(rpart)
library(rpart.plot)
library(caret)

# Load feature-engineered data (already processed as per Feature Engineering script)
data1 <- read.csv("feature_extraction_data.csv", stringsAsFactors = TRUE)

# Split data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(data1), 0.7 * nrow(data1))
train_data <- data1[train_index, ]
test_data <- data1[-train_index, ]

# Check for empty strings in the column 'LogAskPrice'
sum(train_data$LogAskPrice == "")

# Tune Decision Tree hyperparameters using caret
train_data <- na.omit(train_data)
tuned_params <- train(
  LogAskPrice ~ ., 
  data = train_data, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5), # 5-fold cross-validation
  tuneGrid = expand.grid(cp = seq(0.003, 0.03, by = 0.003))
)

# Use the best model parameters
best_cp <- tuned_params$bestTune$cp

dt_model <- rpart(LogAskPrice ~ ., 
                  data = train_data,
                  method = "anova",
                  control = rpart.control(
                    minsplit = 20,
                    minbucket = 10,
                    maxdepth = 10,
                    cp = best_cp
                  ))

# Visualize the decision tree
rpart.plot(dt_model, 
           box.palette = "RdBu",
           shadow.col = "gray",
           nn = TRUE,cex = 0.5)

# Make predictions on the test set
predictions <- predict(dt_model, newdata = test_data)

# Evaluate model performance
rmse <- sqrt(mean((test_data$LogAskPrice - predictions)^2))
r2 <- 1 - sum((test_data$LogAskPrice - predictions)^2) / 
  sum((test_data$LogAskPrice - mean(test_data$LogAskPrice))^2)

# Print results
cat("RMSE (log scale):", round(rmse, 4), "\n")
cat("R-squared:", round(r2, 4), "\n")

# Convert predictions and actual values back to the original scale
original_predictions <- exp(predictions)
original_actual <- exp(test_data$LogAskPrice)

# Calculate accuracy (percentage of predictions within 20% of actual values)
accuracy <- mean(abs((original_actual - original_predictions) / original_actual) <= 0.20) * 100
cat("Model Accuracy (Test Data):", round(accuracy, 2), "%\n")

# Evaluate accuracy on the entire dataset
all_predictions <- predict(dt_model, newdata = data1)
original_all_predictions <- exp(all_predictions)
original_all_actual <- exp(data1$LogAskPrice)
all_accuracy <- mean(abs((original_all_actual - original_all_predictions) / original_all_actual) <= 0.20) * 100
cat("Model Accuracy (Entire Dataset):", round(all_accuracy, 2), "%\n")


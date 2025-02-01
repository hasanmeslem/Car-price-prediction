library(ggplot2)
library(caret)

cleaned_data<-read.csv("cleaned_data.csv")

# Data Splitting for Model
# Set seed for reproducibility
# Split data into training (80%) and testing (20%) sets 

trainIndex <- createDataPartition(cleaned_data$LogAskPrice, p = 0.8, list = FALSE)
trainData <- cleaned_data[trainIndex, ]
testData <- cleaned_data[-trainIndex, ]

# Modelling
model <- lm(LogAskPrice ~ Age+kmDriven+Transmission+Owner+FuelType, data = trainData)
summary(model)
summary(model)$r.squared

# Make predictions on the test data 
predictions <- predict(model, newdata=testData)

# Calculating RMSE
error_lr<-testData$LogAskPrice-predictions
RMSE_lr<-sqrt(mean(error_lr^2))
RMSE_lr

ggplot(data = data.frame(Actual = testData$LogAskPrice, Predicted = predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue") +  # Scatter plot
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Regression line
  labs(title = "Actual vs Predicted Selling Price",
       x = "Actual Car Selling Price",
       y = "Predicted Car Selling Price") +
  theme_minimal()

# Accuracy Calculation (using MAPE)

mape <- mean((error_lr / testData$LogAskPrice)) * 100  # Mean Absolute Percentage Error
accuracy <- 100 -mape   # Accuracy as 100 - MAPE
cat("\nLinear Regression Accuracy (%): ", accuracy, "\n")

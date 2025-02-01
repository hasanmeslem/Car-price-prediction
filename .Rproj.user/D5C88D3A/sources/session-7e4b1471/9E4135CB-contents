library(ggplot2)

#We differentiate between luxurious and regular brands
luxury_brands <- c("BMW", "Mercedes-Benz", "Audi","Tesla","Porsche","Volvo","Lexus","Ferrari", "Lamborghini","Aston Martin","Bentley","Ford","Rolls-Royce" ,"Land Rover")
data1$isLuxury <- data1$Brand %in% luxury_brands

#We extract how many Km were driven per year
data1$kmPerYear <- data1$kmDriven / data1$Age
data1$kmPerYear[is.na(data1$kmPerYear)] <- 0
data1$kmPerYear[is.infinite(data1$kmPerYear)] <- data1$kmDriven[is.infinite(data1$kmPerYear)]

sum(data1$kmPerYear== "")

# Check the levels of the factor
levels(data1$PostedDate)
levels(data1$PostedDate)[1]

# Ensure PostedDate is in Date format (if it is not already)
data1$PostedDate <- as.Date(data1$PostedDate, format = "%Y-%m-%d")  # Adjust format if necessary

# Now, create the PostedMonths variable
data1$PostedMonths <- (as.integer(format(data1$PostedDate, "%Y")) - 2023) * 12 + (as.integer(format(data1$PostedDate, "%m")) - 11)

# Check the result
head(data1$PostedMonths)

#We combine both brand and model into 1 feature
data1$Brand.model <-paste0(data1$Brand,"-",data1$model)
# Save data with features

#After some visualization, we label encode data
encoded_data <- data1
for (col in names(encoded_data)) if (!is.numeric(encoded_data[[col]])) encoded_data[[col]] <- as.numeric(as.factor(encoded_data[[col]]))
encoded_data <- subset(encoded_data, select = -c(Year, PostedDate, PostedYear))

#After Completing Visualization Step:
#We apply log transormation on km per Year and Ask Price
encoded_data$LogkmPerYear = ifelse(encoded_data$kmPerYear == 0,0,log(encoded_data$kmPerYear))
encoded_data$LogAskPrice = ifelse(encoded_data$AskPrice==0,0,log(encoded_data$AskPrice))

#We now look at the correlation with log ask price
encoded_data$kmPerYear <- NULL
encoded_data$AskPrice <- NULL

correlations <- sapply(encoded_data, function(col) cor(col, encoded_data$LogAskPrice, use="pairwise.complete.obs"))
correlations <- abs(correlations[names(correlations) != "LogAskPrice"])

# Convert to a data frame for ggplot
correlation_df <- data.frame(
  Variable = names(correlations),
  Correlation = correlations
)

ggplot(correlation_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Absolute Correlation of Each Column with LogAskPrice", y = "Correlation Coefficient") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))  # Rotate axis labels and adjust size

#We filter out features with low correlation with log ask price
encoded_data <- subset(encoded_data, select = -c(Brand, model, PostedMonths, Brand.model))
write.csv(data1, "feature_extraction_data.csv", row.names=FALSE)

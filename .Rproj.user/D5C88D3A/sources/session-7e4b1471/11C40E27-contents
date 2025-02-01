library(ggplot2)

#Load data set and view its structure
data<-read.csv("used_car_dataset.csv")
str(data)

# Data cleaning
data <- na.omit(data)  
colSums(is.na(data))

# Removing duplicates
data <- unique(data)

# Remove unnecessary columns
data$AdditionInfo<-NULL

#Changing PostedDate column into date format for later analysis
Sys.setlocale("LC_TIME", "C")  # Set to English for date parsing"convert NOV -> 11"
data$PostedDate <- paste0(data$PostedDate, "-15")
data$PostedDate <- as.Date(strptime(data$PostedDate, format="%b-%y-%d"))

#Extracting year from PostedDate
data$PostedYear <- as.integer(format(data$PostedDate, "%Y"))

#Discovering and fixing Age inconsistency isssue
unique(data$Year + data$Age == data$PostedYear)
data$Age = (data$PostedYear - data$Year)

#Checking nominal values
unique(data$Brand)
unique(data$kmDriven)
unique(data$model)
unique(data$Transmission)
unique(data$Owner)
unique(data$FuelType)

#Changing numeric strings into numeric
data$kmDriven = gsub(",", "", data$kmDriven)
data$kmDriven = sub("km", "", data$kmDriven)
data$kmDriven = as.integer(data$kmDriven)

data$AskPrice = gsub(",", "", data$AskPrice)
data$AskPrice = sub("₹", "", data$AskPrice)
data$AskPrice = as.integer(data$AskPrice)

# Boxplot for kmDriven
ggplot(data, aes(y = kmDriven)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.color = "red") +
  labs(title = "Boxplot of kmDriven", y = "Kilometers Driven") +
  theme_minimal()

# Data cleaning
data <- na.omit(data)  
colSums(is.na(data))

#Fixing numeric data outliers problem
kmDriven.Q1 <- quantile(data$kmDriven, 0.25)
kmDriven.Q3 <- quantile(data$kmDriven, 0.75)
kmDriven.IQR <- IQR(data$kmDriven)
data$kmDriven[data$kmDriven < kmDriven.Q1 - 1.5*kmDriven.IQR] <- kmDriven.Q1 - 1.5*kmDriven.IQR
data$kmDriven[data$kmDriven > kmDriven.Q3 + 1.5*kmDriven.IQR] <- kmDriven.Q3 + 1.5*kmDriven.IQR

ggplot(data, aes(y = AskPrice)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.color = "red") +
  labs(title = "Boxplot of AskPrice", y = "Car Selling Price (₹)") +
  theme_minimal()
AskPrice.Q1 <- quantile(data$AskPrice, 0.25)
AskPrice.IQR <- IQR(data$AskPrice)
data$AskPrice[data$AskPrice < AskPrice.Q1 - 1.5*AskPrice.IQR] = AskPrice.Q1 - 1.5*AskPrice.IQR

#Log transformation for ask price to reduce high outliers impact
data$LogAskPrice = log(data$AskPrice)

head(data)
str(data)

# Save cleaned data
write.csv(data, "cleaned_data.csv", row.names=FALSE)

cleaned_data<-read.csv("cleaned_data.csv")

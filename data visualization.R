library(gridExtra)
library(dplyr)
library(ggplot2)
library(reshape2)

p1<-ggplot(data,aes(y=data$kmDriven))+geom_boxplot(fill="steelblue",color="black",outlier.color = "red")+labs(title = "Km Driven", y = "") +
  theme_minimal()
p2<-ggplot(data,aes(y=data$Age))+geom_boxplot(fill="steelblue",color="black",outlier.color = "red")+labs(title = "Age", y = "") +
  theme_minimal()
p3<-ggplot(data,aes(y=data$AskPrice))+geom_boxplot(fill="steelblue",color="black",outlier.color = "red")+labs(title = "Ask Price", y = "") +
  theme_minimal()
grid.arrange(p1, p2, p3, nrow = 1)

#We then visualize our newly created features
p1<-ggplot(data1,aes(y=kmPerYear))+geom_boxplot(fill="steelblue",color="black",outlier.color = "red")+labs(title = "Km Driven") +
  theme_minimal()
p2<-ggplot(data1,aes(y=PostedMonths))+geom_boxplot(fill="steelblue",color="black",outlier.color = "red")+labs(title = "Age") +
  theme_minimal()
grid.arrange(p1, p2, nrow = 1)

#We now further inspect the non-normalized distributions
# Plot distribution of Age
ggplot(data1, aes(x = Age)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Age", x = "Age (Years)", y = "Frequency") +
  theme_minimal()

# Plot distribution of AskPrice
ggplot(data1, aes(x = AskPrice)) +
  geom_histogram(bins = 20, fill = "lightcoral", color = "black") +
  labs(title = "Distribution of Asking Price", x = "Price", y = "Frequency") +
  theme_minimal()

# Plot distribution of kmPerYear
ggplot(data1, aes(x = kmPerYear)) +
  geom_histogram(bins = 20, fill = "lightpink", color = "black") +
  labs(title = "km Driven Per Year", x = "km Per Year", y = "Frequency") +
  theme_minimal()

#We now view categorical data distributions

# Plot Count of Cars by Brand
ggplot(data1, aes(x = Brand)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Count of Cars by Brand", x = "Brand", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10))  # Rotate x-axis labels for readability

# Plot Fuel Type Distribution
p1<-ggplot(data1, aes(x = FuelType)) +
  geom_bar(fill = "orange") +
  labs(title = "Fuel Type Distribution", x = "Fuel Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Transmission Type
p2<-ggplot(data1, aes(x = Transmission)) +
  geom_bar(fill = "lightgreen") +
  labs(title = "Transmission Type", x = "Transmission", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot Owner Type Distribution
p3<-ggplot(data1, aes(x = Owner)) +
  geom_bar(fill = "purple") +
  labs(title = "Owner Type", x = "Owner Type", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(p1,p2,p3,nrow=1)


#We then view some relations between numeric data
# Plot Price vs Age of Car
p1<-ggplot(data1, aes(x = Age, y = AskPrice)) +
  geom_point(color = "blue") +
  labs(title = "Price vs Age of Car", x = "Age (Years)", y = "Asking Price") +
  scale_x_continuous(labels = comma) +  # Format x-axis numbers
  scale_y_continuous(labels = comma) +  # Format y-axis numbers
  theme_minimal()

# Plot Price vs km Driven
p2<-ggplot(data1, aes(x = kmDriven, y = AskPrice)) +
  geom_point(color = "red") +
  labs(title = "Price vs km Driven", x = "km Driven", y = "Asking Price") +
  scale_x_continuous(labels = comma) +  # Format x-axis numbers
  scale_y_continuous(labels = comma) +  # Format y-axis numbers
  theme_minimal()
grid.arrange(p1,p2,nrow=1)


p1<-ggplot(data1, aes(x = Age, y = AskPrice)) +
  geom_point(color = "blue") +
  labs(title = "Price vs Age of Car", x = "km per year", y = "Asking Price") +
  scale_x_continuous(labels = comma) +  # Format x-axis numbers
  scale_y_continuous(labels = comma) +  # Format y-axis numbers
  theme_minimal()

# Plot Price vs km Driven
p2<-ggplot(data1, aes(x = kmDriven, y = AskPrice)) +
  geom_point(color = "red") +
  labs(title = "Price vs km Driven", x = "posted months", y = "Asking Price") +
  scale_x_continuous(labels = comma) +  # Format x-axis numbers
  scale_y_continuous(labels = comma) +  # Format y-axis numbers
  theme_minimal()
grid.arrange(p1,p2,nrow=1)

#After labal encoding, we now create the correlation matrix and view heat map

# Create correlation matrix
cor_matrix <- cor(encoded_data, use = "complete.obs")

# Reshape the correlation matrix to long format for ggplot
cor_matrix_melted <- melt(cor_matrix)

# Create the heatmap using ggplot2
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,limit=c(-1,1),space = "Lab",name = "correlation") +
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1,size = 16,hjust = 1), axis.text.y = element_text(angle = 0, hjust = 1,vjust = 1,size = 16))  # Rotate axis labels


#We lastly create bar plot for correlation with ask price
# Create a data frame from the correlations (excluding AskPrice)
correlations_df <- data.frame(
  Variable = names(correlations),
  Correlation = correlations
)

# Create bar plot using ggplot2
ggplot(correlations_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Absolute Correlation of Each Column with AskPrice", x = "Variable", y = "Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle =90, hjust = 1, size = 15))  # Rotate x-axis labels


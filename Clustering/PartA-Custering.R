#loading the required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

#loading the dataset 
wine_dataset<-read_excel("Git hub projects/MachineLearningAssignment/Clustering/Whitewine_v6.xlsx")

#exploring the dataset
head(wine_dataset)

#exploring the structure of the dataset
str(wine_dataset)

#explore the summary
summary(wine_dataset)

#printing the data column wise

# Print the first column
print(wine_dataset$`fixed acidity`)
# Print the second column
print(wine_dataset$`volatile acidity`)

# Continue this for each column...
print(wine_dataset$`citric acid`)
print(wine_dataset$`residual sugar`)
print(wine_dataset$chlorides)
print(wine_dataset$`free sulfur dioxide`)
print(wine_dataset$`total sulfur dioxide`)
print(wine_dataset$density)
print(wine_dataset$pH)
print(wine_dataset$sulphates)
print(wine_dataset$alcohol)
print(wine_dataset$quality)

#performing the scaling of data
scaled_data<-scale(wine_dataset[1:11])

#Detect outliers
# Reshaping the data from wide to long format
boxplot_data <- as.data.frame(scaled_data)
boxplot_data_long <- pivot_longer(boxplot_data, cols = everything(), 
                                  names_to = "variable", values_to = "value")

# Calculate outliers using IQR method
outliers <- boxplot_data_long %>%
  group_by(variable) %>%
  mutate(outlier = ifelse(value < quantile(value, 0.25) - 1.5 * IQR(value) |
                            value > quantile(value, 0.75) + 1.5 * IQR(value), 1, 0)) %>%
  filter(outlier == 1)

# Plot boxplots to visualize outliers
boxplots <- ggplot(boxplot_data_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  geom_point(data = outliers, aes(color = "red")) +
  labs(title = "Boxplot with Outliers Highlighted") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(boxplots)


#Removing outliers
cleaned_data <- scaled_data[-as.numeric(row.names(outliers)), ]
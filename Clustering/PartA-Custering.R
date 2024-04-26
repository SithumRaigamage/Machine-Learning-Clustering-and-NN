# Loading the required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(NbClust)
library(factoextra)
library(cluster)

# Part a

# Loading the dataset 
wine_dataset <- read_excel("Git hub projects/MachineLearningAssignment/Clustering/Whitewine_v6.xlsx")

# Exploring the dataset
head(wine_dataset)

# Exploring the structure of the dataset
str(wine_dataset)

# Explore the summary
summary(wine_dataset)

# Printing the data column-wise
for (i in 1:11) {
  print(wine_dataset[, i])
}

# Performing the scaling of data
scaled_data <- scale(wine_dataset[1:11])

# Detect outliers
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

# Removing outliers
cleaned_data <- scaled_data[-as.numeric(row.names(outliers)), ]

# Visualize the cleaned data using boxplots
cleaned_boxplot_data <- as.data.frame(cleaned_data)
cleaned_boxplot_data_long <- pivot_longer(cleaned_boxplot_data, cols = everything(), 
                                          names_to = "variable", values_to = "value")

cleaned_boxplots <- ggplot(cleaned_boxplot_data_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot of Cleaned Data") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(cleaned_boxplots)

# Visualize the distribution of cleaned data using density plots
density_plots <- lapply(1:11, function(i) {
  ggplot(cleaned_boxplot_data_long, aes(x = value)) +
    geom_density(fill = "skyblue", color = "black") +
    facet_wrap(~ variable, scales = "free") +
    labs(title = paste("Density Plot of", names(cleaned_boxplot_data_long)[i])) +
    theme_minimal()
})
print(density_plots)

# Print summary statistics of cleaned data
summary(cleaned_data)

# Print the first few rows of cleaned data
head(cleaned_data)

# Check the dimensions to ensure outliers were removed
dim(cleaned_data)

#Part b

#number of variables for dataset
no_of_variables<-11 

#method for clustering
method<-"kmeans"

#determine the number of clusters using NbClust library
nb_clust_result<-NbClust(cleaned_data,distance="euclidean",min.nc = 2,max.nc = 10,method = method,index = "all")

#printing the result
print(nb_clust_result)

# Determine the number of clusters using the Elbow method
elbow_method <- fviz_nbclust(cleaned_data, kmeans, method = "wss")

# Print the Elbow method plot
print(elbow_method)
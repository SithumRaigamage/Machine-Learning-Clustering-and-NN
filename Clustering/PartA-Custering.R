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

#determining the number of clusters using NbClust library
nbclust_result<-NbClust(cleaned_data,distance="euclidean",min.nc = 2,max.nc = 10,method = method,index = "all")

# Extract the suggested number of clusters (k) from NbClust results
nbclust_k <- nbclust_result$Best.nc[1]

#printing the result
print(nb_clust_result)
#no of clusters k=8

# Determining the number of clusters using the Elbow method
elbow_method <- fviz_nbclust(cleaned_data, kmeans, method = "wss")

# Extract the suggested number of clusters (k) from the Elbow method
elbow_k <- elbow_method$data$clusters[which.max(elbow_method$data$y)]

# Check the content of elbow_method$data$nb
print(elbow_method$data)

# Print the Elbow method plot
print(elbow_method)

# Determining the number of clusters using Gap statistics
gap_statistic <- clusGap(cleaned_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Extract the suggested number of clusters (k) from Gap statistics
gap_k <- which.max(gap_statistic$Tab[, "gap"])  # Finding the index of the maximum gap statistic
SE_f <- gap_statistic$Tab[gap_k, "SE.sim"]      # Extracting the standard error at that index

# Print the Gap statistics plot

# Print the Gap statistics plot
print(gap_statistic)
#no of cluster is k=9

# Determine the number of clusters using the Silhouette method
silhouette_scores <- sapply(2:10, function(k) {
  kmeans_result <- kmeans(cleaned_data, centers = k)
  mean(silhouette(kmeans_result$cluster, dist(cleaned_data)))
})

# Extract the suggested number of clusters (k) from Silhouette method
silhouette_k <- which.max(silhouette_scores) + 1
#no of cluster k=10

# Plot Silhouette method results
plot(2:10, silhouette_scores, type = "b", pch = 19, xlab = "Number of clusters", ylab = "Silhouette Score")
abline(v = silhouette_k, col = "red")

# Print the number of clusters suggested by each method
cat("NbClust suggested", nbclust_k, "clusters.\n")
cat("Elbow method suggested", elbow_k, "clusters.\n")
cat("Gap statistics suggested", gap_k, "clusters.\n")
cat("Silhouette method suggested", silhouette_k, "clusters.\n")
# Loading the required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(NbClust)
library(factoextra)

#1st SubTask Objectives

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

# Part b

# Method for clustering
method <- "kmeans"

# Determine the number of clusters using NbClust library
nbclust_result <- NbClust(cleaned_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = method, index = "all")
#number of clusters-k=2

# Print the result
print(nbclust_result)

# Determine the number of clusters using the Elbow method
wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- sum(kmeans(cleaned_data, centers = i)$withinss)
}
#assigning the dataframe
elbow_data <- data.frame(Clusters = 1:10, WSS = wss)

# Plotting the Elbow method results
elbow_plot <- ggplot(elbow_data, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of clusters", y = "Within-cluster sum of squares (WSS)", 
       title = "Elbow Method for Optimal K") +
  theme_minimal()

#printing the elbow plot
print(elbow_plot)
#Clustering k is 2 close to 2

library(cluster)
# Determining the number of clusters using Gap statistics
gap_statistic <- clusGap(cleaned_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)

# Plot the Gap statistics
fviz_gap_stat(gap_statistic)

# Print the Gap statistics plot
print(gap_statistic)
#number of cluster =4


# Determine the number of clusters using the Silhouette method
silhouette_scores <- sapply(2:10, function(k) {
  kmeans_result <- kmeans(cleaned_data, centers = k)
  mean(silhouette(kmeans_result$cluster, dist(cleaned_data)))
})

#print the Silhouette data
print(silhouette_scores)


# Plot Silhouette method results
plot(2:10, silhouette_scores, type = "b", pch = 19, xlab = "Number of clusters", ylab = "Silhouette Score")
#no of clusters 9

#NbCluster method -2
#Elbow method -2 (K value taken based on elbow in plot)
#Gap statistics -4
#Silhouette method -9 (K value taken on highest peak on the plot)

#Part C

#Assigning the K value based on the result above I got previously 
#Assigning 2 since NbCluster and Elbow returned 2
k<-2

#K means clustering
kmeans_result <- kmeans(cleaned_data,centers = k)

#printing kmeans result
print(kmeans_result)

#printing kmeans centers
print(kmeans_result$centers)

#printing clustered results
clustered_data <-cbind(cleaned_data,Cluster=kmeans_result$cluster)
print(clustered_data)

#Calculating BSS
BSS <- sum((apply(kmeans_result$centers,1, function(center) sum((center - colMeans(cleaned_data))^2))) * table(kmeans_result$cluster))
#Calculating TSS
TSS <- sum(apply(cleaned_data, 2, function(feature) sum((feature - mean(feature))^2)))
#Calculating WSS
WSS <- kmeans_result$tot.withinss

#Calculate the ratio of BSS and TSS
Ratio_BSS_and_TSS <-BSS/TSS
print(paste("Ratio of BSS/TSS :", Ratio_BSS_and_TSS))

#Part D

#loading the library cluster
library(cluster)

# Computing silhouette width for each observation
sil_width <- silhouette(kmeans_result$cluster,dist(cleaned_data))

#plot the silhouette plot
plot(sil_width, main="Silhouette plot for K-means Clustering")

# Add average silhouette width to plot
avg_sil_width <- mean(sil_width[, "sil_width"])
abline(h = avg_sil_width, lty = 2)

# Print average silhouette width score
print(paste("Average Silhouette Width Score:", avg_sil_width))

#2nd Sub Objective

#Part e

#Performing PCA analysis using prcomp
pca_result <-prcomp(cleaned_data, center = TRUE, scale. = TRUE)

#printing the pca analysis result
print(pca_result)

#printing the explained variance ratio eigenvalues and eigenvectors
summary(pca_result)

# Cumulative proportion of variance explained
cumulative_variance <- cumsum(pca_result$sdev^2 / sum(pca_result$sdev^2))
print(cumulative_variance)

#plot the cumulative proportion of variance explained
plot(cumulative_variance, type = "b", xlab = "Number of Principal Components", ylab = "Cumulative Proportion of Variance Explained", main = "Cumulative Proportion of Variance Explained by Principal Components")

# Choosing  the number of principal components that provide at least or more than cumulative score > 85%
selected_components <- which(cumulative_variance > 0.85)[1]

#printing the no of selected components
print(selected_components)

#transforming the original data with attributes
transformed_data <-as.data.frame(predict(pca_result, newdata = cleaned_data)[, 1:selected_components])

#printing the tranformed_data
print(head(transformed_data))

#part F

#doing automated tools for the new dataset(transformed_data)

# Determine the number of clusters using NbClust library on PCA-based dataset
nbclust_result_pca <- NbClust(transformed_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
print(nbclust_result_pca)
# k is 2

# Determine the number of clusters using the Elbow method on PCA-based dataset
wss_pca <- numeric(10)
for (i in 1:10) {
  wss_pca[i] <- sum(kmeans(transformed_data, centers = i)$withinss)
}
elbow_data_pca <- data.frame(Clusters = 1:10, WSS = wss_pca)
elbow_plot_pca <- ggplot(elbow_data_pca, aes(x = Clusters, y = WSS)) +
  geom_line() +
  geom_point() +
  labs(x = "Number of clusters", y = "Within-cluster sum of squares (WSS)", 
       title = "Elbow Method for Optimal K (PCA-based dataset)") +
  theme_minimal()
print(elbow_plot_pca)
# k is 2

# Determine the number of clusters using Gap statistics on PCA-based dataset
gap_statistic_pca <- clusGap(transformed_data, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
print(gap_statistic_pca)
fviz_gap_stat(gap_statistic_pca)
#k is 3

# Determine the number of clusters using the Silhouette method on PCA-based dataset
silhouette_scores_pca <- sapply(2:10, function(k) {
  kmeans_result_pca <- kmeans(transformed_data, centers = k)
  mean(silhouette(kmeans_result_pca$cluster, dist(transformed_data)))
})
print(silhouette_scores_pca)
plot(2:10, silhouette_scores_pca, type = "b", pch = 19, xlab = "Number of clusters", ylab = "Silhouette Score (PCA-based dataset)")
#k is 9

#NBcluster k - 2
#Elbow method  k - 2
#Gap statistics k - 3
#Silhouette k - 9

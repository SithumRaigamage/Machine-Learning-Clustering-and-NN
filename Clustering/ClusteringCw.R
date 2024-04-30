# Loading the required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(NbClust)
library(factoextra)
library(clusterCrit)
library(cluster)

# Loading the dataset 
wine_dataset <- read_excel("Git hub projects/MachineLearningAssignment/Clustering/Whitewine_v6.xlsx")

# Remove the last column (Quality) as it is not used 
wine_data <- wine_dataset[, -12]

# Exploring the dataset
head(wine_data)

# Exploring the structure of the dataset
str(wine_data)

# Explore the summary
summary(wine_data)

#plot the boxplot without removing outliers
boxplot(wine_data)

# Function to remove outliers using IQR
remove_outliers_IQR <- function(df, column_name) {
  # Calculate Q1, Q3, and IQR
  Q1 <- quantile(df[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  
  # Identify outliers
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  
  # Replace outliers with NA
  df[[column_name]][df[[column_name]] < lower_bound | df[[column_name]] > upper_bound] <- NA
  
  return(df)
}

# Selecting only the attributes given above (attributes 1 to 11)
selected_columns <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar",
                      "chlorides", "free sulfur dioxide", "total sulfur dioxide",
                      "density", "pH", "sulphates", "alcohol")

# Remove outliers for each selected attribute
for (col_name in selected_columns) {
  print(col_name)
  wine_data <- remove_outliers_IQR(wine_data, col_name)
}
# Clear null rows in the dataframe
cleaned_data <- na.omit(wine_data)

# Customizing the boxplot
boxplot(cleaned_data,
        main = "Boxplot of Wine Attributes",
        ylab = "Attribute Values",
        xlab = "Attributes",
        col = "lightblue",   # Color of the boxplot
        border = "black",    # Border color
        notch = TRUE,        # Add notches
        notchwidth = 0.5,    # Width of notches
        horizontal = FALSE,  # Vertical boxplot
        outline = TRUE,      # Include outliers
        names = selected_columns,  # Attribute names as x-axis labels
        las = 2,             # Rotate x-axis labels vertically
        cex.axis = 0.8       # Adjust size of axis labels
)
# Print summary statistics of cleaned data
summary(cleaned_data)

# Print the first few rows of cleaned data
head(cleaned_data)

# Check the dimensions to ensure outliers were removed
dim(cleaned_data)

# Performing the scaling of data
scaled_data <- scale(cleaned_data)
scaled_data

#ploting the boxplot after scaling
boxplot(scaled_data)

# Part b

# Method for clustering

# Determine the number of clusters using NbClust library
nbclust_result <- NbClust(scaled_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index = "all")
#number of clusters-k=2

# Print the result
print(nbclust_result)

# Determine the number of clusters using the Elbow method
wss <- numeric(10)
for (i in 1:10) {
  wss[i] <- sum(kmeans(scaled_data, centers = i)$withinss)
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

# Determining the number of clusters using Gap statistics
fviz_nbclust(scaled_data, kmeans, method = 'gap_stat')
#Clustering k is 3

# Determine the number of clusters using the Silhouette method
fviz_nbclust(scaled_data, kmeans, method = 'silhouette')
#Clustering k =2

#NbCluster method -2
#Elbow method -2 (K value taken based on elbow in plot)
#Gap statistics -3
#Silhouette method -2 

#Part C

#Assigning the K value based on the result above I got previously 
#Assigning 2 since NbCluster,Elbow and silhouette returned 2
k<-2

#K means clustering
kmeans_result <- kmeans(scaled_data,centers = k)

#printing kmeans result
print(kmeans_result)

#printing kmeans centers
print(kmeans_result$centers)

#printing clustered results
clustered_data <-cbind(scaled_data,Cluster=kmeans_result$cluster)
print(clustered_data)

#Calculating BSS
BSS <- sum((apply(kmeans_result$centers,1, function(center) sum((center - colMeans(scaled_data))^2))) * table(kmeans_result$cluster))
#Calculating TSS
TSS <- sum(apply(scaled_data, 2, function(feature) sum((feature - mean(feature))^2)))
#Calculating WSS
WSS <- kmeans_result$tot.withinss

#Calculate the ratio of BSS and TSS
Ratio_BSS_and_TSS <-BSS/TSS
print(paste("Ratio of BSS/TSS :", Ratio_BSS_and_TSS))

# Convert scaled_data to a data frame
cleaned_data_df <- as.data.frame(scaled_data)

# Adding cluster assignment to the original dataset
clustered_dataset <- cbind(cleaned_data_df, Cluster = factor(kmeans_result$cluster))

# Plotting the data points with color-coded clusters
ggplot(clustered_dataset, aes(x = cleaned_data_df[,1], y = cleaned_data_df[,2], color = Cluster)) +
  geom_point() +
  labs(title = "Data Points after K-means Clustering (2 Clusters)",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal()

#Part D

#loading the library cluster
library(cluster)

# Computing silhouette width for each observation
sil_width <- silhouette(kmeans_result$cluster,dist(scaled_data))
fviz_silhouette(sil_width)

# Add average silhouette width to plot
avg_sil_width <- mean(sil_width[, "sil_width"])
abline(h = avg_sil_width, lty = 2)

# Print average silhouette width score
print(paste("Average Silhouette Width Score:", avg_sil_width))



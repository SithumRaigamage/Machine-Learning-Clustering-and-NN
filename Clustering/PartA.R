# Loading the required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(NbClust)
library(factoextra)
library(clusterCrit)
library(cluster)

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

# Printing the data column-wise
for (i in 1:11) {
  print(wine_dataset[, i])
}

# Selecting only the attributes given above (attributes 1 to 11)
selected_columns <- c("fixed acidity", "volatile acidity", "citric acid", "residual sugar",
                      "chlorides", "free sulfur dioxide", "total sulfur dioxide",
                      "density", "pH", "sulphates", "alcohol")

# Remove outliers for each selected attribute
for (col_name in selected_columns) {
  wine_data <- remove_outliers_IQR(wine_data, col_name)
}
# Clear null rows in the dataframe
wine_data <- na.omit(wine_data)

# Customizing the boxplot
boxplot(wine_data,
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

# Performing the scaling of data
scaled_data <- scale(wine_data)
scaled_data

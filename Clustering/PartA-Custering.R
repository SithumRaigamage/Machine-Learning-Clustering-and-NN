#loading the required libraries
library(readxl)
library(dplyr)
library(ggplot2)

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

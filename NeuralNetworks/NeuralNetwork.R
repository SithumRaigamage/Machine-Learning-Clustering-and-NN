# Install and load the readxl package if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

# Load other required libraries
library(ggplot2)
library(reshape2)
library(gridExtra)

# Read the Excel file
exchange <- read_excel("~/GitHub/MachineLearningAssignment/NeuralNetworks/ExchangeUSD.xlsx")

# Check the structure of the dataframe
str(exchange)

# Check the summary of the dataframe
summary(exchange)

# Check the summary of specific variables
summary(exchange$Wdy)
summary(exchange$`USD/EUR`)
summary(exchange$`YYYY/MM/DD`)

# Function to normalize columns
normalize <- function(x) {
  if (is.numeric(x)) {
    return((x - min(x)) / (max(x) - min(x)))
  } else {
    return(x)  # Return unchanged if not numeric
  }
}

# Apply normalization to all columns
exchange_norm <- as.data.frame(lapply(exchange, normalize))

# Summary of the normalized dataframe
summary(exchange_norm)

summary(exchange_norm$Wdy)   # summary of a specific normalized variable - strength

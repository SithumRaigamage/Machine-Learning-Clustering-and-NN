#loading new libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

#Data Colection

# Read the Excel file
exchange <- read_excel("~/Git hub projects/MachineLearningAssignment/NeuralNetworks/ExchangeUSD.xlsx")

#check the structure of the data
str(exchange)

#checking the summary
summary(exchange)

# Extract the 3rd column (USD/EUR exchange rates)
exchange_rate <- exchange[, 3]

#exploring the data

#check the structure of the data
str(exchange_rate)

#checking the summary
summary(exchange_rate)
summary(exchange_rate$`USD/EUR`)

#Part b

# Define the maximum time delay (up to t-4)
max_delay <- 4

# Initialize a list to store input/output matrices for each time delay
io_matrices <- list()

# Loop through each time delay level
for (delay in 1:max_delay) {
  # Create time-delayed input/output matrix for the current delay level
  io_matrix <- bind_cols()
  
  # Append the current input/output matrix to the list
  io_matrices[[delay]] <- io_matrix
}

# Function to create time-delayed input/output matrix for a given delay level
create_io_matrix <- function(data, delay) {
  # Create lagged columns for each time delay
  lagged_columns <- lapply(1:delay, function(d) lag(data, d))
  
  # Combine lagged columns and original data to form the input/output matrix
  io_matrix <- bind_cols(lagged_columns, G_pred = data)
  
  # Remove rows with NA values
  io_matrix <- io_matrix[complete.cases(io_matrix), ]
  
  return(io_matrix)
}

# Loop through each time delay level and create input/output matrices
for (delay in 1:max_delay) {
  io_matrices[[delay]] <- create_io_matrix(exchange_rate, delay)
}


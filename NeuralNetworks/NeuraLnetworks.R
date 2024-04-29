# Loading necessary libraries
library(readxl)
library(neuralnet)
library(caret)

# Read the Excel file
exchange <- read_excel("~/Git hub projects/MachineLearningAssignment/NeuralNetworks/ExchangeUSD.xlsx")
exchange_rate <- exchange[[3]]  # Extract the 3rd column (USD/EUR exchange rate)

# Split the data into training and testing sets
train_data <- exchange_rate[1:400]
test_data <- exchange_rate[401:500]

# Define the maximum time delay
max_delay <- 4

# Initialize lists to store input/output matrices for different delays
input_matrices <- list()
output_matrices <- list()

# Generate input/output matrices for different delays
for (delay in 1:max_delay) {
  input <- data.frame(matrix(NA, nrow = length(train_data) - delay, ncol = delay))
  for (i in 1:delay) {
    input[, i] <- lag(train_data, i)[delay:(length(train_data) - 1)]
  }
  output <- train_data[(delay + 1):length(train_data)]
  
  input_matrices[[delay]] <- input
  output_matrices[[delay]] <- output
}

#Part C

# Define a function to normalize data
normalize <- function(data) {
  normalized_data <- scale(data)
  return(normalized_data)
}

# Normalize each input matrix for different delays
for (delay in 1:max_delay) {
  input_matrices[[delay]] <- normalize(input_matrices[[delay]])
}

# Define a function to denormalize data
denormalize <- function(normalized_data, original_data) {
  denormalized_data <- normalized_data * sd(original_data) + mean(original_data)
  return(denormalized_data)
}

# Denormalize each input matrix for different delays
denormalized_input_matrices <- list()
for (delay in 1:max_delay) {
  denormalized_input <- matrix(NA, nrow = nrow(input_matrices[[delay]]), ncol = ncol(input_matrices[[delay]]))
  for (col in 1:ncol(input_matrices[[delay]])) {
    denormalized_input[, col] <- denormalize(input_matrices[[delay]][, col], train_data)
  }
  denormalized_input_matrices[[delay]] <- denormalized_input
}

# Print denormalized input matrices for different delays
for (delay in 1:max_delay) {
  cat("Denormalized input matrix for delay =", delay, ":\n")
  print(denormalized_input_matrices[[delay]])
  cat("\n")
}

#Part D





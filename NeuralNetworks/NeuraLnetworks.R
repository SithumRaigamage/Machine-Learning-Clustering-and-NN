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


#accessing input/output matrices for delay = 1
input_matrices[[1]]
output_matrices[[1]]
#accessing input/output matrices for delay = 2
input_matrices[[2]]
output_matrices[[2]]
#accessing input/output matrices for delay = 3
input_matrices[[3]]
output_matrices[[3]]
#accessing input/output matrices for delay = 4
input_matrices[[4]]
output_matrices[[4]]

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

# Print normalized input/output matrices for delay = 1
cat("Normalized input matrix for delay = 1:\n")
print(input_matrices[[1]])
cat("\n")
cat("Output matrix for delay = 1:\n")
print(output_matrices[[1]])
cat("\n")

# Print normalized input/output matrices for delay = 2
cat("Normalized input matrix for delay = 2:\n")
print(input_matrices[[2]])
cat("\n")
cat("Output matrix for delay = 2:\n")
print(output_matrices[[2]])
cat("\n")

# Print normalized input/output matrices for delay = 3
cat("Normalized input matrix for delay = 3:\n")
print(input_matrices[[3]])
cat("\n")
cat("Output matrix for delay = 3:\n")
print(output_matrices[[3]])
cat("\n")

# Print normalized input/output matrices for delay = 4
cat("Normalized input matrix for delay = 4:\n")
print(input_matrices[[4]])
cat("\n")
cat("Output matrix for delay = 4:\n")
print(output_matrices[[4]])
cat("\n")

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




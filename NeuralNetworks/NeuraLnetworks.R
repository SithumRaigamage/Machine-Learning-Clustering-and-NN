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



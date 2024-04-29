# Loading necessary libraries
library(readxl)
library(neuralnet)
library(caret)
library(Metrics)

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
denormalized_input_matrices <- lapply(input_matrices, function(x) {
  apply(x, 2, function(col) denormalize(col, exchange_rate))
})


#Part D

# Define a function to create and train MLP models with activation functions
train_mlp_with_activation <- function(input_data, output_data, hidden_layers, activation_function, linear_output = TRUE) {
  # Combine input and output data
  combined_data <- cbind(input_data, output_data)
  
  # Create and train the MLP model with specified activation function
  mlp_model <- neuralnet(output_data ~ ., hidden = hidden_layers, 
                         linear.output = linear_output, act.fct = activation_function, data = combined_data)
  
  # Return the trained model
  return(mlp_model)
}

# Define a function to evaluate the performance of the MLP model
evaluate_mlp <- function(model, test_input, test_output) {
  # Predict the output using the trained model
  predicted_output <- predict(model, test_input)
  
  # Calculate evaluation metrics
  rmse <- sqrt(mean((predicted_output - test_output)^2))
  mae <- mean(abs(predicted_output - test_output))
  mape <- mean(abs((test_output - predicted_output)/test_output)) * 100
  smape <- 2 * mape / (100 + mape)
  
  # Print evaluation metrics
  cat("RMSE:", rmse, "\n")
  cat("MAE:", mae, "\n")
  cat("MAPE:", mape, "%\n")
  cat("sMAPE:", smape, "%\n")
}

# Define the input and output data for training and testing
train_input <- denormalized_input_matrices[[4]]  # Change the delay as needed
train_output <- output_matrices[[4]]  # Change the delay as needed

test_delay <- 4  # Change the delay as needed
test_input <- data.frame(matrix(NA, nrow = length(test_data) - test_delay, ncol = test_delay))
for (i in 1:test_delay) {
  test_input[, i] <- lag(test_data, i)[test_delay:(length(test_data) - 1)]
}
test_output <- test_data[(test_delay + 1):length(test_data)]

# Train and evaluate MLP models with different configurations including activation functions
mlp_model_3 <- train_mlp_with_activation(train_input, train_output, hidden_layers = 5, 
                                         activation_function = "logistic", linear_output = TRUE)
cat("MLP Model 3 (Linear Output, Logistic Activation):\n")
evaluate_mlp(mlp_model_3, test_input, test_output)
# Plot the MLP models with activation functions
plot(mlp_model_3) 

mlp_model_4 <- train_mlp_with_activation(train_input, train_output, hidden_layers = 12, 
                                         activation_function = "tanh", linear_output = FALSE)
cat("\nMLP Model 4 (Nonlinear Output, Hyperbolic Tangent Activation):\n")
evaluate_mlp(mlp_model_4, test_input, test_output)
# Plot the MLP models with activation functions
plot(mlp_model_4) 

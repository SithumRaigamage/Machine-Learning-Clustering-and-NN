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

# Creating input/output matrix for Train data

# Creating input/output matrix for time-delayed values up to t-1 level
time_lagged_data <- data.frame(G_previous1 = lag(train_data, 1),
                               G_current = train_data)
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]

# Creating input/output matrix for time-delayed values up to t-2 level
time_lagged_data_2 <- data.frame(G_previous2 = lag(train_data, 2),
                                 G_previous1 = lag(train_data, 1),
                                 G_current = train_data)
time_lagged_data_2 <- time_lagged_data_2[complete.cases(time_lagged_data_2),]

# Creating input/output matrix for time-delayed values up to t-3 level
time_lagged_data_3 <- data.frame(G_previous3 = lag(train_data, 3),
                                 G_previous2 = lag(train_data, 2),
                                 G_previous1 = lag(train_data, 1),
                                 G_current = train_data)
time_lagged_data_3 <- time_lagged_data_3[complete.cases(time_lagged_data_3),]

# Creating input/output matrix for time-delayed values up to t-4 level
time_lagged_data_4 <- data.frame(G_previous4 = lag(train_data, 4),
                                 G_previous3 = lag(train_data, 3),
                                 G_previous2 = lag(train_data, 2),
                                 G_previous1 = lag(train_data, 1),
                                 G_current = train_data)
time_lagged_data_4 <- time_lagged_data_4[complete.cases(time_lagged_data_4),]


# Creating input/output matrix for Test data

# Creating input/output matrix for time-delayed values up to t-1 level for test_data
test_time_lagged_data <- data.frame(G_previous1 = lag(test_data, 1),
                                    G_current = test_data)
test_time_lagged_data <- test_time_lagged_data[complete.cases(test_time_lagged_data),]

# Creating input/output matrix for time-delayed values up to t-2 level for test_data
test_time_lagged_data2 <- data.frame(G_previous2 = lag(test_data, 2),
                                     G_previous1 = lag(test_data, 1),
                                     G_current = test_data)
test_time_lagged_data2 <- test_time_lagged_data2[complete.cases(test_time_lagged_data2),]

# Creating input/output matrix for time-delayed values up to t-3 level for test_data
test_time_lagged_data3 <- data.frame(G_previous3 = lag(test_data, 3),
                                     G_previous2 = lag(test_data, 2),
                                     G_previous1 = lag(test_data, 1),
                                     G_current = test_data)
test_time_lagged_data3 <- test_time_lagged_data3[complete.cases(test_time_lagged_data3),]

# Creating input/output matrix for time-delayed values up to t-4 level for test_data
test_time_lagged_data4 <- data.frame(G_previous4 = lag(test_data, 4),
                                     G_previous3 = lag(test_data, 3),
                                     G_previous2 = lag(test_data, 2),
                                     G_previous1 = lag(test_data, 1),
                                     G_current = test_data)
test_time_lagged_data4 <- test_time_lagged_data4[complete.cases(test_time_lagged_data4),]

# Extracting the result for Input and Output for training dataset
# Extracting input and output data 
input_data1 <- time_lagged_data[, -ncol(time_lagged_data)]
output_data1 <- time_lagged_data[, ncol(time_lagged_data)]

# Extracting input and output data
input_data2 <- time_lagged_data_2[, -ncol(time_lagged_data_2)]
output_data2 <- time_lagged_data_2[, ncol(time_lagged_data_2)]

# Extracting input and output data
input_data3 <- time_lagged_data_3[, -ncol(time_lagged_data_3)]
output_data3 <- time_lagged_data_3[, ncol(time_lagged_data_3)]

# Extracting input and output data
input_data4 <- time_lagged_data_4[, -ncol(time_lagged_data_4)]
output_data4 <- time_lagged_data_4[, ncol(time_lagged_data_4)]

# Extracting Input and output for input and output for training data

# Extracting input and output data
test_input1 <- test_time_lagged_data[, -ncol(test_time_lagged_data)]
test_output1 <- test_time_lagged_data[, ncol(test_time_lagged_data)]

# Extracting input and output data
test_input2 <- test_time_lagged_data2[, -ncol(test_time_lagged_data2)]
test_output2 <- test_time_lagged_data2[, ncol(test_time_lagged_data2)]

# Extracting input and output data
test_input3 <- test_time_lagged_data3[, -ncol(test_time_lagged_data3)]
test_output3 <- test_time_lagged_data3[, ncol(test_time_lagged_data3)]

# Extracting input and output data
test_input4 <- test_time_lagged_data4[, -ncol(test_time_lagged_data4)]
test_output4 <- test_time_lagged_data4[, ncol(test_time_lagged_data4)]

# Function to normalize data
normalize <- function(data) {
  normalized_data <- scale(data)
  return(normalized_data)
}

# Normalize input and output data for train_data
normalized_input1 <- normalize(input_data1)
normalized_output1 <- normalize(output_data1)

normalized_input2 <- normalize(input_data2)
normalized_output2 <- normalize(output_data2)

normalized_input3 <- normalize(input_data3)
normalized_output3 <- normalize(output_data3)

normalized_input4 <- normalize(input_data4)
normalized_output4 <- normalize(output_data4)

# Normalize input and output data for test_data
normalized_test_input1 <- normalize(test_input1)
normalized_test_output1 <- normalize(test_output1)

normalized_test_input2 <- normalize(test_input2)
normalized_test_output2 <- normalize(test_output2)

normalized_test_input3 <- normalize(test_input3)
normalized_test_output3 <- normalize(test_output3)

normalized_test_input4 <- normalize(test_input4)
normalized_test_output4 <- normalize(test_output4)

# Function to create and train MLP model
train_mlp <- function(input_data, output_data, test_input, test_output) {
  # Define neural network architecture
  mlp_model <- neuralnet(output_data ~ ., 
                         data = input_data, 
                         hidden = c(5, 3),  # Example: 2 hidden layers with 5 and 3 nodes respectively
                         linear.output = FALSE, 
                         threshold = 0.01)  # Example: Threshold for stopping criteria
  
  # Make predictions on test data
  test_predictions <- predict(mlp_model, test_input)
  
  # Calculate evaluation metrics
  rmse <- sqrt(mean((test_predictions - test_output)^2))
  mae <- mean(abs(test_predictions - test_output))
  mape <- mean(abs((test_output - test_predictions)/test_output)) * 100
  smape <- mean(2 * abs(test_predictions - test_output) / (abs(test_predictions) + abs(test_output))) * 100
  
  # Return evaluation metrics
  return(list(RMSE = rmse, MAE = mae, MAPE = mape, sMAPE = smape))
}

# Train and evaluate MLP models for each case
evaluation_results1 <- train_mlp(normalized_input1, normalized_output1, normalized_test_input1, normalized_test_output1)
# Display evaluation results
print("Evaluation Results for Time-delayed values up to t-1 level:")
print(evaluation_results1)

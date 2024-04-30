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

# Function to normalize data
normalize <- function(data) {
  normalized_data <- scale(data)
  return(normalized_data)
}

# Normalize time_lagged_data for training
normalized_time_lagged_data <- lapply(list(time_lagged_data, time_lagged_data_2, time_lagged_data_3, time_lagged_data_4), normalize)

# Normalize test_time_lagged_data for testing
normalized_test_time_lagged_data <- lapply(list(test_time_lagged_data, test_time_lagged_data2, test_time_lagged_data3, test_time_lagged_data4), normalize)

#Part D


# Train MLP model for configuration 1
nn_model_1 <- neuralnet(G_current ~ G_previous1 ,
                        data = time_lagged_data,
                        hidden = c(5),  # Example hidden layers
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function
plot(nn_model_1)
# Make predictions on test data
predicted_output_1 <- predict(nn_model_1, test_time_lagged_data)

# Calculate evaluation metrics for configuration 1
evaluation_results_1 <- list(
  RMSE = sqrt(mean((predicted_output_1 - test_time_lagged_data$G_current)^2)),
  MAE = mean(abs(predicted_output_1 - test_time_lagged_data$G_current)),
  MAPE = mean(abs((test_time_lagged_data$G_current - predicted_output_1) / test_time_lagged_data$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data$G_current - predicted_output_1) / (abs(test_time_lagged_data$G_current) + abs(predicted_output_1)))) * 100
)
evaluation_results_1

# Train MLP model for configuration 2
nn_model_2 <- neuralnet(G_current ~ G_previous1 ,
                        data = time_lagged_data,
                        hidden = c(10),  # Example hidden layers for model 2
                        linear.output = TRUE,  # Example linear output
                        act.fct = "tanh")  # Example activation function for model 2
plot(nn_model_2)
# Make predictions on test data
predicted_output_2 <- predict(nn_model_2, test_time_lagged_data)

# Calculate evaluation metrics for configuration 2
evaluation_results_2 <- list(
  RMSE = sqrt(mean((predicted_output_2 - test_time_lagged_data$G_current)^2)),
  MAE = mean(abs(predicted_output_2 - test_time_lagged_data$G_current)),
  MAPE = mean(abs((test_time_lagged_data$G_current - predicted_output_2) / test_time_lagged_data$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data$G_current - predicted_output_2) / (abs(test_time_lagged_data$G_current) + abs(predicted_output_2)))) * 100
)
evaluation_results_2

# Train MLP model for configuration 3
nn_model_3 <- neuralnet(G_current ~ G_previous1 ,
                        data = time_lagged_data,
                        hidden = c(5, 5),  # Example hidden layers for model 3
                        linear.output = FALSE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 3
plot(nn_model_3)
# Make predictions on test data
predicted_output_3 <- predict(nn_model_3, test_time_lagged_data)

# Calculate evaluation metrics for configuration 3
evaluation_results_3 <- list(
  RMSE = sqrt(mean((predicted_output_3 - test_time_lagged_data$G_current)^2)),
  MAE = mean(abs(predicted_output_3 - test_time_lagged_data$G_current)),
  MAPE = mean(abs((test_time_lagged_data$G_current - predicted_output_3) / test_time_lagged_data$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data$G_current - predicted_output_3) / (abs(test_time_lagged_data$G_current) + abs(predicted_output_3)))) * 100
)
evaluation_results_3

# Train MLP model for configuration 2
nn_model_4 <- neuralnet(G_current ~ G_previous1 + G_previous2,
                        data = time_lagged_data_2,
                        hidden = c(5, 5),  # Example hidden layers for model 4
                        linear.output = TRUE,  # Example linear output
                        act.fct = "threshold")  # Example activation function for model 4
plot(nn_model_4)
# Make predictions on test data
predicted_output_4 <- predict(nn_model_4, test_time_lagged_data2)

# Calculate evaluation metrics for configuration 3
evaluation_results_4 <- list(
  RMSE = sqrt(mean((predicted_output_3 - test_time_lagged_data2$G_current)^2)),
  MAE = mean(abs(predicted_output_3 - test_time_lagged_data2$G_current)),
  MAPE = mean(abs((test_time_lagged_data2$G_current - predicted_output_3) / test_time_lagged_data2$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data2$G_current - predicted_output_3) / (abs(test_time_lagged_data2$G_current) + abs(predicted_output_3)))) * 100
)
evaluation_results_4



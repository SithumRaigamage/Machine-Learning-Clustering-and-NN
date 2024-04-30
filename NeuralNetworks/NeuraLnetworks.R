# Loading necessary libraries
library(readxl)
library(neuralnet)
library(caret)
library(Metrics)
library(dplyr)

# Read the Excel file
exchange <- read_excel("~/Git hub projects/MachineLearningAssignment/NeuralNetworks/ExchangeUSD.xlsx")
exchange_rate <- exchange[[3]]  # Extract the 3rd column (USD/EUR exchange rate)

# Split the data into training and testing sets
train_data <- exchange_rate[1:400]
test_data <- exchange_rate[401:500]

# Creating input/output matrix for Train data

# Creating input/output matrix for time-delayed values up to t-1 level
time_lagged_data <- bind_cols(G_previous1 = lag(train_data, 1),
                               G_current = train_data)
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]

# Creating input/output matrix for time-delayed values up to t-2 level
time_lagged_data_2 <- bind_cols(G_previous2 = lag(train_data, 2),
                                 G_previous1 = lag(train_data, 1),
                                 G_current = train_data)
time_lagged_data_2 <- time_lagged_data_2[complete.cases(time_lagged_data_2),]

# Creating input/output matrix for time-delayed values up to t-3 level
time_lagged_data_3 <- bind_cols(G_previous3 = lag(train_data, 3),
                                 G_previous2 = lag(train_data, 2),
                                 G_previous1 = lag(train_data, 1),
                                 G_current = train_data)
time_lagged_data_3 <- time_lagged_data_3[complete.cases(time_lagged_data_3),]

# Creating input/output matrix for time-delayed values up to t-4 level
time_lagged_data_4 <- bind_cols(G_previous4 = lag(train_data, 4),
                                 G_previous3 = lag(train_data, 3),
                                 G_previous2 = lag(train_data, 2),
                                 G_previous1 = lag(train_data, 1),
                                 G_current = train_data)
time_lagged_data_4 <- time_lagged_data_4[complete.cases(time_lagged_data_4),]


# Creating input/output matrix for Test data

# Creating input/output matrix for time-delayed values up to t-1 level for test_data
test_time_lagged_data <- bind_cols(G_previous1 = lag(test_data, 1),
                                    G_current = test_data)
test_time_lagged_data <- test_time_lagged_data[complete.cases(test_time_lagged_data),]

# Creating input/output matrix for time-delayed values up to t-2 level for test_data
test_time_lagged_data2 <- bind_cols(G_previous2 = lag(test_data, 2),
                                     G_previous1 = lag(test_data, 1),
                                     G_current = test_data)
test_time_lagged_data2 <- test_time_lagged_data2[complete.cases(test_time_lagged_data2),]

# Creating input/output matrix for time-delayed values up to t-3 level for test_data
test_time_lagged_data3 <- bind_cols(G_previous3 = lag(test_data, 3),
                                     G_previous2 = lag(test_data, 2),
                                     G_previous1 = lag(test_data, 1),
                                     G_current = test_data)
test_time_lagged_data3 <- test_time_lagged_data3[complete.cases(test_time_lagged_data3),]

# Creating input/output matrix for time-delayed values up to t-4 level for test_data
test_time_lagged_data4 <- bind_cols(G_previous4 = lag(test_data, 4),
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
                        act.fct = "logistic")  # Example activation function for model 2
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

# Train MLP model for configuration 4
nn_model_4 <- neuralnet(G_current ~ G_previous1 + G_previous2,
                        data = time_lagged_data_2,
                        hidden = c(5, 5),  # Example hidden layers for model 4
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 4
plot(nn_model_4)
# Make predictions on test data
predicted_output_4 <- predict(nn_model_4, test_time_lagged_data2)

# Calculate evaluation metrics for configuration 4
evaluation_results_4 <- list(
  RMSE = sqrt(mean((predicted_output_4 - test_time_lagged_data2$G_current)^2)),
  MAE = mean(abs(predicted_output_4 - test_time_lagged_data2$G_current)),
  MAPE = mean(abs((test_time_lagged_data2$G_current - predicted_output_4) / test_time_lagged_data2$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data2$G_current - predicted_output_4) / (abs(test_time_lagged_data2$G_current) + abs(predicted_output_4)))) * 100
)
evaluation_results_4

# Train MLP model for configuration 5
nn_model_5 <- neuralnet(G_current ~ G_previous1 + G_previous2,
                        data = time_lagged_data_2,
                        hidden = c(5),  # Example hidden layers for model 5
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 5
plot(nn_model_5)
# Make predictions on test data
predicted_output_5 <- predict(nn_model_5, test_time_lagged_data2)

# Calculate evaluation metrics for configuration 5
evaluation_results_5 <- list(
  RMSE = sqrt(mean((predicted_output_5 - test_time_lagged_data2$G_current)^2)),
  MAE = mean(abs(predicted_output_5 - test_time_lagged_data2$G_current)),
  MAPE = mean(abs((test_time_lagged_data2$G_current - predicted_output_5) / test_time_lagged_data2$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data2$G_current - predicted_output_5) / (abs(test_time_lagged_data2$G_current) + abs(predicted_output_5)))) * 100
)
evaluation_results_5

# Train MLP model for configuration 6
nn_model_6 <- neuralnet(G_current ~ G_previous1 + G_previous2,
                        data = time_lagged_data_2,
                        hidden = c(10),  # Example hidden layers for model 6
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 6
plot(nn_model_6)
# Make predictions on test data
predicted_output_6 <- predict(nn_model_6, test_time_lagged_data2)

# Calculate evaluation metrics for configuration 6
evaluation_results_6 <- list(
  RMSE = sqrt(mean((predicted_output_6 - test_time_lagged_data2$G_current)^2)),
  MAE = mean(abs(predicted_output_6 - test_time_lagged_data2$G_current)),
  MAPE = mean(abs((test_time_lagged_data2$G_current - predicted_output_6) / test_time_lagged_data2$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data2$G_current - predicted_output_6) / (abs(test_time_lagged_data2$G_current) + abs(predicted_output_5)))) * 100
)
evaluation_results_6

# Train MLP model for configuration 7
nn_model_7 <- neuralnet(G_current ~ G_previous1 + G_previous2+G_previous3,
                        data = time_lagged_data_3,
                        hidden = c(10,10),  # Example hidden layers for model 7
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 7
plot(nn_model_7)
# Make predictions on test data
predicted_output_7 <- predict(nn_model_7, test_time_lagged_data3)

# Calculate evaluation metrics for configuration 7
evaluation_results_7 <- list(
  RMSE = sqrt(mean((predicted_output_7 - test_time_lagged_data3$G_current)^2)),
  MAE = mean(abs(predicted_output_7 - test_time_lagged_data3$G_current)),
  MAPE = mean(abs((test_time_lagged_data3$G_current - predicted_output_7) / test_time_lagged_data3$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data3$G_current - predicted_output_7) / (abs(test_time_lagged_data3$G_current) + abs(predicted_output_7)))) * 100
)
evaluation_results_7

# Train MLP model for configuration 8
nn_model_8 <- neuralnet(G_current ~ G_previous1 + G_previous2+G_previous3,
                        data = time_lagged_data_3,
                        hidden = c(10,10),  # Example hidden layers for model 8
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 8
plot(nn_model_8)
# Make predictions on test data
predicted_output_8 <- predict(nn_model_8, test_time_lagged_data3)

# Calculate evaluation metrics for configuration 8
evaluation_results_8 <- list(
  RMSE = sqrt(mean((predicted_output_8 - test_time_lagged_data3$G_current)^2)),
  MAE = mean(abs(predicted_output_8 - test_time_lagged_data3$G_current)),
  MAPE = mean(abs((test_time_lagged_data3$G_current - predicted_output_8) / test_time_lagged_data3$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data3$G_current - predicted_output_8) / (abs(test_time_lagged_data3$G_current) + abs(predicted_output_8)))) * 100
)
evaluation_results_8

# Train MLP model for configuration 9
nn_model_9 <- neuralnet(G_current ~ G_previous1 + G_previous2+G_previous3,
                        data = time_lagged_data_3,
                        hidden = c(10),  # Example hidden layers for model 9
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 9
plot(nn_model_9)
# Make predictions on test data
predicted_output_9 <- predict(nn_model_9, test_time_lagged_data3)

# Calculate evaluation metrics for configuration 9
evaluation_results_9 <- list(
  RMSE = sqrt(mean((predicted_output_9 - test_time_lagged_data3$G_current)^2)),
  MAE = mean(abs(predicted_output_9 - test_time_lagged_data3$G_current)),
  MAPE = mean(abs((test_time_lagged_data3$G_current - predicted_output_9) / test_time_lagged_data3$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data3$G_current - predicted_output_9) / (abs(test_time_lagged_data3$G_current) + abs(predicted_output_9)))) * 100
)
evaluation_results_9

# Train MLP model for configuration 10
nn_model_10 <- neuralnet(G_current ~ G_previous1 + G_previous2 + G_previous3 + G_previous4,
                        data = time_lagged_data_4,
                        hidden = c(10),  # Example hidden layers for model 10
                        linear.output = TRUE,  # Example linear output
                        act.fct = "logistic")  # Example activation function for model 10
plot(nn_model_10)
# Make predictions on test data
predicted_output_10 <- predict(nn_model_10, test_time_lagged_data4)

# Calculate evaluation metrics for configuration 5
evaluation_results_10 <- list(
  RMSE = sqrt(mean((predicted_output_10 - test_time_lagged_data4$G_current)^2)),
  MAE = mean(abs(predicted_output_10 - test_time_lagged_data4$G_current)),
  MAPE = mean(abs((test_time_lagged_data4$G_current - predicted_output_10) / test_time_lagged_data4$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data4$G_current - predicted_output_10) / (abs(test_time_lagged_data4$G_current) + abs(predicted_output_10)))) * 100
)
evaluation_results_10

# Train MLP model for configuration 11
nn_model_11 <- neuralnet(G_current ~ G_previous1 + G_previous2 + G_previous3 + G_previous4,
                         data = time_lagged_data_4,
                         hidden = c(10),  # Example hidden layers for model 10
                         linear.output = TRUE,  # Example linear output
                         act.fct = "logistic")  # Example activation function for model 10
plot(nn_model_11)
# Make predictions on test data
predicted_output_11 <- predict(nn_model_11, test_time_lagged_data4)

# Calculate evaluation metrics for configuration 5
evaluation_results_11 <- list(
  RMSE = sqrt(mean((predicted_output_11 - test_time_lagged_data4$G_current)^2)),
  MAE = mean(abs(predicted_output_11 - test_time_lagged_data4$G_current)),
  MAPE = mean(abs((test_time_lagged_data4$G_current - predicted_output_11) / test_time_lagged_data4$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data4$G_current - predicted_output_11) / (abs(test_time_lagged_data4$G_current) + abs(predicted_output_11)))) * 100
)
evaluation_results_11

# Train MLP model for configuration 12
nn_model_12 <- neuralnet(G_current ~ G_previous1 + G_previous2 + G_previous3 + G_previous4,
                         data = time_lagged_data_4,
                         hidden = c(6,3),  # Example hidden layers for model 10
                         linear.output = TRUE,  # Example linear output
                         act.fct = "logistic")  # Example activation function for model 10
plot(nn_model_12)
# Make predictions on test data
predicted_output_12 <- predict(nn_model_12, test_time_lagged_data4)

# Calculate evaluation metrics for configuration 5
evaluation_results_12 <- list(
  RMSE = sqrt(mean((predicted_output_12 - test_time_lagged_data4$G_current)^2)),
  MAE = mean(abs(predicted_output_12 - test_time_lagged_data4$G_current)),
  MAPE = mean(abs((test_time_lagged_data4$G_current - predicted_output_12) / test_time_lagged_data4$G_current)) * 100,
  sMAPE = 2 * mean(abs((test_time_lagged_data4$G_current - predicted_output_12) / (abs(test_time_lagged_data4$G_current) + abs(predicted_output_12)))) * 100
)
evaluation_results_12


#Part H

# Compile RMSE values for each configuration
rmse_values <- c(evaluation_results_1$RMSE, evaluation_results_2$RMSE, evaluation_results_3$RMSE,
                 evaluation_results_4$RMSE, evaluation_results_5$RMSE, evaluation_results_6$RMSE,
                 evaluation_results_7$RMSE, evaluation_results_8$RMSE, evaluation_results_9$RMSE,
                 evaluation_results_10$RMSE, evaluation_results_11$RMSE, evaluation_results_12$RMSE)

evaluation_results_1$RMSE
evaluation_results_2$RMSE
evaluation_results_3$RMSE
evaluation_results_4$RMSE
evaluation_results_5$RMSE
evaluation_results_6$RMSE
evaluation_results_7$RMSE
evaluation_results_8$RMSE
evaluation_results_9$RMSE
evaluation_results_10$RMSE
evaluation_results_11$RMSE
evaluation_results_12$RMSE

# Find the configuration with the lowest RMSE
best_configuration <- which.min(rmse_values)

# Print the best configuration and its RMSE value
cat("Best configuration:", best_configuration, "\n")
cat("RMSE:", rmse_values[best_configuration], "\n")

# Plotting predicted vs. actual values for "Config 4"
plot(test_time_lagged_data$G_current, predicted_output_1,
     xlab = "Actual Exchange Rate",
     ylab = "Predicted Exchange Rate",
     main = "Actual vs. Predicted Exchange Rate (Config 1)",
     col = "blue")
abline(0, 1, col = "red")  # Adding a reference line for perfect prediction
legend("topleft", legend = "Perfect Prediction", col = "red", lty = 1)


# Plotting a grid
plot(test_time_lagged_data$G_current, type = "l", col = "blue", lwd = 2,
     xlab = "Time", ylab = "Exchange Rate",
     main = "Actual Exchange Rate over Time")
lines(predicted_output_1, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Actual", "Predicted"),
       col = c("blue", "red"), lty = c(1, 2), lwd = c(2, 2))
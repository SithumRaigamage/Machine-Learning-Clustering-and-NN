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


#part b
# Creating input/output matrix for Train data

# Creating input/output matrix for time-delayed values up to t-1 level
time_lagged_data <- data.frame(G_previous1 = lag(train_data, 1),
                               G_current = train_data)
time_lagged_data <- time_lagged_data[complete.cases(time_lagged_data),]

# Creating input/output matrix for time-delayed values up to t-2 level
time_lagged_data_2 <- data.frame(G_previous2 = lag(train_data, 2),
                               G_previous1 = lag(train_data, 1),
                               G_current = train_data)
time_lagged_data_2 <- time_lagged_data_2[complete.cases(time_lagged_data),]

# Creating input/output matrix for time-delayed values up to t-3 level
time_lagged_data_3 <- data.frame(G_previous3 = lag(train_data, 3),
                               G_previous2 = lag(train_data, 2),
                               G_previous1 = lag(train_data, 1),
                               G_current = train_data)
time_lagged_data_3 <- time_lagged_data_3[complete.cases(time_lagged_data),]

# Creating input/output matrix for time-delayed values up to t-4 level
time_lagged_data_4 <- data.frame(G_previous4 = lag(train_data, 4),
                               G_previous3 = lag(train_data, 3),
                               G_previous2 = lag(train_data, 2),
                               G_previous1 = lag(train_data, 1),
                               G_current = train_data)
time_lagged_data_4 <- time_lagged_data_4[complete.cases(time_lagged_data),]


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

#Extracting the result for Input and Output for training dataset
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

#Extracting Input and output for input and output for training data

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



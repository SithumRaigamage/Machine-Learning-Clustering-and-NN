library(readxl)

# Read the Excel file
exchange <- read_excel("~/Git hub projects/MachineLearningAssignment/NeuralNetworks/ExchangeUSD.xlsx")

# Extract the 3rd column (USD/EUR exchange rates)
exchange_rate <- exchange[, 3]


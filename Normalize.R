# Reading the Dataset
data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

# Subset of data (you can adjust the subset size as needed)
subset_data <- data[1:1000, ]  # Adjust the subset size as needed

# Convert 'Date' column to date format
scaled_data$Date <- as.Date(scaled_data$Date, format = "%d-%m-%Y")

# Fitting a Linear Regression Model with Scaled Features
model_scaled <- lm(Conversion_Rate ~ ., data = scaled_data)

# Summary of the Model with Scaled Features
summary(model_scaled)




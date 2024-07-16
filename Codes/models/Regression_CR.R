# Load required libraries
library(caret)
library(ggplot2)

# Load the data
marketing_data <- read.csv("final_modified_data.csv", stringsAsFactors = FALSE)

# Remove 'Date' column
marketing_data <- subset(marketing_data, select = -c(Date, Campaign_ID, Company, Clicks, Engagement_Score))

# Take the first 1000 records
subset_data <- head(marketing_data, 1000)

# Set seed for reproducibility
set.seed(123)

# Split data into train and test sets
trainIndex <- createDataPartition(subset_data$Conversion_Rate, p = 0.8, list = FALSE)
train_data <- subset_data[trainIndex, ]
test_data <- subset_data[-trainIndex, ]

# Train regression model
conversion_model <- train(Conversion_Rate ~ ., data = train_data, method = "lm")

# Predict on test set
predictions <- predict(conversion_model, newdata = test_data)

# Plot actual vs predicted using ggplot2
ggplot(data = data.frame(Actual = test_data$Conversion_Rate, Predicted = predictions), aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5) +
  labs(title = "Actual vs Predicted Conversion Rate",
       x = "Actual Conversion Rate", y = "Predicted Conversion Rate") +
  theme_minimal()

# Calculate and print the metrics
rmse <- sqrt(mean((predictions - test_data$Conversion_Rate)^2))
mse <- mean((predictions - test_data$Conversion_Rate)^2)
rsquared <- cor(predictions, test_data$Conversion_Rate)^2

# Print the metrics
cat("RMSE:", rmse, "\n")
cat("MSE:", mse, "\n")
cat("R-squared:", rsquared, "\n")

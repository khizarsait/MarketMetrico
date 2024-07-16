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
trainIndex <- createDataPartition(subset_data$ROI, p = 0.8, list = FALSE)
train_data <- subset_data[trainIndex, ]
test_data <- subset_data[-trainIndex, ]

# Train regression model
conversion_model <- train(ROI ~ ., data = train_data, method = "lm")

# Predict on test set
predictions <- predict(conversion_model, newdata = test_data)

# Create a data frame for actual vs. predicted values
results <- data.frame(Actual = test_data$ROI, Predicted = predictions)

# Plot actual vs. predicted graph
ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5) +
  labs(title = "Actual vs. Predicted ROI",
       x = "Actual ROI", y = "Predicted ROI") +
  theme_minimal()


# Calculate R-squared
rsquared <- cor(results$Actual, results$Predicted)^2

# Calculate MSE
mse <- mean((results$Actual - results$Predicted)^2)

# Calculate RMSE
rmse <- sqrt(mse)

# Print the metrics
cat("R-squared:", rsquared, "\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")


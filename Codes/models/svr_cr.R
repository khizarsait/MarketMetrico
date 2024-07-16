# Load required libraries
library(caret)
library(ggplot2)

# Load the data
data <- read.csv("final_modified_data.csv", nrows = 1000)

# Select relevant numerical columns
relevant_columns <- c("Acquisition_Cost", "Conversion_Rate", "Clicks", "Impressions", "Engagement_Score", "Duration_In_Days")

# Subset the data with relevant columns
data_subset <- data[, relevant_columns]

# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets (80% training, 20% test)
train_index <- sample(1:nrow(data_subset), 0.8 * nrow(data_subset))
train_data <- data_subset[train_index, ]
test_data <- data_subset[-train_index, ]

# Split predictors and target variable for training and test sets
X_train <- train_data[, -2]  # All columns except the second (Conversion_Rate)
Y_train <- train_data$Conversion_Rate
X_test <- test_data[, -2]
Y_test <- test_data$Conversion_Rate

# Scale the predictors for training and test sets
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test)

# Train SVR model on training data
svr_model <- svm(Y_train ~ ., data = as.data.frame(X_train_scaled), kernel = "radial")

# Predict Conversion Rate for test data
predicted_Conversion_Rate <- predict(svr_model, X_test_scaled)

# Calculate RMSE for test data
rmse_test <- sqrt(mean((Y_test - predicted_Conversion_Rate)^2))

# Calculate R-squared for test data
rsquared_test <- cor(Y_test, predicted_Conversion_Rate)^2

# Calculate MSE for test data
mse_test <- mean((Y_test - predicted_Conversion_Rate)^2)

# Print the performance metrics for the test data
cat("Test Data Metrics:\n")
cat("RMSE:", rmse_test, "\n")
cat("R-squared:", rsquared_test, "\n")
cat("MSE:", mse_test, "\n")

# Create a data frame with actual and predicted Conversion Rate values
plot_data <- data.frame(Actual_Conversion_Rate = Y_test, Predicted_Conversion_Rate = predicted_Conversion_Rate)

# Plot predicted vs. actual graph using ggplot
ggplot(plot_data, aes(x = Actual_Conversion_Rate, y = Predicted_Conversion_Rate)) +
  geom_point(color = "blue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5) +
  labs(title = "Predicted vs. Actual Conversion Rate (Test Data)",
       x = "Actual Conversion Rate", y = "Predicted Conversion Rate") +
  theme_minimal()

summary(svr_model)

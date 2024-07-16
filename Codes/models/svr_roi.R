# Load the nnet package
library(e1071)
library(ggplot2)

# Load your dataset
data <- read.csv("final_modified_data.csv",nrows=1000)

# Select relevant numerical columns
relevant_columns <- c("Acquisition_Cost", "ROI", "Clicks", "Impressions", "Engagement_Score", "Duration_In_Days")

# Subset the data with relevant columns
data_subset <- data[, relevant_columns]

# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets (80% training, 20% test)
train_index <- sample(1:nrow(data_subset), 0.8 * nrow(data_subset))
train_data <- data_subset[train_index, ]
test_data <- data_subset[-train_index, ]

# Split predictors and target variable for training and test sets
X_train <- train_data[, -1]  # All columns except the first (ROI)
Y_train <- train_data$ROI
X_test <- test_data[, -1]
Y_test <- test_data$ROI

# Scale the predictors for training and test sets
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test)

# Train SVR model on training data
svr_model <- svm(Y_train ~ ., data = as.data.frame(X_train_scaled), kernel = "radial")

# Predict ROI for test data
predicted_ROI <- predict(svr_model, X_test_scaled)

# Calculate RMSE for test data
rmse_test <- sqrt(mean((Y_test - predicted_ROI)^2))

# Calculate R-squared for test data
rsquared_test <- cor(Y_test, predicted_ROI)^2

# Calculate MSE for test data
mse_test <- mean((Y_test - predicted_ROI)^2)

# Print the performance metrics for the test data
cat("Test Data Metrics:\n")
cat("RMSE:", rmse_test, "\n")
cat("R-squared:", rsquared_test, "\n")
cat("MSE:", mse_test, "\n")

# Create a data frame with actual and predicted ROI values
plot_data <- data.frame(Actual_ROI = Y_test, Predicted_ROI = predicted_ROI)

# Plot predicted vs. actual graph using ggplot
ggplot(plot_data, aes(x = Actual_ROI, y = Predicted_ROI)) +
  geom_point(color = "blue", alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5) +
  labs(title = "Predicted vs. Actual ROI (Test Data)",
       x = "Actual ROI", y = "Predicted ROI") +
  theme_minimal()




#For total rows

# Load the nnet package
library(e1071)
library(ggplot2)

# Load your dataset
data <- read.csv("C:/Users/vijay/Desktop/PMA/new code/final_data.csv")

# Select relevant numerical columns
relevant_columns <- c("Acquisition_Cost", "ROI", "Clicks", "Impressions", "Engagement_Score", "Duration_In_Days")

# Subset the data with relevant columns
data_subset <- data[, relevant_columns]

# Set seed for reproducibility
set.seed(123)

# Split data into training and test sets (80% training, 20% test)
train_index <- sample(1:nrow(data_subset), 0.8 * nrow(data_subset))
train_data <- data_subset[train_index, ]
test_data <- data_subset[-train_index, ]

# Split predictors and target variable for training and test sets
X_train <- train_data[, -1]  # All columns except the first (ROI)
Y_train <- train_data$ROI
X_test <- test_data[, -1]
Y_test <- test_data$ROI

# Scale the predictors for training and test sets
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(X_test)

# Train SVR model on training data
svr_model <- svm(Y_train ~ ., data = as.data.frame(X_train_scaled), kernel = "radial")

# Predict ROI for test data
predicted_ROI <- predict(svr_model, X_test_scaled)

# Calculate RMSE for test data
rmse_test <- sqrt(mean((Y_test - predicted_ROI)^2))

# Calculate R-squared for test data
rsquared_test <- cor(Y_test, predicted_ROI)^2

# Calculate MSE for test data
mse_test <- mean((Y_test - predicted_ROI)^2)

# Print the performance metrics for the test data
cat("Test Data Metrics:\n")
cat("RMSE:", rmse_test, "\n")
cat("R-squared:", rsquared_test, "\n")
cat("MSE:", mse_test, "\n")

# Create a data frame with actual and predicted ROI values
plot_data <- data.frame(Actual_ROI = Y_test, Predicted_ROI = predicted_ROI)

# Plot predicted vs. actual graph using ggplot
ggplot(plot_data, aes(x = Actual_ROI, y = Predicted_ROI)) +
  geom_point(color = "blue", alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1.5) +
  labs(title = "Predicted vs. Actual ROI (Test Data)",
       x = "Actual ROI", y = "Predicted ROI") +
  theme_minimal()

summary(svr_model)






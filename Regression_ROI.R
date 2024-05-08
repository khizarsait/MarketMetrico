# Load required libraries
library(caret)

# Load the data
marketing_data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

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

# Evaluate performance (e.g., RMSE, MSE, R-squared)
rmse <- sqrt(mean((predictions - test_data$ROI)^2))
mse <- mean((predictions - test_data$ROI)^2)
rsquared <- cor(predictions, test_data$ROI)^2

# Print the metrics
print(paste("RMSE:", rmse))
print(paste("MSE:", mse))
print(paste("R-squared:", rsquared))

# Define new data with relevant columns
new_data <- data.frame(
  Campaign_Type = "Email",
  Target_Audience_Age_Min = 0,
  Target_Audience_Age_Max = 0,
  Target_Audience_Gender='All',
  Channel_Used='Website',
  Conversion_Rate=0.00,
  Location='New York',
  Language='English',
  Impressions=0,
  Customer_Segment='Fashionistas',
  Acquisition_Cost = 0,
  Duration_In_Days = 30
)

# Predict using the model
prediction <- predict(conversion_model, newdata = new_data)

# Print the prediction
print(prediction)

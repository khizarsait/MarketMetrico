# Load required libraries
library(caret)
marketing_data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

# Remove 'Date' column
marketing_data <- subset(marketing_data, select = -c(Date,Campaign_ID,Company,Clicks,Engagement_Score))


# Set seed for reproducibility
set.seed(123)

# Split data into train and test sets
trainIndex <- createDataPartition(marketing_data$ROI, p = 0.8, list = FALSE)
train_data <- marketing_data[trainIndex, ]
test_data <- marketing_data[-trainIndex, ]

# Take a random subset of the training dataset
train_subset_indices <- sample(1:nrow(train_data), size = 1000, replace = FALSE)
train_subset_data <- train_data[train_subset_indices, ]

# Train regression model
conversion_model <- train(ROI ~ ., data = train_subset_data, method = "lm")

# Predict on test set
predictions <- predict(conversion_model, newdata = test_data)

# Evaluate performance (e.g., RMSE)
RMSE(predictions, test_data$Conversion_Rate)
print(predictions[1:10])

# Define new data with relevant columns
new_data <- data.frame(
  Campaign_Type = "Email",
  Target_Audience_Age_Min = 18,
  Target_Audience_Age_Max = 24,
  Target_Audience_Gender='All',
  Channel_Used='Website',
  Conversion_Rate=0.05,
  Location='New York',
  Language='English',
  Impressions=1000,
  Customer_Segment='Fashionistas',
  Acquisition_Cost = 16174,
  Duration_In_Days = 30
)

# Predict using the model
prediction <- predict(conversion_model, newdata = new_data)

# Print the prediction
print(prediction)

# Reading the Dataset
data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

# Define a threshold for classifying campaign success based on conversion rate
threshold <- 0.05  # Adjust the threshold as needed

# Create a binary label indicating campaign success
data$Campaign_Success <- ifelse(data$Conversion_Rate >= threshold, 1, 0)

# Convert categorical variables to factors
data$Campaign_Type <- as.factor(data$Campaign_Type)
data$Target_Audience_Gender <- as.factor(data$Target_Audience_Gender)
data$Channel_Used <- as.factor(data$Channel_Used)
data$Location <- as.factor(data$Location)
data$Language <- as.factor(data$Language)
data$Customer_Segment <- as.factor(data$Customer_Segment)

# Define the formula for logistic regression
formula <- as.formula("Campaign_Success ~ Campaign_Type + Target_Audience_Gender + Target_Audience_Age_Min + Target_Audience_Age_Max + Channel_Used + Location + Language + Clicks + Impressions + Customer_Segment + Duration_In_Days")

# Fit the logistic regression model
model <- glm(formula, data = data, family = binomial)

# Summary of the model
summary(model)

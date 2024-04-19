
# Load required libraries
library(ggplot2)

# Read the data
df <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

# Boxplot for Conversion Rate by Campaign Type
ggplot(df, aes(x = Campaign_Type, y = Conversion_Rate)) + 
  geom_boxplot() +
  labs(x = "Campaign Type", y = "Conversion Rate") +
  ggtitle("Conversion Rate by Campaign Type")

# Barplot of Customer Segments
ggplot(df, aes(x = Customer_Segment)) + 
  geom_bar(fill = "red") +
  stat_count(geom = "text", aes(label = ..count..), vjust = -0.5, size = 3, color = "black") +
  labs(x = "Customer Segment", y = "Count") +
  ggtitle("Barplot for Customer Segments")



# Boxplot of Engagement Score by Customer Segment
ggplot(df, aes(x = Customer_Segment, y = Engagement_Score)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Customer Segment", y = "Engagement Score") +
  ggtitle("Boxplot of Engagement Score by Customer Segment") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Boxplot for Conversion Rate by Campaign Type
ggplot(df, aes(x = Campaign_Type, y = Conversion_Rate)) + 
  geom_boxplot(fill = "lightblue") +
  labs(x = "Campaign Type", y = "Conversion Rate") +
  ggtitle("Boxplot of Conversion Rate by Campaign Type")



# Sample a subset of rows
sampled_data <- df[sample(nrow(df), 5000), ]  # Adjust the number of rows as needed

# Create scatterplot for Acquisition Cost vs Impressions
ggplot(sampled_data, aes(x = Acquisition_Cost, y = Impressions)) +
  geom_point() +
  labs(x = "Acquisition Cost", y = "Impressions") +
  ggtitle("Scatterplot of Acquisition Cost vs Impressions")

# Create scatterplot for Acquisition Cost vs Clicks
ggplot(sampled_data, aes(x = Acquisition_Cost, y = Clicks)) +
  geom_point() +
  labs(x = "Acquisition Cost", y = "Clicks") +
  ggtitle("Scatterplot of Acquisition Cost vs Clicks")
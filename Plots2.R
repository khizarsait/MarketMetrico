# Load necessary libraries
library(ggplot2)
library(dplyr)

# Read the dataset
marketing_data <- read.csv("C:/Users/vijay/Desktop/PMA/final_modified_data.csv", stringsAsFactors = FALSE)

# Convert 'Date' column to Date format
marketing_data$Date <- as.Date(marketing_data$Date, format = "%d-%m-%Y")

# Subset the data by month (January 2021)
january_data <- subset(marketing_data, format(Date, "%m-%Y") == "01-2021")

# Take a random subset of 1000 unique rows
january_data_subset <- january_data %>% distinct() %>% sample_n(1000)

# Density plot for sensible numerical columns
numerical_columns <- c("Impressions", "Clicks", "CPC", "CTR", "CPA", "CPI", "ROI", "Acquisition_Cost")
for (column in numerical_columns) {
  ggplot(january_data_subset, aes(x = !!sym(column))) +
    geom_density(fill = "blue", color = "black") +
    labs(x = column, y = "Density", title = paste("Density Plot of", column, "for January 2021"))
}

# Scatter plot for ROI vs. Acquisition Cost with hexbin plot
ggplot(january_data_subset, aes(x = Acquisition_Cost, y = ROI)) +
  geom_hex() +
  labs(x = "Acquisition Cost", y = "ROI", title = "Hexbin Plot of ROI vs. Acquisition Cost for January 2021")

# Scatter plot for ROI vs. Acquisition Cost with subsampling
sampled_data <- january_data_subset[sample(nrow(january_data_subset), 1000), ]
ggplot(sampled_data, aes(x = Acquisition_Cost, y = ROI)) +
  geom_point(color = "blue", alpha = 0.5) +  # Adjust alpha to control transparency
  labs(x = "Acquisition Cost", y = "ROI", title = "Scatter Plot of ROI vs. Acquisition Cost for January 2021")

# Scatter plot for ROI vs. Acquisition Cost with adjusted axis limits
ggplot(january_data_subset, aes(x = Acquisition_Cost, y = ROI)) +
  geom_point(color = "blue", alpha = 0.5) +  # Adjust alpha to control transparency
  labs(x = "Acquisition Cost", y = "ROI", title = "Scatter Plot of ROI vs. Acquisition Cost for January 2021") +
  xlim(0, 5000) +
  ylim(0, 1)  # Adjust the y-axis limit based on the range of ROI values



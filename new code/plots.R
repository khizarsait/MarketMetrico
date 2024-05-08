# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(heatmaply)

# Reading the Dataset
data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

# Filter numeric columns
numeric_data <- data %>%
  select_if(is.numeric)

# Compute correlation matrix
correlation_matrix <- cor(numeric_data)

# Plot heatmap for correlation matrix
heatmap_correlation <- heatmaply(correlation_matrix, 
                                 layout_matrix = "tight", 
                                 width = 800, height = 600,
                                 main = "Correlation Heatmap of Numeric Attributes")

# Display the heatmap
heatmap_correlation

# Density plots for Gender, Location, and Language
density_data <- data %>%
  select(Target_Audience_Gender) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Density Plots for Gender, Location, and Language") +
  theme_minimal()

# Display the density plots
density_data






pie_chart_data <- data %>%
  count(Customer_Segment) %>%
  ggplot(aes(x = "", y = n, fill = Customer_Segment)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("Count: ", n)), position = position_stack(vjust = 0.5)) +  # Add count as text labels
  coord_polar("y", start = 0) +
  labs(title = "Pie Chart for Customer Segment") +
  theme_minimal()

# Display the pie chart
pie_chart_data
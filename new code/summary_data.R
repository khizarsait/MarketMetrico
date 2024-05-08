library(dplyr)
library(readr)
library(DataExplorer)
library(DT)
library(tidyr)

# Reading the Dataset
data <- read.csv("C:/Users/vijay/Documents/final_data.csv", stringsAsFactors = FALSE)

# Displaying the first 10 rows of the dataset using DT::datatable
DT::datatable(head(data, 10), rownames = FALSE, options = list(pageLength = 10))

# Selecting required columns
required_cols <- c("Target_Audience_Age_Min", "Target_Audience_Age_Max", "Conversion_Rate", 
                   "Acquisition_Cost", "ROI", "Clicks", "Impressions", "Engagement_Score", "Duration_In_Days")

# Check if all required columns exist in the dataset
missing_cols <- required_cols[!(required_cols %in% names(data))]
if (length(missing_cols) > 0) {
  stop(paste("The following columns are missing in the dataset:", paste(missing_cols, collapse = ", ")))
}

# Subset the data with required columns
data_required <- data[, required_cols]

# Convert numeric columns to numeric type
numeric_cols <- c("Target_Audience_Age_Min", "Target_Audience_Age_Max")
data_required[numeric_cols] <- lapply(data_required[numeric_cols], as.numeric)

# Check if there are non-numeric values in numeric columns
non_numeric_values <- sapply(data_required[numeric_cols], function(x) any(!is.na(x) & !is.numeric(x)))
if (any(non_numeric_values)) {
  print("Non-numeric values found in numeric columns. Please handle them.")
}

# Summary of the data
summary_data <- summary(data_required)
print(summary_data)

# Export summary to CSV file

write.csv(summary_data, "summary_final_data.csv", row.names = TRUE)

# Get the current working directory
getwd()

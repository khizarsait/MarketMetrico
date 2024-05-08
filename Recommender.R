# Reading the Dataset
data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

# Selecting the input features
input_features <- c("Campaign_Type", "Target_Audience_Gender", "Target_Audience_Age_Min", 
                    "Target_Audience_Age_Max", "Channel_Used", "Location", "Language", 
                    "Clicks", "Impressions", "Customer_Segment", "Duration_In_Days")

# Subsetting the dataset with selected features
data_subset <- data[input_features]

# Convert categorical variables to factors
data_subset <- lapply(data_subset, as.factor)

# Function to normalize data
normalize_data <- function(data) {
  normalized_data <- scale(data)
  return(normalized_data)
}

# Identify numeric indices
numeric_indices <- sapply(data_subset, is.numeric)

# Normalize numeric data
numeric_data <- data_subset[, numeric_indices]
numeric_data <- as.data.frame(sapply(numeric_data, as.numeric))  # Ensure numeric conversion
data_subset[, numeric_indices] <- normalize_data(numeric_data)

# Handling missing values by imputing with mean for numeric data
numeric_means <- colMeans(numeric_data, na.rm = TRUE)
numeric_data[is.na(numeric_data)] <- numeric_means[col(numeric_data)]
data_subset[, numeric_indices] <- numeric_data

# Performing similarity calculation (e.g., cosine similarity)
similarity_matrix <- crossprod(as.matrix(data_subset)) / (sqrt(rowSums(data_subset^2)) %*% t(sqrt(rowSums(data_subset^2))))

# Function to get top N similar campaigns for each campaign
get_top_similar_campaigns <- function(similarity_matrix, campaign_names, N = 5) {
  top_similar_campaigns <- list()
  for (i in 1:length(campaign_names)) {
    similar_campaigns <- sort(similarity_matrix[i,], decreasing = TRUE)
    top_similar_campaigns[[campaign_names[i]]] <- names(similar_campaigns[1:N])
  }
  return(top_similar_campaigns)
}

# Get top 5 similar campaigns for each campaign
top_similar_campaigns <- get_top_similar_campaigns(similarity_matrix, rownames(data_subset), N = 5)

# View recommended campaigns
print(top_similar_campaigns)

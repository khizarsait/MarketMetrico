# Reading the Dataset
data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)

# Selecting the input features
input_features <- c("Campaign_Type", "Target_Audience_Gender", "Target_Audience_Age_Min", 
                    "Target_Audience_Age_Max", "Channel_Used", "Location", "Language", 
                    "Clicks", "Impressions", "Customer_Segment", "Duration_In_Days")

# Subsetting the dataset with selected features
data_subset <- data[input_features]

# Convert all variables to numeric
data_subset <- apply(data_subset, 2, as.numeric)

# Handling missing values by imputing with mean
data_subset[is.na(data_subset)] <- mean(data_subset, na.rm = TRUE)

# Sampling a subset of the data (adjust the size as needed)
set.seed(123)  # For reproducibility
sampled_indices <- sample(nrow(data_subset), 1000)  # Sample 1000 rows
data_subset <- data_subset[sampled_indices, ]

# Performing k-means clustering
k <- 5  # Number of clusters (you can adjust this as needed)
set.seed(123)  # For reproducibility
kmeans_model <- kmeans(data_subset, centers = k)

# Plotting the clusters
plot(data_subset[, c("Clicks", "Impressions")], col = kmeans_model$cluster, 
     main = "K-means Clustering of Marketing Data (Subset)",
     xlab = "Clicks", ylab = "Impressions")
points(kmeans_model$centers[, c("Clicks", "Impressions")], col = 1:k, pch = 19, cex = 2, bg = "white")
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 19, cex = 1, title = "Cluster")

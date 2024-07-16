# Load required libraries
library(cluster)

# Reading the Dataset
data <- read.csv("final_modified_data.csv", stringsAsFactors = FALSE)

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
set.seed(123)  
sampled_indices <- sample(nrow(data_subset), 1000) 
data_subset <- data_subset[sampled_indices, ]

# Performing k-means clustering
k <- 5 
set.seed(123)
kmeans_model <- kmeans(data_subset, centers = k)

# Plotting the clusters
plot(data_subset[, c("Clicks", "Impressions")], col = kmeans_model$cluster, 
     main = "K-means Clustering of Marketing Data",
     xlab = "Clicks", ylab = "Impressions")
points(kmeans_model$centers[, c("Clicks", "Impressions")], col = 1:k, pch = 19, cex = 2, bg = "white")
legend("topright", legend = paste("Zone", 1:k), col = 1:k, pch = 19, cex = 1, title = "Aud_Eng")

## Calculate silhouette score
silhouette_score <- silhouette(kmeans_model$cluster, dist(data_subset))
cat("Silhouette Score:", mean(silhouette_score[, "sil_width"]), "\n")


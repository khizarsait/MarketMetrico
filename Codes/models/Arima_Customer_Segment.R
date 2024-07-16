# Load necessary libraries
library(dplyr)
library(forecast)
library(ggplot2)


# Read the dataset
marketing_data <- read.csv("final_modified_data.csv", stringsAsFactors = FALSE)

# Convert 'Date' column to Date format
marketing_data$Date <- as.Date(marketing_data$Date, format = "%d-%m-%Y")

# Take a random subset of 10,000 records
#sampled_data <- head(marketing_data, 10000)

# Subset data for each unique segment
segments <- unique(marketing_data$Customer_Segment)

# Create a list to store plots for each segment
plots_list <- list()

# Create a list to store MAE for each segment
mae_list <- list()

# Perform ARIMA analysis and plot for each segment
for (segment in segments) {
  segment_data <- filter(marketing_data, Customer_Segment == segment)
  
  # Aggregate data to monthly level and calculate average ROI
  monthly_avg_roi <- segment_data %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    summarize(avg_roi = mean(ROI, na.rm = TRUE))
  
  # ARIMA analysis
  arima_model <- auto.arima(monthly_avg_roi$avg_roi)
  
  # Calculate MAE
  arima_accuracy <- accuracy(arima_model)
  mae <- mean(abs(arima_accuracy[, "MAE"]))
  mae_list[[segment]] <- mae
  
  # Plot monthly average ROI against date and store the plot in the list
  plots_list[[segment]] <- autoplot(arima_model$residuals) +
    labs(x = "Date", y = "Residuals", title = paste("ARIMA Residuals Plot for", segment))
}

# Combine plots separately for each segment using facet_wrap
plots_combined <- do.call(gridExtra::grid.arrange, c(plots_list, ncol = 2))
plots_combined <- plots_combined + facet_wrap(~ names(plots_list), ncol = 2)

# Print the combined plot
print(plots_combined)

# Print the MAE for each segment
for (segment in segments) {
  cat("MAE for", segment, ":", mae_list[[segment]], "\n")
}










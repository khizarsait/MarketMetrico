
# Load necessary libraries
library(dplyr)
library(forecast)
library(ggplot2)
library(gridExtra)

# Read the dataset
marketing_data <- read.csv("final_modified_data.csv", stringsAsFactors = FALSE)

# Convert 'Date' column to Date format
marketing_data$Date <- as.Date(marketing_data$Date, format = "%d-%m-%Y")

# Take the first 10,000 records
#sampled_data <- head(marketing_data, 100000)

# Subset data for each unique channel
channels <- unique(marketing_data$Channel_Used)

# Create a list to store plots for each channel
plots_list <- list()

# Create a list to store MAE for each channel
mae_list <- list()

# Perform ARIMA analysis and plot for each channel
for (channel in channels) {
  channel_data <- filter(marketing_data, Channel_Used == channel)
  
  # Aggregate data to monthly level and calculate average ROI
  monthly_avg_roi <- channel_data %>%
    group_by(month = format(Date, "%Y-%m")) %>%
    summarize(avg_roi = mean(ROI, na.rm = TRUE))
  
  # ARIMA analysis
  arima_model <- auto.arima(monthly_avg_roi$avg_roi)
  
  # Calculate MAE
  arima_accuracy <- accuracy(arima_model)
  mae <- mean(abs(arima_accuracy[, "MAE"]))
  mae_list[[channel]] <- mae
  
  # Plot monthly average ROI against date and store the plot in the list
  plots_list[[channel]] <- autoplot(arima_model$residuals) +
    labs(x = "Date", y = "Residuals", title = paste("ARIMA Residuals Plot for", channel))
}

# Combine plots separately for each channel using facet_wrap
plots_combined <- do.call(gridExtra::grid.arrange, c(plots_list, ncol = 2))
plots_combined <- plots_combined + facet_wrap(~ names(plots_list), ncol = 2)

# Print the combined plot
print(plots_combined)

# Print the MAE for each channel
for (channel in channels) {
  cat("MAE for", channel, ":", mae_list[[channel]], "\n")
}
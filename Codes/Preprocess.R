library(dplyr) #Used to manipulate data in R
library(readr) #Used to reAd csv files
library(DataExplorer) #Used To explore data
library(DT) #Used to display tables in Rmarkdown
library(ggplot2) #Used to display different plots and graphs
library(heatmaply) #Used for Heatmaps
library(kableExtra)
library(tidyr)


#Reading the Dataset
data <- read.csv("C:/Users/vijay/Downloads/marketing_campaign_dataset.csv", stringsAsFactors = FALSE)

#looking for missing values
plot_missing(data)


#Duplicating the data
data_new <-data

data_new$Target_Audience[data_new$Target_Audience=="All Ages"]<-"All a-b"

data_new <- data_new %>%
  separate(Target_Audience, into = c("Target_Audience_Gender", "Target_Audience_Age"), sep = " ")

DT::datatable(head(data_new, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))


data_new <- data_new %>%
  separate(Target_Audience_Age, into = c("Target_Audience_Age_Min", "Target_Audience_Age_Max"), sep = "-")


DT::datatable(head(data_new, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))
#replacing a
min_age <- min(data_new$Target_Audience_Age_Min[data_new$Target_Audience_Age_Min != "a"], na.rm = TRUE)

# Replace "a" with the minimum value
data_new$Target_Audience_Age_Min[data_new$Target_Audience_Age_Min == "a"] <- min_age

DT::datatable(head(data_new, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))

#replacing b
max_age <- max(data_new$Target_Audience_Age_Max[data_new$Target_Audience_Age_Max != "b"], na.rm = TRUE)

# Replace "a" with the minimum value
data_new$Target_Audience_Age_Max[data_new$Target_Audience_Age_Max == "b"] <- max_age

DT::datatable(head(data_new, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))

data_new <- data_new %>%
  mutate(Duration_In_Days = as.numeric(sub(" days", "", Duration)))

# Drop the original Duration column
data_new <- data_new %>%
  select(-Duration)
non_numeric_values <- data_new$Acquisition_Cost[!grepl("^\\$?[0-9,.]*$", data_new$Acquisition_Cost, perl = TRUE)]
# If there are non-numeric values, print them
if (length(non_numeric_values) > 0) {
  print(paste("Non-numeric values in Acquisition_Cost column:", unique(non_numeric_values)))
}

# Modify the Acquisition_Cost column
data_new <- data_new %>%
  mutate(Acquisition_Cost = as.numeric(gsub("[^0-9.]", "", Acquisition_Cost)))

# Print the modified data
print(data_new)
plot_missing(data_new)

DT::datatable(head(data_new, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))


write.csv(data_new, "final_data.csv",row.names=FALSE)
getwd()

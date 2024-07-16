library(dplyr) #Used to manipulate data in R
library(readr) #Used to reAd csv files
library(DataExplorer) #Used To explore data
library(DT) #Used to display tables in Rmarkdown
library(ggplot2) #Used to display different plots and graphs
library(heatmaply) #Used for Heatmaps
library(kableExtra)
library(tidyr)


#Reading the Dataset
data <- read.csv("C:/Users/vijay/Desktop/PMA/final_data.csv", stringsAsFactors = FALSE)
print(data)

#looking for missing values
plot_missing(data)


DT::datatable(head(data, 10), rownames = FALSE, options = list(pageLength = 10))


# Calculate CPC, CTR, CPA, and CPI
data <- data %>%
  mutate(
    # CPC (Cost Per Click)
    Target_Audience_Age_Mean=(Target_Audience_Age_Min+Target_Audience_Age_Max)/2,
    
    CPC = Acquisition_Cost / Clicks,
    
    # CTR (Click Through Rate)
    CTR = (Clicks / Impressions) * 100,
    
    # CPA (Cost Per Acquisition)
    CPA = Acquisition_Cost / (Conversion_Rate * Impressions),
    
    # CPI (Cost Per Impression)
    CPI = Acquisition_Cost / Impressions
  )

DT::datatable(head(data, 10), rownames = FALSE, options = list(pageLength = 10))


write.csv(data, "final_modified_data.csv",row.names=FALSE)
getwd()
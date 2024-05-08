library(dplyr) #Used to manipulate data in R
library(readr) #Used to reAd csv files
library(DataExplorer) #Used To explore data
library(DT) #Used to display tables in Rmarkdown
library(ggplot2) #Used to display different plots and graphs
library(heatmaply) #Used for Heatmaps
library(kableExtra) #Used for tables

#Lets bring our dat
data <- read.csv("C:/Users/vijay/Downloads/KAG_conversion_data.csv", stringsAsFactors = FALSE)

#Lets drop fb_campaign_id
data<- data %>% select(-fb_campaign_id)

#Lets Rename xyz_campaign_id
data<- data %>% rename(campaign_id=xyz_campaign_id)

#Lets Rename Interest
data<- data %>% rename(Interest_ID = interest)

#Look for missing data
plot_missing(data)


#Lets calculate the different variables for our calculations
data <- data %>% mutate(CTR = round(((Clicks / Impressions) * 100),2), 
                        CPC = ifelse(Clicks != 0, round(Spent / Clicks,2), Spent), 
                        CostPerConv_Total = ifelse(Total_Conversion !=0,round(Spent/Total_Conversion,2),Spent),
                        CostPerConv_Approved = ifelse(Approved_Conversion !=0,round(Spent/Approved_Conversion,2),Spent),
                        CPM = round((Spent / Impressions) * 1000, 2) )

#Lets remove some columns to see only columns that analyzes clicks and Impressions' metrics
desc_data <- data %>% select(-Total_Conversion, -Approved_Conversion, -CostPerConv_Total, -CostPerConv_Approved) 

#Descriptive Statistics of this dataset
DT::datatable(head(desc_data, 25),
              rownames = FALSE,
              options = list(
                pageLength = 10))


#Top 10 ad_ids by number of impressions
Top_10 <- data[order(data$Impressions,decreasing = TRUE),] %>% select(ad_id, CPC, CPM, Impressions) %>% head(10)

DT::datatable(head(Top_10, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))


#Bottom 10 ad_ids by number of impressions
Bottom_10 <- data[order(data$Impressions,decreasing = FALSE),] %>% select(ad_id, CPC, CPM, Impressions) 

DT::datatable(head(Bottom_10, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))
#Ad with least CPC that leAd to most impressions
data %>% 
  filter(CPC == min(CPC)) %>% 
  filter(Impressions == max(Impressions)) %>% 
  select(ad_id, CPC, Impressions, Clicks) %>%
  kable() %>%
  kable_styling(c("striped", "bordered"))

#Ad with highest CPC that leAd to least impressions
data %>% 
  filter(CPC == max(CPC)) %>%
  filter(Impressions == min(Impressions)) %>% 
  select(ad_id, CPC, Impressions, Clicks) %>%
  kable() %>%
  kable_styling(c("striped", "bordered"))

#What campaign spent least efficiently on brand awareness on an average
data %>% 
  filter(CPM == max(CPM)) %>% 
  filter(Impressions == min(Impressions)) %>% 
  select(campaign_id, CPM, Impressions, Clicks) %>%
  kable() %>%
  kable_styling(c("striped", "bordered"))



#What campaign spent most efficiently on brand awareness on an average
data %>% filter(CPM == min(CPM)) %>% 
  filter(Impressions == max(Impressions)) %>% 
  select(campaign_id, CPM, Impressions, Clicks) %>%
  kable() %>%
  kable_styling(c("striped", "bordered"))



#Lets calculate ROAS
data_new <-
  data %>%
  mutate(totConv = Total_Conversion + Approved_Conversion, conVal = Total_Conversion * 5, appConVal = Approved_Conversion *50) %>%
  mutate(totConVal = conVal + appConVal) %>%
  mutate(ROAS = round(totConVal / Spent, 2)) %>%
  filter(ROAS != 'Inf') %>%
  filter(ROAS != 'NaN') 

# Lets build a Boxplot based on gender
data_new %>%
  ggplot(aes(x = as.factor(campaign_id), y = ROAS, fill = as.factor(gender))) + 
  geom_boxplot() + 
  scale_y_log10() + 
  ggtitle("ROAS Boxplot") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(color="dark green", size=14, face="bold.italic"),
    axis.title.x = element_text(size=14, face="bold"),
    axis.title.y = element_text(size=14, face="bold"))+
  labs(x = 'Campaign ID', y = 'ROAS') +
  scale_fill_discrete(name="Gender")

# ROAS by gender for the different campaigns
data_new %>%
  select(campaign_id, gender, ROAS) %>%
  group_by(campaign_id, gender) %>%
  summarise(median.ROAS = median(ROAS), mean.ROAS = mean(ROAS))%>%
  kable() %>%
  kable_styling(c("striped", "bordered"))


#Lets create a trimmed dataset 
dataTrim <- data_new %>%
  select(CTR, CPC, CostPerConv_Approved, CostPerConv_Total, CPM, ROAS, Impressions, Spent, Clicks)

#After omitting missing values, and normalize data, lets calculate correlations and plot a heatmap
heatmaply_cor(cor(normalize(na.omit(dataTrim))))




#Lets do a boxplot to determine the campaign where the most money was spent
ggplot(data, aes(as.factor(campaign_id), Spent)) +
  geom_boxplot(fill = "#086CA2", color = "black") + 
  scale_y_log10() +
  ggtitle("Spent Boxplot by Campaign ID") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    panel.border = element_blank(),
    panel.background = element_rect(colour = "black"),
    plot.title = element_text(color="dark green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="dark red", size=14, face="bold"))+
  labs(x = "Campaign ID", y = "Advertising Spend")




#Lets filter our Campaign ID '1178'
data_1178 <- data_new %>% 
  filter(campaign_id == 1178) %>%
  select(-campaign_id)

#Lets look at the distribution of this data
plot_bar(data_1178,ggtheme = theme_classic())




plot_histogram(data_1178,ggtheme = theme_classic())




#Lets also see the correlation matrix for this campaign
plot_correlation(data_1178)



#Lets now see an overview of the success of conversion and ROAS in relation to the amount spent.

ggplot(data_1178, aes(Spent, ROAS), ggtheme = theme_classic()) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_y_log10() +
  labs(x = "Amount spent on campaign", y = "ROAS")


ggplot(data_1178, aes(Spent, CostPerConv_Total), ggtheme = theme_light()) +
  geom_point() + 
  scale_y_log10() +
  geom_smooth(method = "lm") +
  labs(x = "Amount spent on campaign", y = "Conversion Value")



#Lets build a boxplot with gender and ROAS
ggplot(data_1178, aes(gender, ROAS, fill =gender)) + 
  geom_boxplot() + 
  scale_y_log10()+
  ggtitle("ROAS Boxplot") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(color="dark green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold"),
    axis.title.y = element_text(color="dark red", size=14, face="bold")) +
  labs(x = 'Gender', y = 'ROAS') +
  scale_fill_discrete(name="Gender")

#Now lets see which gender has a higher ROAS mean
data_1178 %>%
  select(gender, ROAS) %>%
  group_by(gender) %>%
  summarise(median.ROAS = median(ROAS), mean.ROAS = mean(ROAS))%>%
  kable() %>%
  kable_styling(c("striped", "bordered"))

#Lets take a look at the amount of clicks per Interest ID
ggplot(data_1178, aes(Interest_ID,Clicks))  +
  geom_bar(stat = 'identity') +
  ggtitle("ROAS by Interest ID") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "gray"),
    plot.title = element_text(color="dark green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold"),
    axis.title.y = element_text(color="dark red", size=14, face="bold")) +
  labs(x = 'Interest ID', y = 'ROAS')





#Lets take a look at the amount of ROAS per Interest ID
ggplot(data_1178, aes(Interest_ID,ROAS))  +
  geom_bar(stat = 'identity') +
  ggtitle("ROAS by Interest ID") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
    plot.title = element_text(color="dark green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold"),
    axis.title.y = element_text(color="dark red", size=14, face="bold")) +
  labs(x = 'Interest ID', y = 'ROAS')




#Lets further explore and look at our best performers by ROAS
Interest_1178 <- data_1178 %>%
  select(Interest_ID, ROAS, Clicks) %>%
  group_by(Interest_ID) %>%
  summarise(median.ROAS = median(ROAS),
            mean.ROAS = round(mean(ROAS),2), 
            clicks = sum(Clicks)) %>%
  arrange(desc(mean.ROAS))

#Lets display our top 10 Interest ID by Mean ROAS
DT::datatable(head(Interest_1178, 10),
              rownames = FALSE,
              options = list(
                pageLength = 10))



#Lets build a boxplot for these interest ID and split by gender
data_1178 %>%
  filter(Interest_ID == 104 | Interest_ID == 101 | Interest_ID == 15) %>%
  ggplot(aes(x = as.factor(Interest_ID), y = ROAS, fill = gender)) + 
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle("ROAS Boxplot by Gender") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(color="dark green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold"),
    axis.title.y = element_text(color="dark red", size=14, face="bold")) +
  labs(x = 'Interest ID', y = 'ROAS') +
  scale_fill_discrete(name="Gender")


#Lets further explore and look at our best performers by ROAS
Gender_1178 <- data_1178 %>%
  select(Interest_ID, gender, ROAS, Clicks) %>%
  group_by(Interest_ID, gender) %>%
  filter(Interest_ID == 104 | Interest_ID == 101 | Interest_ID == 15) %>%
  summarise(median.ROAS = median(ROAS),
            mean.ROAS = round(mean(ROAS),2), 
            clicks = sum(Clicks)) %>%
  arrange(desc(mean.ROAS))

#Lets display our top Mean ROAS split by Interest ID and Gender
Gender_1178%>%
  kable() %>%
  kable_styling(c("striped", "bordered"))




#Lets build a boxplot for Interest ID 15, 101 & 104

data_1178 %>%
  filter(Interest_ID == 104 | Interest_ID == 101 | Interest_ID == 15) %>%
  ggplot(aes(x = as.factor(age), y = ROAS, fill = as.factor(Interest_ID))) + 
  geom_boxplot() + 
  scale_y_log10() +
  ggtitle("ROAS Boxplot by Age Group") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(
    plot.title = element_text(color="dark green", size=14, face="bold.italic"),
    axis.title.x = element_text(color="dark blue", size=14, face="bold"),
    axis.title.y = element_text(color="dark red", size=14, face="bold")) +
  labs(x = 'Age Group', y = 'ROAS') +
  scale_fill_discrete(name="Interest\nID")


#Lets further explore and look at our best performers by ROAS
Age_1178 <- data_1178 %>%
  select(Interest_ID, age, ROAS, Clicks) %>%
  group_by(age, Interest_ID) %>%
  filter(Interest_ID == 104 | Interest_ID == 101 | Interest_ID == 15) %>%
  summarise(median.ROAS = median(ROAS),
            mean.ROAS = round(mean(ROAS),2), 
            clicks = sum(Clicks)) %>%
  arrange(desc(mean.ROAS))

#Lets display our top Mean ROAS split by Interest ID & Age Group
DT::datatable(head(Age_1178, 20),
              options = list(
                pageLength = 10))

## Fire/smoke data by county
# Pull data for the 2018 Camp Fire in Butte County, calculate daily average
# Official incident dates are 11-8-2018 to 11-25-2018

library(dplyr)
library(lubridate)
library(ggplot2)

smoke <- read.csv("Data/Fire/ad_viz_plotval_data.csv")

smoke$Date <- mdy(smoke$Date)

# Review data
smoke[smoke$Daily.Mean.PM2.5.Concentration<=0, ] # some observations are zero or negative
nrow(smoke[smoke$Daily.Mean.PM2.5.Concentration<=0, ]) / nrow(smoke) 

# 0.6% of observations are not acceptable. Remove them.
smoke <- smoke %>% filter(Daily.Mean.PM2.5.Concentration > 0)

# Dates of Camp Fire
start_date_1 <- mdy("11/08/2018") 
end_date_1 <- mdy("11/21/2018")

# Dates of the 2 months preceding the fire
start_date_2 <- mdy("09/01/2018")
end_date_2 <- mdy("11/01/2018")

# Calculate the incremental pollution from the fire
camp <- smoke %>% filter(Date >= start_date_1, Date <= end_date_1)
camp_avg <- camp %>%
  group_by(COUNTY) %>%
  summarize(camp_avg = mean(Daily.Mean.PM2.5.Concentration))

pre_camp <- smoke %>% filter(Date >= start_date_2, Date <= end_date_2)
pre_camp_avg <- pre_camp %>%
  group_by(COUNTY) %>%
  summarize(pre_camp_avg = mean(Daily.Mean.PM2.5.Concentration))

camp_avg$camp_only <- camp_avg$camp_avg - pre_camp_avg$pre_camp_avg


# Visualize data to see trends
ggplot(camp_avg, aes(x = Date, y = camp_only, color = COUNTY)) +
  geom_point() +
  labs(x = "Date", y = "PM2.5 Concentration", color = "County") +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
  theme_minimal()

ggplot(smoke_no_outliers_tukey, aes(x = COUNTY, y = Daily.Mean.PM2.5.Concentration)) +
  geom_boxplot() +
  labs(x = "County", y = "PM2.5 Concentration") +
  theme_minimal()

# PM2.5 elevated only until 11/21/2018. Re-filter dataset and rerun scatterplot
end_date <- mdy("11/21/2018")
campsmoke <- campsmoke %>% filter(Date >= start_date, Date <= end_date)

# Note: Do we want to only include counties above a certain PM2.5 concentration? 
# (i.e., to filter out counties that weren't affected from the fire?)

observation_count <- count(campsmoke, COUNTY)

avg_daily_pollution <- campsmoke %>%
  group_by(COUNTY) %>%
  summarize(average_concentration = mean(Daily.Mean.PM2.5.Concentration))

ggplot(avg_daily_pollution, aes(x = COUNTY, y = average_concentration)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "County", y = "Average Concentration") +
  theme_minimal() +
  coord_flip()

saveRDS(avg_daily_pollution, "Data/Fire/smokedata.RDS")

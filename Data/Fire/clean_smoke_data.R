## Fire/smoke data by county
# Pull data for the 2018 Camp Fire in Butte County, calculate daily average
# Official incident dates are 11-8-2018 to 11-25-2018

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

smoke <- read.csv("Data/Fire/ad_viz_plotval_data.csv")

smoke$Date <- mdy(smoke$Date)
smoke <- smoke %>%
  rename(countyfip = COUNTY_CODE)
# Review data
nrow(smoke[smoke$Daily.Mean.PM2.5.Concentration<=0, ]) # some observations are zero or negative
nrow(smoke[smoke$Daily.Mean.PM2.5.Concentration<=0, ]) / nrow(smoke) 

# 0.6% of observations are not acceptable. Remove them.
smoke <- smoke %>% filter(Daily.Mean.PM2.5.Concentration > 0)

smoke_abbr <- smoke %>%
  group_by(Date, COUNTY, countyfip) %>%
  summarize(daily_mean_PM2.5 = mean(Daily.Mean.PM2.5.Concentration))

# Dates of Camp Fire
start_date_1 <- mdy("11/08/2018") 
end_date_1 <- mdy("11/21/2018")

# Dates of the 2 months preceding the fire
start_date_2 <- mdy("09/01/2018")
end_date_2 <- mdy("11/01/2018")

# Calculate the incremental pollution from the fire, following the methods from Gan et al 2022
# (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8745685/)
## First get background pollution (i.e., median pollution during the 2 months preceding the fire)
## Then subtract that median value from each measurement during the fire
pre_camp <- smoke_abbr %>% filter(Date >= start_date_2, Date <= end_date_2)

background_pollution <- pre_camp %>%
  group_by(COUNTY, countyfip) %>%
  summarize(background_PM2.5 = median(daily_mean_PM2.5))

camp <- smoke_abbr %>% filter(Date >= start_date_1, Date <= end_date_1)
camp <- merge(camp, background_pollution, by = c("COUNTY","countyfip"), all.x = TRUE)

camp <- camp %>%
  mutate(fire_PM2.5 = daily_mean_PM2.5 - background_PM2.5)  

# 66 rows (10%) took on negative PM2.5 values. Most of them are counties not affected
# Some seem to be measurement errors. Recode all as 0 (no fire effect).

camp$fire_PM2.5[camp$fire_PM2.5<0] <- 0

camp <- arrange(camp, countyfip, Date)

saveRDS(camp, "Data/Fire/campfire_PM2.5.RDS")
#   
# ## Now we need to calculate number of days where daily PM2.5 > 35 ug/m3 
# smoke_exposure <- camp %>%
#   filter(fire_PM2.5 > 35) %>%
#   group_by(COUNTY, COUNTY_CODE) %>%
#   summarize(days_exposed = n())
 
# saveRDS(smoke_exposure, "Data/Fire/campfire_exposure.RDS")

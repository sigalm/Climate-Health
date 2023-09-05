## Fire/smoke data by county
# Pull data for the 2018 Camp Fire in Butte County, calculate daily average
# Official incident dates are 11-8-2018 to 11-25-2018

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

smoke1 <- read.csv("Data/Fire/ad_viz_plotval_data_2018.csv")
smoke2 <- read.csv("Data/Fire/ad_viz_plotval_data_2019.csv")

# identical(names(smoke1), names(smoke2))  # check if ok to append

smoke <- rbind(smoke1, smoke2)

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

# Dates for the final data frame
start_date_3 <- start_date_1 - weeks(1)
end_date_3 <- start_date_1 + years(1) - days(1)

# Calculate the incremental pollution from the fire, following the methods from Gan et al 2022
# (https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8745685/)
## First get background pollution (i.e., median pollution during the 2 months preceding the fire)
## Then subtract that median value from each measurement during the fire
pre_camp <- smoke_abbr %>% filter(Date >= start_date_2, Date <= end_date_2)

background_pollution <- pre_camp %>%
  group_by(COUNTY, countyfip) %>%
  summarize(background_PM2.5 = median(daily_mean_PM2.5))

camp <- smoke_abbr %>% filter(Date >= start_date_3, Date <= end_date_3)
camp <- merge(camp, background_pollution, by = c("COUNTY","countyfip"), all.x = TRUE)

camp <- camp %>%
  mutate(fire_PM2.5 = daily_mean_PM2.5 - background_PM2.5)

# Assign 0 to dates outside of the fire
camp$fire_PM2.5[camp$Date<start_date_1 | camp$Date>end_date_1] <- 0

# 67 rows (10%) took on negative PM2.5 values. Most of them are counties that were not affected,
# Some seem to be measurement errors. Recode all as 0 (no fire effect).
nrow(camp[camp$fire_PM2.5<=0 & camp$Date>=start_date_1 & camp$Date<=end_date_1, ]) # some observations are zero or negative
nrow(camp[camp$fire_PM2.5<=0 & camp$Date>=start_date_1 & camp$Date<=end_date_1, ]) / nrow(camp[camp$Date>=start_date_1 & camp$Date<=end_date_1, ]) 

camp$fire_PM2.5[camp$fire_PM2.5<0] <- 0

# Reorder rows to make more sense then save dataframe
camp <- arrange(camp, countyfip, Date)

saveRDS(camp, "Data/Fire/campfire_PM2.5.RDS")


camp <- readRDS("Data/Fire/campfire_PM2.5.RDS")
# Create a second data frame that contains # of smoke days each week, with counties in rows and weeks (cycles) in columns
camp <- camp %>%
  group_by(countyfip) %>%
  mutate(cycle = as.integer(difftime(Date, first(Date), units = "days")) %/% 7)

camp_roll <- camp %>%
  filter(fire_PM2.5 > 35) %>%
  group_by(countyfip, cycle) %>%
  summarize(count_smokey = n())

smoke_days <- expand.grid(countyfip = unique(camp$countyfip), cycle = unique(camp$cycle))
smoke_days <- left_join(smoke_days, camp_roll, by = c("countyfip", "cycle"))
smoke_days$count_smokey[is.na(smoke_days$count_smokey)] <- 0
smoke_per_cycle <- smoke_days %>%
  pivot_wider(names_from = cycle, values_from = count_smokey, names_prefix = "Cycle_")

smoke_per_cycle <- select(smoke_per_cycle, -1, everything(), 1)

saveRDS(smoke_per_cycle, "Data/Fire/smoke_per_cycle.RDS")


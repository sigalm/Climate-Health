# Get 2021 ACS microdata from IPUMS USA
# The documentation for this dataset can be found in "Data/Microdata/usa_00002_codebook.pdf". 


####* Initial data import *####

library(dplyr)
data <- read.csv(gzfile("Data/Microdata/usa_00002.csv.gz")) 
data <- data[data$STATEFIP == 6, ]             ## Only keep CA data (STATEFIP = 6)
data <- data[data$COUNTYFIP != 0, ]            ## drop non-identifiable counties (COUNTYFIP = 0)
names(data) <- tolower(names(data))            ## Change column names to lower caps to match code
saveRDS(data,"Data/Microdata/ca_00002.rds")


####* Import reduced CA dataset *####
data <- readRDS("Data/Microdata/ca_00002.rds")

## Add rural/urban designation to responses based on county
# https://www.counties.org/sites/main/files/file-attachments/2020-june3-countycaucusesinfographic-4-final.pdf
counties <- read.csv("Data/Microdata/counties.csv")   # Read in list of counties and corresponding FIPS codes

## Create rural / urban flag variable: 1 = rural or suburban, 0 = urban
data$rural <- counties$Designation[match(data$countyfip, counties$FIPS)]
data$rural[data$rural == "suburban" | data$rural == "rural"] <- 1
data$rural[data$rural == "urban"] <- 0
data$rural <- as.numeric(data$rural)

## Recode sex variable (male = 1, female = 0)
data$sex <- data$sex - 1

## Create categorical age variable
data$age_cat <- cut(data$age, breaks = c(-Inf, 4, 17, 64, Inf), 
                    labels = c("0-4", "5-17", "18-64", "65+"))
data$age_cat <- gsub("-"," to ", data$age_cat)


####** Some cleaning **####
data$hhincome[data$hhincome == 9999999] <- NA
data$hcovany <- data$hcovany - 1                  # 1 = has any coverage, 0 = no coverage
data$hcovpriv <- data$hcovpriv - 1                # 1 = has private insurance, 0 = no private insurance
data$hcovpub <- data$hcovpub - 1                  # 1 = has public insurance, 0 = no public insurance

data$poverty[data$poverty == 0] <- NA             # continuous, past-year income as a percentage of the poverty threshold
data$poverty_cat <- NA                            # create categorical poverty variable
data$poverty_cat[data$poverty>=200] <- 0          # income greater than double poverty line
data$poverty_cat[data$poverty<200 & 
                   data$poverty>=100] <- 1        # income at or less than double poverty line
data$poverty_cat[data$poverty<100] <- 2           # income under poverty line



## Create initial asthma states
asthma <- read.csv("Data/Asthma/Prevalence/ca_asthma_by_county.csv")              # https://www.cdph.ca.gov/Programs/CCDPHP/DEODC/EHIB/CPE/Pages/CaliforniaBreathingCountyAsthmaProfiles.aspx
asthma$County_prevalence <- as.numeric(asthma$County_prevalence)  
asthma$FIPS <- counties$FIPS[match(asthma$County, counties$County)]
asthma$Group <- gsub("-", " to ", asthma$Group)


# Notes on asthma prevalence data above:
# Data largely missing for ages 0-4 (only 5 out of 58 counties are available). Not enough to impute. Use CA prevalence instead.
# 31/58 values available for ages 5-17. Impute using "residual" average (i.e., calculate the average of missing values and assign that average to each missing data point)
# The two remaining age groups have complete data.

avg_0to4_CA <- asthma$California_prevalence[asthma$Group=="0 to 4"][1]

## Impute missing values for ages 5-17
# Calculate population weights for counties (age-specific)
pop_by_county <- read.csv("Data/Asthma/Prevalence/ca_population_by_age_by_county.csv")
pop_by_county$pop_5to17 <- pop_by_county$SE_T008_003 + pop_by_county$SE_T008_004 + pop_by_county$SE_T008_005
pop_by_county$pop_weight <- pop_by_county$pop_5to17 / sum(pop_by_county$pop_5to17)

avg_CA_5to17 <- asthma$California_prevalence[asthma$Group=="5 to 17"][1]
wavg_known_5to17 <- sum(asthma$County_prevalence[asthma$Group=="5 to 17"] * pop_by_county$pop_weight, na.rm = TRUE)
w_unknown_5to17 <- sum(pop_by_county$pop_weight[is.na(asthma$County_prevalence[asthma$Group=="5 to 17"])])
avg_unknown_5to17 <- (avg_CA_5to17 - wavg_known_5to17) / w_unknown_5to17

# Calculate SES risks using RRs from: https://www.lung.org/research/trends-in-lung-disease/asthma-trends-brief/current-demographics#:~:text=In%202018%2C%20current%20asthma%20rates,to%20above%20the%20poverty%20threshold.
# R0 * (p0 + RR1*p1 + RR2*p2) = Rt
# p0: reference probability, income 2x or more of poverty line
# p1: income 1 to <2 of poverty line, RR1 = 9/6.8
# p2: income <1x of poverty line, RR2 = 11/6.8

p0 <- sum(data$poverty_cat == 0, na.rm = TRUE) / sum(!is.na(data$poverty_cat))
p1 <- sum(data$poverty_cat == 1, na.rm = TRUE) / sum(!is.na(data$poverty_cat))
p2 <- sum(data$poverty_cat == 2, na.rm = TRUE) / sum(!is.na(data$poverty_cat))

RR1 <- 9/6.8
RR2 <- 11/6.8

# Asthma control probabilities for those with asthma
# Refer to transition probabilities calibration workbook for how these were estimated)
asthma_control_probs <- read.csv("Data/Asthma/Prevalence/asthma_control_probs.csv")

assign_asthma_status <- function(countyfip, age_cat, poverty_cat) {
  
  row_match <- which(asthma$FIPS == countyfip & asthma$Group == age_cat)  # Look up the row in the asthma prevalence data that matches the county and age
  if (is.na(asthma$County_prevalence[row_match])) {
    if (age_cat == "0 to 4") {
      prevalence <- avg_0to4_CA                                # if age 0-4, assign age-specific California average prevalence
    } else if (age_cat == "5 to 17") {
      prevalence <- avg_unknown_5to17                         # if age 5-17, assign weighted average of missing values for age- and county-specific prevalence
    }} else {
      prevalence <- asthma$County_prevalence[row_match]        # otherwise pull age- and county-specific value from asthma data
    }
  
  # Adjust prevalence for poverty level
  R0 <- prevalence / (p0 + RR1*p1 + RR2*p2)                    # Calculate base risk (no risk factor)
  if (!is.na(poverty_cat)) {
    if (poverty_cat == 0) {
      prevalence <- R0
    } else if (poverty_cat == 1) {
      prevalence <- R0 * RR1
    } else if (poverty_cat == 2) {
      prevalence <- R0 * RR2
    } else {
      prevalence <- prevalence  # Default to average age- and county-specific prevalence if poverty level not known
    }}
  
  # Simulate a binary asthma status (yes/no) based on the prevalence and poverty level
  if (runif(1) < prevalence) {
    asthma_status <- 1
  } else {
    asthma_status <- 0
  }
  
  # Assign asthma control to those with asthma
  if (asthma_status == 1) {
    asthma_control <- sample(asthma_control_probs$state_name, 1, prob = asthma_control_probs$control_prob)
  } else {
    asthma_control <- 0
  }
  
  return(asthma_control)
}


## Assign asthma status to each individual in the individual-level demographic data
data <- data %>% 
  mutate(asthma_status = mapply(assign_asthma_status, countyfip, age_cat, poverty_cat))

## Add initial asthma therapy based on control
asthma_therapies <- read.csv("Data/Asthma/Therapies/therapies.csv", row.names = 1) # https://onlinelibrary.wiley.com/doi/10.1111/j.1398-9995.2007.01383.x


data$asthma_therapy <- apply(asthma_therapies[as.character(data$asthma_status), ], 1, function(row) {
  sample(names(asthma_therapies), 1, prob = row)
})
  

####* Review and save results *#### 
 
## Calculate missing population ##
fips <- unique(data$countyfip)
my_counties <- counties[counties$FIPS %in% fips, ]
pop_by_county <- read.csv("Data/Microdata/pop_by_county.csv")                     # https://www.california-demographics.com/counties_by_population
my_counties <- merge(my_counties, pop_by_county, by = "County", all.x = TRUE)
ca_population <- sum(pop_by_county$Population)
my_population <- sum(my_counties$Population)
pop_accounted_for <- my_population*100/ca_population                              # The 35 counties represent 96.4% of the entire CA population
                                                                                  # The sample in "data" represent 1% of the 96.4%
## Compare prevalence from asthma data vs generated values
Rt <- asthma$California_prevalence[asthma$Group == "All"][1]
R0 <-  Rt / (p0 + RR1*p1 + RR2*p2)
prop.table(table(data$asthma_status, data$poverty_cat), margin =2)

## Save dataset
saveRDS(data, "Data/Microdata/microdata.RDS")







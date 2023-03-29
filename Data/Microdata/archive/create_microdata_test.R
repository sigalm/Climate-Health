# Load the data set
data <- read.csv("Data/Microdata/ca_age_dist.csv")    # source: ACS 2021 1-yr, https://censusreporter.org/profiles/04000US06-california/
data <- data[1:9, ]                                   # delete empty rows
sum(data$probability)                                 # adds up to 1.001
data$probability[4] <- data$probability[4] - 0.001
write.csv(data, "Data/Microdata/ca_age_dist_clean.csv", row.names = FALSE)   # save clean csv

data <- read.csv("Data/Microdata/ca_age_dist_clean.csv") # read in clean data




sample_size <- 100000
cols <- c("sex", "age", "geo")
microdata <- data.frame(matrix(nrow = sample_size, ncol=length(cols)))
colnames(microdata) <- cols

microdata$sex <- sample(0:1, sample_size, 0.50)   # 1 is female

age_by_sex <- read.csv("Data/Microdata/age_by_sex.csv")

age_cats <- age_by_sex$age_group[1:12]


probs_male <- age_by_sex$probability[age_by_sex$sex=="male"]
probs_female <- age_by_sex$probability[age_by_sex$sex=="female"]

ages_female <- rep(age_cats, times = probs_female * length(microdata$sex[microdata$sex==1]))
length(ages) == sample_size   

for (i in seq_along(age_cats)) {
  low <- age_cats[i]
  high <- ifelse(i < length(age_cats), age_cats[i+1] - 1, 100)
  n <- length(ages[ages==low])
  ages[ages==low] <- sample(low:high, n, replace = TRUE)
}


# Create a data frame with the ages
micro_data <- data.frame(age = sample(ages, sample_size, replace = TRUE))

# Write the micro data set to a new R dataset
write.rds(micro_data, file = "micro_data_set.rds", row.names = FALSE)

asthma <- read.csv("Data/Asthma/Prevalence/ca_asthma_by_county.csv")              # https://www.cdph.ca.gov/Programs/CCDPHP/DEODC/EHIB/CPE/Pages/CaliforniaBreathingCountyAsthmaProfiles.aspx
asthma$County_prevalence <- as.numeric(asthma$County_prevalence)  
asthma$Group <- gsub("-", " to ", asthma$Group)

library(ggplot2)

asthma2 <- asthma
asthma2$Group <-
  factor(asthma2$Group, levels = c("0 to 4", "5 to 17", "18 to 64", "65+", "All"))
asthma2$County_prevalence <- asthma2$County_prevalence*100
asthma2$California_prevalence <- asthma2$California_prevalence*100

# Boxplot to find outliers
box_by_age <-
  ggplot(data = asthma2, aes(x = Group, y = County_prevalence)) +
  geom_boxplot() +
  labs(x = "Age Group", y = "County Prevalence") +
  ggtitle("County Prevalence by Age Group (Boxplot)")

sorted_data <-
  asthma2[order(asthma2$County_prevalence, decreasing = TRUE),]
head(sorted_data, 3)

# Faceted bar plot, by county
facet_by_county <-
  ggplot(data = asthma2, aes(x = Group, y = County_prevalence, fill = Group)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 8.7, linetype = "dashed", color = "red") +
  labs(x = "Age Group", y = "Asthma Prevalence", fill = "Age Group") +
  ggtitle("Asthma Prevalence by Age Group and County") +
  facet_wrap( ~ County, scales = "fixed") +
  coord_cartesian(ylim = c(0, 20))


# Heatmap
heatmap <-
  ggplot(data = asthma2, aes(x = County, y = Group, fill = County_prevalence)) +
  geom_tile() +
  labs(x = "County", y = "Age Group", fill = "Asthma Prevalence") +
  ggtitle("Asthma Prevalence by Age Group and County") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_fill_gradient(low = "white", high = "red")  # Inverted color scheme



# Faceted bar plot, by age group
custom_palette <- 
  c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928",
    "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99",
    "#636363", "#fb9a99", "#b2df8a", "#1f78b4", "#33a02c", "#e31a1c",
    "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99",
    "#fdbf6f", "#cab2d6", "#ffff99", "#636363", "#fb9a99", "#b2df8a",
    "#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928",
    "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6", "#ffff99",
    "#636363", "#fb9a99", "#b2df8a", "#1f78b4", "#33a02c", "#e31a1c",
    "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99",
    "#fdbf6f", "#cab2d6", "#ffff99", "#636363", "#fb9a99")

facet_by_age <-
  ggplot(data = asthma2, aes(x = County, y = County_prevalence, fill = County)) +
  geom_bar(stat = "identity") +
  labs(x = "Age Group", y = "Asthma Prevalence", fill = "Age Group") +
  ggtitle("Asthma Prevalence by Age Group and County") +
  facet_wrap( ~ Group, scales = "fixed") +
  scale_fill_manual(values=custom_palette)


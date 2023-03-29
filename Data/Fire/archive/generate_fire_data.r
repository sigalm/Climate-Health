#### Placeholder fire data 2.0 #####


# Load the xml2 package
library(xml2)

# Specify the file path of the XML file
file_path <- "Data/Fire/BP_CA.tif.xml"

# Read the XML file
xml_data <- read_xml(file_path)


xmlView(xml_data)

# Extract the data from the XML into a data frame
df <- xml_data %>%
  xml_find_all("//element_name") %>%
  xml_attrs() %>%
  bind_rows()

# Print the resulting data frame
print(df)



########### OPTION 2 ############




counties <- read.csv("Data/Microdata/counties.csv")

p.fire <- 0.10                   # probability that a wildfire occurs 
rr.fire.urban <- 0.25            # risk ratio for fire in urban area versus rural
rr.fireOverTime <- 1.05          # additional fire risk each consecutive year

# Get year over year fire data per neighborhood (or another geographic granularity) and air quality data - since no data yet, create matrix
n_t <-  100
m.fire <- matrix(nrow=nrow(counties), ncol=n_t, dimnames = list(paste(counties$FIPS), paste("cycle",1:n_t)))

for (t in 1:n_t) {
  p.fire.tmp <- min(p.fire*rr.fireOverTime^(t-1), 1)                     # calculate fire risk for year t

  for (k in 1:nrow(counties)) {
    currentCounty <- counties$FIPS[k]                           # get neighborhood name
    m.fire[k, t] <- rbinom(1,size=1,prob=(
      (counties$Designation[k]=="rural")*p.fire.tmp +
        (counties$Designation[k]!="rural")*min(p.fire.tmp*rr.fire.urban,1)))        # calculate whether fire happened given fire risk and urban/rural designation
    
  }}

saveRDS(m.fire, "Data/Fire/fire_data_28apr2023.rds")

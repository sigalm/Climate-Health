library(tidycensus)
census_api_key("4f091e4a30a602d2e089ea476e70d4aa8fd595ce")


# Download the data from the ACS API
data <- get_acs(geography = "state", variables = "A02002", survey = "acs/acs1", year = 2021, state = "CA")

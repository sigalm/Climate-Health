# Calculate all-cause medical costs

# Median costs among those with asthma, per 4 months
# (Sullivan et al 2016, table 2: https://www.tandfonline.com/doi/full/10.1080/02770903.2016.1194430)

annual_change_mcpi_2011_2023 <- c(3.0,
                                  3.7,
                                  2.5,
                                  2.4,
                                  2.6,
                                  3.8,
                                  2.5,
                                  2.0,
                                  2.8,
                                  4.1,
                                  1.2,
                                  4.1)

# Above data from data.bls.gov (query 12-month averages between 2011 and 2023)

avg_mcpi <- mean(annual_change_mcpi_2011_2023)/100

c01 <- 1225
c12 <- 1385
c24 <- 1686 * 759 / (759+191) + 1926 * 191 / (759+191)
c45 <- 2748
c56 <- 5359

costs2011 <- c(c01,c12,c24,c45,c56)
costs2023 <- (costs2011 * (1 + avg_mcpi)^(2023-2011)) / (4 * 4.3)

# 4.3 weeks in a month on average

c0 <- c01

# assumes well-controlled has no asthma-related additional costs.  


# See also:
# Dunn et al 2018: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5785315/
# For a review of different inflation indexes and why M-CPI might not be the best to use.
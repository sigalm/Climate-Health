# Health state utility extrapolation

# utility data from Oh et al 2022 meta-analysis

control_cat <- c(1,2,3,4,5)

utilities <- c(0.93, 0.87, 0.69)  # this now uses the EQ-5D-5L measurements, table 3
x_val <- c(    1,    3,    5)

dat <- as.data.frame(cbind(x_val, utilities))


library(ggplot2)
ggplot(dat, aes(x_val, utilities)) +
  geom_point() + 
  geom_smooth(method="lm")

lm <- lm(utilities ~ x_val, data=dat)
summary(lm)

c_vals <- coefficients(lm)

utilities_5 <- c_vals[1] + c_vals[2]*control_cat

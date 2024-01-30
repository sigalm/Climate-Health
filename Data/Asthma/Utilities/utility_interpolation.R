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


# Extrapolate decrement in utility in weeks 1-6 post-exacerbation
# Data from Briggs et al (see Jim e-mail) table 2 (EQ=5D utilities)
dec_data <- data.frame(time_since_exacerbation = c(7, 14, 21, 28, 49),
                   utility_decrement = c(-0.163, -0.132, -0.125, -0.115, 0))

lm_model <- lm(utility_decrement ~ time_since_exacerbation, data = dec_data)

new_time_points <- c(35, 42)  # Add the time points you want to extrapolate to
predicted_values <- predict(lm_model, newdata = data.frame(time_since_exacerbation = new_time_points))

extrapolation_result <- data.frame(time_since_exacerbation = new_time_points, predicted_values)
print(extrapolation_result)


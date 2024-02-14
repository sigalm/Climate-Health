### Prepare results


library(patchwork)
patch <- fig_no_resid + figure_0.1_resid + plot_layout(ncol= 2, guides = "collect") + plot_annotation(tag_levels = "A") & 
  theme(legend.position = "bottom")


# Assuming matrices are named sim_no_fire$m_asthma_states and sim_fire_0.1_resid$m_asthma_states
matrix_no_fire <- sim_no_fire$m_asthma_states
matrix_fire <- sim_fire_0.1_resid$m_asthma_states

# Extract columns for cycle 0 and cycle 2 for each scenario
health_states_cycle0_no_fire <- matrix_no_fire[, 5]
health_states_cycle2_no_fire <- matrix_no_fire[, 7]

health_states_cycle0_fire <- matrix_fire[, 5]
health_states_cycle2_fire <- matrix_fire[, 7]

# Calculate health state differences for each scenario
health_state_diff_no_fire <- as.numeric(health_states_cycle2_no_fire) - as.numeric(health_states_cycle0_no_fire)
health_state_diff_fire <- as.numeric(health_states_cycle2_fire) - as.numeric(health_states_cycle0_fire)

# Calculate percentage of individuals with poorer health state in cycle 2 for each scenario
percentage_poorer_no_fire <- mean(health_state_diff_no_fire > 0) * 100
percentage_poorer_fire <- mean(health_state_diff_fire > 0) * 100

# Print the results
cat("Percentage of individuals with poorer health state in cycle 2 (Scenario - No Fire):", percentage_poorer_no_fire, "%\n")
cat("Percentage of individuals with poorer health state in cycle 2 (Scenario - Fire):", percentage_poorer_fire, "%\n")

percentage_poorer_fire / percentage_poorer_no_fire
# this shows that the probability of worse outcomes are increased by 33%

5000 * (percentage_poorer_fire - percentage_poorer_no_fire) / 100



sim_no_fire$TR_proportion
colMeans(sim_no_fire$TR_proportion[-1, ])
colMeans(sim_no_fire$TR_healthcare_use[-1, ])
sum(sim_no_fire$te)
sim_no_fire$te_hat

#10 weeks in perfect health would be:
5000 *1 * 10/52


sim_fire_0.1_resid$TR_proportion[3 , ] / sim_no_fire$TR_proportion[3, ]
rbind(sim_no_fire$TR_proportion[3 , ],sim_fire_0.1_resid$TR_proportion[3, ] )



sum(sim_fire_0.1_resid$te)
sim_fire_0.1_resid$te_hat


# Costs
sum(sim_no_fire$tc)
sim_no_fire$tc_hat

sum(sim_fire_0.1_resid$tc)
sim_fire_0.1_resid$tc_hat


# Plot costs
scenarios <- c("No Wildfire", "Wildfire")
total_costs <- c(sum(sim_no_fire$tc), sum(sim_fire_0.1_resid$tc))
data <- data.frame(Scenario = scenarios, Costs = total_costs)

ggplot(data, aes(x = Scenario, y = Costs, fill = Scenario)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = scales::dollar(Costs), y = Costs),
            vjust = -0.5, size = 3) +
  labs(title = "Total All-Cause Medical Costs, 10 weeks",
       y = "Cost",
       x = "") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = " M", decimal.mark = ".", big.mark = ",")) +
  theme_minimal() +
  theme(legend.position = "none")

total_qalys <- c(sum(sim_no_fire$te), sum(sim_fire_0.1_resid$te))
data <- data.frame(Scenario = scenarios, QALYs = total_qalys)

ggplot(data, aes(x = Scenario, y = QALYs, fill = Scenario)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = sprintf("%.1f", QALYs), y = QALYs),
            vjust = -0.5, size = 3) +
  labs(title = "Total Quality-Adjusted Life Years, 10 weeks",
       y = "Quality-Adjusted Life Years",
       x = "") +
  theme_few() +
  theme(legend.position = "none")

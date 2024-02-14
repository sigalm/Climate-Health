## Results

library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(knitr)
library(gridExtra)
library(patchwork)

# Generate figures for simulation trace

fig_no_fire <- make_figures(sim_no_fire, "No wildfire")
figure_w_fire <- make_figures(sim_fire_0.1_resid, "Wildfire", 1)

combined_fig <- fig_no_fire + figure_w_fire + plot_layout(ncol=2, guides = "collect") & 
  theme(legend.position = "bottom", legend.text = element_text(size = 12), axis.text = element_text(size = 10))
# combined_fig <- combined_fig + plot_layout(tag_level = 'new') +
#   plot_annotation(tag_levels = list(c('No Wildfire', 'Wildfire')))


no_fire_healthcare_use <- sim_no_fire$m_asthma_healthcare_use

hcu_result <- as.data.frame(no_fire_healthcare_use[ ,-1])

hcu_result <- hcu_result %>%
  mutate_at(vars(starts_with("cycle")), ~factor(., levels = v_healthcare_use))

percentage <- hcu_result %>%
  select(starts_with("cycle")) %>%
  summarise(across(everything(), ~ prop.table(table(.)) * 100))

mean_hcu_no_fire <- rowMeans(percentage)

prop.table(table(sim_fire_0.1_resid$m_asthma_healthcare_use[ ,7]))

colMeans(sim_no_fire$TR_proportion)

te16 <- (sim_no_fire$m_qalys[ ,5:20] * cycle_length) 
sum(te16)/5000

5000 * 16/52  - sum(te16)
((sim_fire_0.1_resid$TR_proportion[7, ] / colMeans(sim_no_fire$TR_proportion))) -1


te16_fire <- (sim_fire_0.1_resid$m_qalys[ ,5:20] * cycle_length) 
sum(te16_fire)/5000


(sum(te16)/5000) / (sum(te16_fire)/5000)
(sum(te16)/5000) - (sum(te16_fire)/5000)

((sum(te16_fire)/5000) - (sum(te16)/5000)) / (sum(te16)/5000)
sum(te16) - sum(te16_fire)



sum(sim_no_fire$m_costs[ ,5:20]) / 5000
sum(sim_fire_0.1_resid$m_costs[ ,5:20]) / 5000


sum(sim_fire_0.1_resid$m_costs[ ,5:20]) / sum(sim_no_fire$m_costs[ ,5:20])

sum(sim_fire_0.1_resid$m_costs[ ,5:20]) - sum(sim_no_fire$m_costs[ ,5:20])



# Average days of exposure
indivs <- asthma_fire_sample[ ,c("id", "countyfip")]
smoke_sum <- cbind(smoke_data, Total = rowSums(smoke_data[ ,-ncol(smoke_data)]))
indiv_smoke <- merge(indivs, smoke_sum[ ,c("countyfip","Total")], by="countyfip")
sum(indiv_smoke$Total)



# Repeat sims for 10 different seeds to ensure we're capturing enough of the uncertainty.

test_table <- data.frame(seed=rep(NA, 10), util_no_fire=rep(NA,10), util_fire=rep(NA,10))
seeds <- round(runif(10, min = 10000, max=99999))

for (i in 5:10) {
  
  sim_no_fire <-MicroSim(n_i, n_t, 
                         smoke_data = smoke_data_0,
                         v_asthma_state_names, 
                         asthma_fire_sample, 
                         risk_modifiers,
                         cycle_length, 
                         baseline_birth_rate, annual_birth_rate_change, 
                         annual_allcause_mortality_change,
                         v_asthma_therapies,
                         m_asthma_therapy_probs, 
                         v_asthma_costs,
                         v_intervention_costs,
                         intervention_coverage = 0, 
                         intervention_trigger = 0, 
                         discount_rate_costs,
                         discount_rate_qalys,
                         min_residual = 0,
                         seed = seeds[i],
                         record_run = FALSE,
                         description="Asthma Sim No Fire 0% Min Residual Probability")
  
  sim_fire <- MicroSim(n_i, n_t, 
                       smoke_data = smoke_data,
                       v_asthma_state_names, 
                       pop_sample = asthma_fire_sample, 
                       risk_modifiers,
                       cycle_length, 
                       baseline_birth_rate, annual_birth_rate_change, 
                       annual_allcause_mortality_change,
                       v_asthma_therapies,
                       m_asthma_therapy_probs, 
                       v_asthma_costs,
                       v_intervention_costs,
                       intervention_coverage = 0, 
                       intervention_trigger = 0, 
                       discount_rate_costs,
                       discount_rate_qalys,
                       min_residual = 0.1,
                       seed = seeds[i],
                       record_run = FALSE,
                       description = "Asthma Sim With Fire and Lag 50% Min Resid")
  
  
  
  
  util_fire <- sum(sim_fire$m_qalys[ ,5:10] * cycle_length)
  util_no_fire <- sum(sim_no_fire$m_qalys[ ,5:10] * cycle_length)
  
  test_table[i, ] <- c(seeds[i], util_no_fire, util_fire)
  
}


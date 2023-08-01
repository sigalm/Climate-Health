# Review results -- 2023.07.18

no_fire_healthcare_use <- sim_no_fire$m_asthma_healthcare_use

hcu_result <- as.data.frame(no_fire_healthcare_use[ ,-1])

hcu_result <- hcu_result %>%
  mutate_at(vars(starts_with("cycle")), ~factor(., levels = v_healthcare_use))

percentage <- hcu_result %>%
  select(starts_with("cycle")) %>%
  summarise(across(everything(), ~ prop.table(table(.)) * 100))

mean_hcu_no_fire <- rowMeans(percentage)

saveRDS(mean_hcu_no_fire, "Simulation Results/20230718/mean_hcu_no_fire.rds")



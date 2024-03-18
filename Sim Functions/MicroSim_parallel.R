MicroSim_parallel <- function(n_i,
                     n_t,
                     smoke_data,
                     v_asthma_state_names,
                     pop_sample,
                     risk_modifiers, 
                     cycle_length,
                     baseline_birth_rate,
                     annual_birth_rate_change, 
                     annual_allcause_mortality_change,
                     v_asthma_therapies,
                     m_asthma_therapy_probs, 
                     v_asthma_costs,
                     v_intervention_costs,
                     intervention_coverage, 
                     intervention_trigger, 
                     discount_rate_costs,
                     discount_rate_qalys,
                     cores,
                     min_residual = 0,
                     seed = 1, 
                     record_run = TRUE,
                     description = "This is an asthma simulation"
) {
  
  # ===============SET UP LOGS/RECORDS ==================
  start_time <- Sys.time()
  
  timestamp <- format(start_time, "%Y%m%d_%H%M")
  log_file_name <- paste0("Runs/log_", timestamp, ".txt")
  log_file <- file(log_file_name, open = "a")
  
  metadata <- NULL
  if (record_run) {
    metadata <- list(
      parameters = list(
        n_i = n_i,
        n_t = n_t,
        smoke_data = smoke_data,
        v_asthma_state_names = v_asthma_state_names,
        pop_sample = pop_sample,
        risk_modifiers = risk_modifiers,
        cycle_length = cycle_length,
        baseline_birth_rate = baseline_birth_rate,
        annual_birth_rate_change = annual_birth_rate_change,
        annual_allcause_mortality_change = annual_allcause_mortality_change,
        v_asthma_therapies = v_asthma_therapies,
        m_asthma_therapy_probs = m_asthma_therapy_probs,
        v_asthma_costs = v_asthma_costs,
        v_intervention_costs = v_intervention_costs,
        intervention_coverage = intervention_coverage,
        intervention_trigger = intervention_trigger,
        discount_rate_costs = discount_rate_costs,
        discount_rate_qalys = discount_rate_qalys,
        seed = seed,
        min_residual = min_residual,
        record_run = record_run,
        description = description
      )
    )
    
  }
  
  log_output(100, "...INITIALIZING...", log_file)
  
  # =================INITIALIZE================
  
  v_discount_weights_costs <- 1 / (1 + discount_rate_costs) ^ (0:n_t)            # calculate discount weights for costs per cycle
  v_discount_weights_qalys <- 1 / (1 + discount_rate_qalys) ^ (0:n_t)            # calculate discount weights for QALYs per cycle
  
  v_birth_rates <- baseline_birth_rate * (1+annual_birth_rate_change)^(0:n_t)    # calculate birth rates per cycle
  v_death_rate_adjusters <- (1+annual_allcause_mortality_change)^(0:n_t)         # calculate death rate adjuster per cycle
  
  # initialize matrices to capture fires, health states, costs, and health outcomes each cycle:
  
  m_fire <- smoke_data
  
  m_asthma_states <- 
    m_intervention_receipt <- 
    m_asthma_therapies <-
    m_asthma_healthcare_use <-
    m_costs <- 
    m_qalys <- 
    matrix(nrow=n_i, ncol=n_t+1, dimnames = list(paste("ind", 1:n_i), 
                                                 paste("cycle",0:n_t))) 
  m_intervention_receipt[ ,1] <- 0                      # initial intervention: none
  m_asthma_therapies[ ,1] <- pop_sample$asthma_therapy  # initial asthma management: pulled from individual data
  m_asthma_states[ ,1] <- pop_sample$asthma_status      # initial health states: pulled from data
  m_asthma_healthcare_use[ ,1] <- "none"
  
  time_since_dr_visit <- rep(0, n_i)                    # create vector to count cycles between doctor visits
  time_rf <-  rep(0, n_i)                               # create vector to track "risk" of improvement after severe exacerbation (poorly controlled or uncontrolled)
  
  m_costs[ ,1] <- v_asthma_costs[match(m_asthma_states[ ,1], v_asthma_state_names)] 
  m_qalys[ ,1] <- Effs(m_asthma_states[ , 1], time_rf)
  
  
  v_total_pop  <- rep(NA, n_t+1)                        # vector to track population size over each cycle
  v_total_pop[1] <- n_i
  v_total_pop_alive <- v_total_pop
  
  log_output(100, 
             paste0("Starting simulation for n_i = ", n_i, 
                    " and n_t = ", n_t, "."), log_file)
  
  cluster <- makeCluster(cores)
  registerDoParallel(cluster)
  
  # ================= SIMULATION LOOP START ================
  for (t in 1:n_t) {                                                             # start loop for time cycles
    
    total_births_t <- 0                                                          # initialize variable to keep track of number of births (define this in the loop because we want it to reset each cycle)

    time_rf <- time_rf - (time_rf>0)                                             # reduce time risk factor by 1 if not zero
    
    log_output(3, sprintf("Simulating cycle t=%s/%s", t, n_t), log_file)
    
    results_t <- foreach(i = 1:n_i, .combine = 'rbind', .packages = c('dplyr','matrixStats'),
                         .export = c("Probs","Costs","Effs","log_output",
                                     "v_healthcare_use",
                                     "m_asthma_healthcare_use_probs_nofire",
                                     "m_asthma_healthcare_use_probs_fireadj",
                                     "v_asthma_hsu","v_hsu_decrements",
                                     "max_dur_decrement",
                                     "v_asthma_costs"),
                         .verbose = FALSE) %dopar% {
      
      set.seed(seed+i*79+t*71)                                                   # set seed for every individual
      
      # determine if individual gets intervention
      # exposed to smoke in the past week, they MAY receive intervention 
      # (based on intervention_coverage)
      
      fire_it <- as.integer(m_fire[m_fire$countyfip==pop_sample[i, "countyfip"], t+1])
      fire_it_2wk <- as.integer(m_fire[m_fire$countyfip==pop_sample[i, "countyfip"], t]) + fire_it
      
      # THIS NEEDS TO BE FIXED
      if (intervention_coverage>0 & 
          (fire_it == 1)) {
        # m_intervention_receipt[i,t+1] <- 
        intervention_receipt_tplus1 <- 
          rbinom(n=1, size=1, prob=intervention_coverage)
      } else {
        # m_intervention_receipt[i,t+1] <- 
        intervention_receipt_tplus1 <- 0
      }
      
      # calculate the transition probability for individual i at cycle t
      # First use the Probs function to calculate the transition probabilities 
      # into each "to" state for individual i, given their individual 
      # characteristics/risk factors and the health state they're coming from
      # Then sample the new health state using those probabilities.
      
      v_probs <- Probs(M_it = m_asthma_states[i,t],
                       risk_modifiers = risk_modifiers,
                       v_asthma_state_names = v_asthma_state_names,
                       x_i = pop_sample[i, ], 
                       fire_it_2wk = fire_it_2wk,
                       intervention_coverage_it = m_intervention_receipt[i,t],
                       death_rate_t = v_death_rate_adjusters[t],
                       min_residual = min_residual,
                       record_run = record_run)
      log_output(1, sprintf("   Probs calculated i=%s in" , i), log_file)
      
      # m_asthma_states[i, t+1] <- 
      asthma_states_tplus1 <- 
        sample(v_asthma_state_names, prob=v_probs, size=1)                      
      
      
      # Sample if and what acute health resources will be used given new health state  
      if (fire_it == 0) {
       # m_asthma_healthcare_use[i, t+1] <-
        asthma_healthcare_use_tplus1 <- 
          sample(v_healthcare_use, size = 1,
                 prob = m_asthma_healthcare_use_probs_nofire[asthma_states_tplus1, ])
      } else {
        # m_asthma_healthcare_use[i, t+1] <-
        asthma_healthcare_use_tplus1 <- 
          sample(v_healthcare_use, size = 1,
                 prob = m_asthma_healthcare_use_probs_fireadj[asthma_states_tplus1, ])
      }
      
      # Determine new therapy if therapy changed
      worse_control <- as.integer(asthma_states_tplus1) > 
        as.integer(asthma_states_tplus1)                                        # assess if asthma control status got worse
      time_since_dr_visit_it <- time_since_dr_visit[i] + 1                       # increase time since last doctor visit by one cycle
      
      if (worse_control | time_since_dr_visit_it>=2) {                                 
        asthma_therapies_tplus1 <- 
          sample(v_asthma_therapies, size=1, 
                 prob=m_asthma_therapy_probs[asthma_states_tplus1, ])          # determine therapy for next cycle (given prob of each therapy based on new health state)
        time_since_dr_visit_it <- 0                                              # reset dr_visits counter
      } else {                                                                   # otherwise keep using the same therapy
        asthma_therapies_tplus1 <- m_asthma_therapies[i,t]
      }
      
      
      # Start countdown from severe exacerbation
      severe_exacerbation <- worse_control & (asthma_states_tplus1 == 4 | asthma_states_tplus1 == 5)
      if (severe_exacerbation) {
        time_rf_it <- 7                                                       # note: max effect is 6 weeks. starting from 7 because will subtract 1 at the beginning of next cycle
      }  else {
        time_rf_it <- time_rf[i]
      }
      
      # Assign health state utility
      qalys_tplus1 <- Effs(M_it = m_asthma_states[i,t],
                              time_rf_it = time_rf_it) 
      
      
      # Assign cost
      costs_tplus1 <- Costs(M_it = m_asthma_states[i,t],
                               fire_it = fire_it)
      
                         
      
      
      # # if individual is not Dead, determine # of kids born in year t
      # if (m_asthma_states[i,t+1] != "50" & m_asthma_states[i, t+1] != "100") {
      #   set.seed(seed+i*79+t*71) 
      #   kids_it <- rpois(1, v_birth_rates[t])                                    # determine if individual i had children and how many using a 
      #   # Poisson distribution with probability being the birth rate 
      #   # at time t
      #   
      #   # if individual i had children, assign characteristics to child 
      #   # and add new row to pop_sample
      #   
      #   if (kids_it > 0) {
      #     log_output(record_run, sprintf("Individual=%s had a kid at t=%s", i, t))
      #     for (kid in 1:kids_it) {
      #       new_x <- c(as.numeric(pop_sample[nrow(pop_sample), 'id'] + 1), # get last ID in population and add 1
      #                  as.numeric(0),                                                     # age is 0
      #                  as.integer(rbinom(1,1,0.55)),                                      # sex (0=male, 1=female) based on binomial distribution with p=0.55 for female
      #                  as.logical(ifelse(m_fire[m_fire$countyfip==pop_sample[i, "countyfip"], t]==1, TRUE, FALSE)), # exposure is TRUE if fire happened that year
      #                  as.integer(pop_sample$rural[i]),                        # same rural/urban designation as parent
      #                  as.character("NoTx"),                                              # define asthma care (none because no asthma at birth)
      #                  as.numeric(pop_sample$countyfip[i]),                 # same neighborhood as parent
      #                  as.integer(pop_sample$fam_id[i]))                       # same family as parent
      #       pop_sample <- rbind(pop_sample, new_x)                  # add the above as new row to pop_sample, matrix of individuals and risk factors
      #       
      #       
      #       # assign a health state, initial costs, and initial QALYs 
      #       # to the newborn
      #       
      #       new_M <- new_I <- new_T <-new_C <- new_E <- rep(NA,n_t+1)
      #       new_M[t+1] <- 0                                                      # Add initial health state of new child to m_asthma_states (age of onset is 5, so no asthma when born)
      #       m_asthma_states <- rbind(m_asthma_states, new_M)
      #       new_C[t+1] <- Costs(M_it=new_M[t+1], 
      #                           intervention_coverage = intervention_coverage)
      #       m_costs <- rbind(m_costs, new_C)
      #       new_E[t+1] <- Effs(M_it=new_M[t+1], x_i=new_x)
      #       m_qalys <- rbind(m_qalys, new_E)
      #       m_intervention_receipt <- rbind(m_intervention_receipt, new_I)
      #       m_asthma_therapies <- rbind(m_asthma_therapies, new_T) 
      #     }
      #   }  
      #   total_births_t <- total_births_t + kids_it                               # calculate total number of births in cycle t
      # } # close if statement for kids
      
      result <- data.frame(
        intervention_receipt_tplus1 = as.numeric(intervention_receipt_tplus1), 
        asthma_states_tplus1 = asthma_states_tplus1,
        asthma_healthcare_use_tplus1 = asthma_healthcare_use_tplus1,
        asthma_therapies_tplus1 = asthma_therapies_tplus1,
        qalys_tplus1 = as.numeric(qalys_tplus1),
        costs_tplus1 = as.numeric(costs_tplus1),
        time_since_dr_visit_it = as.numeric(time_since_dr_visit_it),
        time_rf_it = as.numeric(time_rf_it))
      
    }  # close loop for individuals
    
    m_intervention_receipt[ ,t+1] <- results_t$intervention_receipt_tplus1
    m_asthma_states[ ,t+1] <- results_t$asthma_states_tplus1
    m_asthma_healthcare_use[ ,t+1] <- results_t$asthma_healthcare_use_tplus1
    m_asthma_therapies[ ,t+1] <- results_t$asthma_therapies_tplus1
    m_qalys[ ,t+1] <- results_t$qalys_tplus1
    m_costs[ ,t+1] <- results_t$costs_tplus1
    time_since_dr_visit <- results_t$time_since_dr_visit_it
    time_rf <- as.numeric(results_t$time_rf_it)
    
    # Increase age by cycle length
    pop_sample$age <- 
      pop_sample$age + cycle_length          
    
    n_i <- n_i + total_births_t                                                  # increase n_i for next cycle by the total number of births
    v_total_pop[t+1] <- n_i                                                      # add the new population size to the population size tracking vector
    n_deaths <- sum(m_asthma_states[ ,t+1]=="50" | m_asthma_states[ ,t+1]=="100") # count number who died in cycle t
    v_total_pop_alive[t+1] <- n_i - n_deaths
    
    
    
  }    # close loop for cycles
  
  stopCluster(cluster)
  
  # ================= SIMULATION LOOP END ================
  
  
  log_output(100, "Simulation complete. Preparing results.", log_file)
  
  
  tc <- m_costs %*% v_discount_weights_costs                                     # total discounted costs per individual
  te <- (m_qalys * cycle_length) %*% v_discount_weights_qalys                    # total discounted QALYs per individual
  tc_hat <- mean(tc, na.rm=TRUE)                                                 # average discounted costs
  te_hat <- mean(te, na.rm=TRUE)                                                 # average discounted QALYS
  
  
  # create simulation trace (both absolute and percentage)
  
  TR_absolute <- t(apply(m_asthma_states, 2, function(x) 
    table(factor(x, levels = v_asthma_state_names, ordered = TRUE))))
  TR_proportion <- TR_absolute/v_total_pop 
  colnames(TR_absolute) <- v_asthma_state_names
  rownames(TR_absolute) <- paste("cycle", 0:n_t)
  colnames(TR_proportion) <- v_asthma_state_names
  rownames(TR_proportion) <- paste("cycle", 0:n_t)
  
  TR_healthcare_use <- t(apply(m_asthma_healthcare_use, 2, function(x) 
    table(factor(x, levels = v_healthcare_use, ordered = TRUE)))) / v_total_pop_alive
  colnames(TR_healthcare_use) <- v_healthcare_use
  rownames(TR_healthcare_use) <- paste("cycle", 0:n_t)
  
  
  # return outputs as a list
  
  results <- list(
    m_asthma_states = m_asthma_states,
    m_intervention_receipt = m_intervention_receipt,
    m_asthma_therapies = m_asthma_therapies,
    m_asthma_healthcare_use = m_asthma_healthcare_use,
    m_costs = m_costs,
    m_qalys = m_qalys,
    tc = tc,
    te = te,
    tc_hat = tc_hat,
    te_hat = te_hat,
    pop_sample_end = pop_sample, 
    TR_absolute = TR_absolute,
    TR_proportion = TR_proportion,
    TR_healthcare_use = TR_healthcare_use
  )
  
  
  end_time <- Sys.time()
  
  log_output(100, 
             paste0("Results saved. Total run time: ", 
                    difftime(end_time, start_time)), log_file)
  
  if (!is.null(log_file)) {
    close(log_file)
  }
  
  if (record_run) {
    results[["metadata"]] <- metadata
    timestamp <- format(start_time, "%Y%m%d_%H%M")
    result_file_path <- paste0("runs/results_", timestamp, ".RData")
    save(results, file = result_file_path)
    
    add_results_to_simulation_list(result_file_path, description, n_i, n_t)
  }
  
  
  return(results)
  
}

reRunMicroSim <- function(results_file) {
  # Read the metadata from the file
  load(results_file)
  
  # Extract the parameters from metadata
  parameters <- results$metadata$parameters
  
  # Call MicroSim function with extracted parameters
  MicroSim(
    n_i = parameters$n_i,
    n_t = parameters$n_t,
    smoke_data = parameters$smoke_data,
    v_asthma_state_names = parameters$v_asthma_state_names,
    pop_sample = parameters$pop_sample,
    risk_modifiers = parameters$risk_modifiers,
    cycle_length = parameters$cycle_length,
    baseline_birth_rate = parameters$baseline_birth_rate,
    annual_birth_rate_change = parameters$annual_birth_rate_change,
    annual_allcause_mortality_change = parameters$annual_allcause_mortality_change,
    v_asthma_therapies = parameters$v_asthma_therapies,
    m_asthma_therapy_probs = parameters$m_asthma_therapy_probs,
    v_asthma_costs = parameters$v_asthma_costs,
    v_intervention_costs = parameters$v_intervention_costs,
    intervention_coverage = parameters$intervention_coverage,
    intervention_trigger = parameters$intervention_trigger,
    discount_rate_costs = parameters$discount_rate_costs,
    discount_rate_qalys = parameters$discount_rate_qalys,
    seed = parameters$seed,
    min_residual = parameters$min_residual,
    record_run = parameters$record_run,
    description = parameters$description
  )
}




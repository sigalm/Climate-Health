MicroSim <- function(n_i,
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
    matrix(nrow=n_i, ncol=n_t+1, dimnames = list(paste("ind", 1:n_i), 
                                                 paste("cycle",0:n_t))) 
  
  m_intervention_receipt[ ,1] <- 0      # initial intervention: none
  
  m_asthma_therapies[ ,1] <- pop_sample$asthma_therapy  # initial asthma management: pulled from individual data
  
  m_asthma_states[ ,1] <- pop_sample$asthma_status      # initial health states: pulled from data
  
  m_asthma_healthcare_use[ ,1] <- "none"
  
  # initialize treatment tracking table (keep track of how long people have been on current therapy, if it has been successful in the past)
  temp <- expand.grid(pop_sample$id, v_asthma_therapies)
  tx_tracker <- data.frame(id=temp$Var1, tx=temp$Var2, tx_start=NA, tx_end=NA, tx_success=NA, tx_count=0)
  
  
  # assign start time to each individual for the therapy at initialization

  for (i in 1:n_i) {
    tx <-  pop_sample[i, "asthma_therapy"]                                # find what therapy they're on
    tx_tracker[tx_tracker$id==i & tx_tracker$tx==tx, ]$tx_start <- 0             # note start time 0 for that individual-treatment row
  }
  
  v_total_pop  <- rep(NA, n_t+1)                                                 # vector to track population size over each cycle
  v_total_pop[1] <- n_i
  
  log_output(100, 
             paste0("Starting simulation for n_i = ", n_i, 
                    " and n_t = ", n_t, "."), log_file)
  
  # ================= SIMULATION LOOP START ================
  for (t in 1:n_t) {                                                             # start loop for time cycles
    
    total_births_t <- 0                                                          # initialize variable to keep track of number of births (define this in the loop because we want it to reset each cycle)
    
    log_output(3, sprintf("Simulating cycle t=%s/%s", t, n_t), log_file)
    for (i in 1:n_i) {
      
      set.seed(seed+i*79+t*71)                                                   # set seed for every individual
      
      # determine if individual gets intervention
      # exposed to smoke in the past week, they MAY receive intervention 
      # (based on intervention_coverage)
      
      fire_it <- as.integer(m_fire[m_fire$countyfip==pop_sample[i, "countyfip"], t+1])
      fire_it_lag1 <- as.integer(m_fire[m_fire$countyfip==pop_sample[i, "countyfip"], t]) + fire_it
      
      if (intervention_coverage>0 & 
          (fire_it == 1)) {
        m_intervention_receipt[i,t+1] <- 
          rbinom(n=1, size=1, prob=intervention_coverage)
      } else {
        m_intervention_receipt[i,t+1] <- 0
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
                       fire_it = fire_it, 
                       fire_it_lag1 = fire_it_lag1,
                       intervention_coverage_it = m_intervention_receipt[i,t],
                       death_rate_t = v_death_rate_adjusters[t],
                       min_residual = min_residual,
                       record_run = record_run)
      log_output(1, sprintf("   Probs calculated i=%s in" , i), log_file)
      
      
      set.seed(seed+i*79+t*71) 
      m_asthma_states[i, t+1] <- 
        sample(v_asthma_state_names, prob=v_probs, size=1)                       # sample the next health state given adjusted probabilities
      
      
      
      m_asthma_healthcare_use[i, t+1] <- 
        sample(v_healthcare_use, size = 1,
               prob = m_asthma_healthcare_use_probs[m_asthma_states[i,t+1], ])
      
      
      # Determine new therapy if therapy changed
      set.seed(seed+i*79+t*71) 
      worse_control <- as.integer(m_asthma_states[i,t+1]) > 
        as.integer(m_asthma_states[i, t])                                        # assess if asthma control status got worse
      last_dr_visit <-  (t+1) - 
        tx_tracker$tx_start[tx_tracker$id==i & 
                              tx_tracker$tx==m_asthma_therapies[i,t]]            # assess time since current therapy began
      
      # if you have poorer control of if you're due for a doctor's visit, 
      # you will be reevaluated for changing therapies
      
      if (worse_control | last_dr_visit>2) {                                 
        
        m_asthma_therapies[i, t+1] <- 
          sample(v_asthma_therapies, size=1, 
                 prob=m_asthma_therapy_probs[m_asthma_states[i,t+1], ])          # determine therapy for next cycle (given prob of each therapy based on new health state)
        
        tx_tracker$tx_end[tx_tracker$id==i & 
                            tx_tracker$tx==m_asthma_therapies[i,t]] <- t         # note tx end date for previous tx
        
        tx_tracker$tx_start[tx_tracker$id==i & 
                              tx_tracker$tx==m_asthma_therapies[i,t+1]] <- t     # note tx start date for new tx
        
        tx_tracker$tx_count[tx_tracker$id==i & 
                              tx_tracker$tx==m_asthma_therapies[i,t]] <- 
          tx_tracker$tx_count[tx_tracker$id==i & 
                                tx_tracker$tx==m_asthma_therapies[i,t]] + 1      # update lifetime count of that therapy
        
        # assess whether therapy was successful
        # if no change in control or poorer control, therapy NOT successful.
        # To be discussed.
        
        if (as.integer(m_asthma_states[i,t+1]) == 
            as.integer(m_asthma_states[i, t]) | worse_control) {                 
          tx_tracker$tx_success[tx_tracker$id==i & 
                                  tx_tracker$tx==m_asthma_therapies[i,t]] <- 0
        } else {
          tx_tracker$tx_success[tx_tracker$id==i & 
                                  tx_tracker$tx==m_asthma_therapies[i,t]] <- 1
        }
      } else {                                                                   # otherwise keep using the same therapy
        m_asthma_therapies[i, t+1] <- m_asthma_therapies[i,t]
      }

      pop_sample$age[i] <- 
        pop_sample$age[i] + cycle_length                              # increase age by cycle length
      
     
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
      
    }  # close loop for individuals
    
    n_i <- n_i + total_births_t                                                  # increase n_i for next cycle by the total number of births
    v_total_pop[t+1] <- n_i                                                      # add the new population size to the population size tracking vector
    
  
    
  }    # close loop for cycles
  
  # ================= SIMULATION LOOP END ================
  
  log_output(100, "Simulation complete. Preparing results.", log_file)
  
  
  m_costs <- sapply(as.data.frame(m_asthma_states), function(col) v_asthma_costs[col])
  # add intervention cost
  rownames(m_costs) <- paste("ind", 1:n_i)
  m_qalys <- sapply(as.data.frame(m_asthma_states), function(col) v_asthma_hsu[col])
  rownames(m_qalys) <- paste("ind", 1:n_i)
  
  
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
    table(factor(x, levels = v_healthcare_use, ordered = TRUE)))) / v_total_pop
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
    TR_healthcare_use = TR_healthcare_use,
    tx_tracker = tx_tracker
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




# This function generates a vector of adjusted transition probabilities for an 
# individual.
# It takes the "from" health state (i.e., the health state occupied at cycle t) 
# and individual risk factors to adjust the relevant ROW of the transition 
# matrix. 
# It does NOT assign the individual their next health state! 

Probs <- function(M_it,                           # health state occupied by individual i at cycle t
                  risk_modifiers,                 # 3D object containing effect sizes for risk factors
                  v_asthma_state_names,           # vector of asthma health state names
                  x_i,                            # individual characteristics of individual i (row i of m_individual characteristics)
                  fire_it,                        # fire experience of individual i at cycle t (binary)
                  intervention_coverage_it,       # intervention receipt of individual i at cycle t (binary)
                  death_rate_t,
                  logger = FALSE) {                 # death rate adjuster for cycle t

  # Setup
  from_state = as.character(M_it)
  v_probs_it <- risk_modifiers[from_state, ,"transition_prob"]
  names(v_probs_it) <- v_asthma_state_names
  
  
  # Create a matrix where each row contains the individual's risk factors 
  # for each health state
  risk_factors_matrix <- cbind(age_it <- 
                                 x_i$age >= risk_modifiers[from_state, 
                                                           v_asthma_state_names, 
                                                           "age_threshold"],
                               sex_it <- x_i$sex,
                               fire_it,
                               intervention_coverage_it)
  
  # Extract the risk ratios for the appropriate transition probabilities
  risk_ratios_matrix <- risk_modifiers[from_state, v_asthma_state_names, 3:6]
  
  # Calculate probability adjustment factor encompassing all risk factors
  v_prob_modifiers <- apply(
    risk_factors_matrix * risk_ratios_matrix + (1 - risk_factors_matrix), 
    1, prod)
  
  # Calculated adjusted probabilities vector
  v_probs_it <- pmin(v_probs_it * v_prob_modifiers, 1)
  
  
  # update the probability of all-cause mortality (changes over time) 
  # with the adjuster at time t
  
  v_probs_it['100'] = v_probs_it['100']*death_rate_t     
  
  # calculate the "stay put" probability as 1 minus the rest
  v_probs_it[from_state] = 1-sum(v_probs_it[-which(names(v_probs_it) == from_state)])            
  
  
  # test for errors
  
  if (any(is.na(v_probs_it))) {
    err_msg <- paste("NAs produced at cycle", t, "individual", i)
    log_output(logger, err_msg)
    stop(err_msg)
  } else if (any(v_probs_it < 0)) {
    err_msg <- paste("Negative probability calculated at cycle", t, "individual", i, ":", toString(v_probs_it))
    log_output(logger, err_msg)
    stop(err_msg)
  } else if (!isTRUE(all.equal(sum(v_probs_it), 1, tolerance = 1e-9))) {
    err_msg <- paste("Probabilities do not sum to 1 at cycle", t, "individual", i, ": ", toString(sum(v_probs_it)), 
                     "\nProbabilities: ", toString(v_probs_it))
    log_output(logger, err_msg)
    stop(err_msg)
  } else {
    return(v_probs_it)
  }
  

} 


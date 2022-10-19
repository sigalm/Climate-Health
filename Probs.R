# This function generates a vector of adjusted transition probabilities for an individual.
# It takes the "from" health state (i.e., the health state occupied at cycle t) and individual risk factors
# to adjust the relevant ROW of the transition matrix. 
# It does NOT assign the individual their next health state! 



Probs <- function(M_it,                      # health state occupied by individual i at cycle t
                  fire_it,                   # fire experience of individual i at cycle t (binary)
                  x_i,                       # individual characteristics of individual i (row i of m.x)
                  intervention_it,           # is the individual getting the intervention (binary)
                  deathRate_t) {             # death rate at cycle t 
  
  from_state = as.character(M_it)                           # Take the health state occupied by individual i at cycle t
  v.p.it <- riskmodifiers[from_state, ,"transition_prob"]   # Take the corresponding row from the transition probabilities matrix
  names(v.p.it) <- v.n_asthma                               # Name the vector items (each item is a health state the individual can transition to)
  
  # The following loop recalculates each item of the vector of probabilities based on individual risk factors
  
  if (from_state != "50" & from_state != "100") {  # unless the person is already in one of two absolute health states (dead) in which case no adjustment is needed and the probability of staying dead is 1
    
    for (to_state_ind in 1:length(v.n_asthma)){    # go through each item of the vector one by one
      to_state <- v.n_asthma[to_state_ind]         # get the name of the health state
      
      # create a vector with the individual's risk factors (all must be 1/0 logicals and in the same order as the risk ratios vector below)
      
      v.x <- c(x_i$age>=riskmodifiers[from_state, to_state, "age_threshold"],  
               x_i$sex, 
               fire_it, 
               intervention_it
               )
      
      
      # save the risk ratios for the appropriate transition probability in a vector (comes from the different sheets of the transition probs matrix excel)
      
      v.rr <- c(riskmodifiers[from_state, to_state, 3:6])
      
      # for all probabilities (except for the "stay put" probability), calculate an adjusted probability using the baseline probability and multiplying with an overall risk modifier
      
      if (from_state != to_state){
        v.p.it[to_state_ind] = min(v.p.it[to_state_ind] * modifyRisk(v.x, v.rr), 1)
      }
      
    }  # close for loop
    
    
    v.p.it['100'] = v.p.it['100']*deathRate_t     # update the probability of all-cause mortality (changes over time) with the adjuster at time t
    v.p.it[from_state] = 1-sum(v.p.it)            # calculate the "stay put" probability as 1 minus the rest
  } 
  
  
  # test for errors
  
  if (any(is.na(v.p.it==TRUE))) {
    print("NAs produced")
  } else if (any(v.p.it<0)) {
    print("Negative probability calculated")
    print(v.p.it)
  } else if (isTRUE(all.equal(sum(v.p.it), 1))) {
    return(v.p.it)
  } else {
    print("Probabilities do not sum to 1")
    print(sum(v.p.it))
    print(v.p.it)
    stop()
  } 
  
}  

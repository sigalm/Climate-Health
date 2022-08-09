Probs <- function(M_it,                      # health state occupied by individual i at cycle t
                  fire_it,                   # fire experience of individual i at cycle t (binary)
                  x_i,                       # individual characteristics of individual i (row i of m.x)
                  smoke.duration.cum_it,     # cumulative smoke exposure of individual i up to cycle t
                  deathRate_t) {             # death rate at cycle t 
  
  from_state = as.character(M_it)
  v.p.it <- riskmodifiers[from_state, ,"transition_prob"]
  names(v.p.it) <- v.n_asthma
  
  if (from_state != "50" & from_state != "100") {
    
    for (to_state_ind in 1:length(v.n_asthma)){
      to_state <- v.n_asthma[to_state_ind]
      
      if (from_state != to_state){
        v.p.it[to_state_ind] = modifyRisk(
          p0 = v.p.it[to_state_ind],
          x1 = fire_it,
          rr1 = riskmodifiers[from_state, to_state, "rr.fire"],
          x2 = x_i$sex,
          rr2 = riskmodifiers[from_state, to_state, "rr.sex"], 
          x3 = x_i$age>=riskmodifiers[from_state, to_state, "age_threshold"],
          rr3 = riskmodifiers[from_state, to_state, "rr.age"]
        )
      }
      
      
      
    }  # close for loop
    v.p.it['100'] = deathRate_t
    v.p.it[from_state] = 1-sum(v.p.it)
  }
  
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

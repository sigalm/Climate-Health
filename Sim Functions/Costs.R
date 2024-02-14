
Costs <- function(M_it,
                  fire_it) {
  
  
  cost_it <- v_asthma_costs[match(M_it, v_asthma_state_names)] 
  
  if (fire_it > 0) {
    cost_it <- cost_it*1.27
  }
  
  
  return(cost_it) 
  
}                     

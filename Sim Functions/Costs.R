
Costs <- function(v_asthma_costs, 
                  M_it, 
                  v_asthma_state_names, 
                  intervention_it, 
                  v_intervention_costs) {
  
  cost_it <- v_asthma_costs[match(M_it, v_asthma_state_names)] + (intervention_it * v_intervention_costs)
  
  return(cost_it) 
  
}                     

Costs <- function(M_it,                     # M_it: health state occupied by individual i at cycle t
                  intervention_it) {        # intervention: is this person receiving intervention (binary)
  
  
  c_it <- v.costs[match(M_it, v.n_asthma)] + (intervention_it * interventionCost)

  

  return(c_it)                                          # return the costs
}

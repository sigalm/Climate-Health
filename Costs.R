Costs <- function(M_it,                     # M_it: health state occupied by individual i at cycle t
                  intervention=FALSE) {     # intervention: is there an intervention?
  
  
  c_it <- v.costs[match(M_it, v.n_asthma)] + (intervention * interventionCost)

  
  # c.it[M_it] <-  c.H                             # cost of staying healthy for one cycle
  # c.it[M_it] <-  c.Act + c.intn*intervention   # cost of staying acutely injured for one cycle plus cost of intervention
  # c.it[M_it == "Prm"] <-  c.Prm + c.intn*intervention   # cost of staying permanently injured for one cycle plus cost of intervention
  # 
  return(c_it)                                          # return the costs
}

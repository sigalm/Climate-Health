Costs <- function(M_it,                     # M_it: health state occupied by individual i at cycle t
                  intervention=FALSE) {     # intervention: is there an intervention?
  
  
  c_it[M_it==0] <- v.costs[1]                                         
  c_it[M_it==1] <- v.costs[2]
  c_it[M_it==21] <- v.costs[3]
  c_it[M_it==22] <- v.costs[4]
  c_it[M_it==212] <- v.costs[5]
  c_it[M_it==223] <- v.costs[6]
  c_it[M_it==50] <- v.costs[7]
  c_it[M_it==100] <- v.costs[8]
  
  # c.it[M_it] <-  c.H                             # cost of staying healthy for one cycle
  # c.it[M_it] <-  c.Act + c.intn*intervention   # cost of staying acutely injured for one cycle plus cost of intervention
  # c.it[M_it == "Prm"] <-  c.Prm + c.intn*intervention   # cost of staying permanently injured for one cycle plus cost of intervention
  # 
  return(c_it)                                          # return the costs
}

Costs <- function(M_it,                     # M_it: health state occupied by individual i at cycle t
                  intervention=FALSE) {     # intervention: is there an intervention?
  
  
  c.it <- 0                                             # everyone starts with 0 costs
  c.it[M_it == "H"] <-  c.H                             # cost of staying healthy for one cycle
  c.it[M_it == "Act"] <-  c.Act + c.intn*intervention   # cost of staying acutely injured for one cycle plus cost of intervention
  c.it[M_it == "Prm"] <-  c.Prm + c.intn*intervention   # cost of staying permanently injured for one cycle plus cost of intervention
  
  return(c.it)                                          # return the costs
}

Effs <- function(M_it,                 # M_it: health state occupied by individual i at cycle t
                 intervention=FALSE,   # intervention: is there an intervention?
                 x_i=NULL,         # x_i: matrix of individual characteristics
                 cl=1) {               # cl: cycle length in years
  
  u.it <- 0                                                                # everyone starts with utility 0
  u.it[M_it == "H"] <- u.H                                                 # utility if healthy
  u.it[M_it == "Act"] <- intervention*u.intn + (1-intervention)*u.Act      # utility if acutely injured, conditional on intervention
  u.it[M_it == "Prm"] <- u.Prm                                             # utility if permanently injured
  
  QALYs <- u.it * cl        # calculate QALYs during cycle t
  return(QALYs)             # return QALYs
}


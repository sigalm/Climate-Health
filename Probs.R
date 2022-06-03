Probs <- function(M_it,                      # health state occupied by individual i at cycle t
                  fire_it,                   # fire experience of individual i at cycle t (binary)
                  x_i,                       # individual characteristics of individual i (row i of m.x)
                  smoke.duration.cum_it) {   # cumulative smoke exposure of individual i up to cycle t
  
  
  v.p.it <- rep(NA, n.s)   # initialize vector of state transition probabilities
  names(v.p.it) <- v.n     # name columns with the health states
  
  # update v.p.it with appropriate probabilities (note: v.n is H; Act; Prm; D)
  
  p.PrmD_it <- min(1, fire_it *
                     (1-exp(-r.PrmD * (1 + rp.smoke * smoke.duration.cum_it))))     # calculate p.PrmD conditional on cumulative exposure
  
  p.HAct_it <- min(1, p.HAct * fire_it * 
                     (x_i$sex * rr.HAct.f + (1-x_i$sex)) *          # risk modification for sex
                     ifelse(x_i$age>=65, rr.HAct.65,1))                   # risk modification for age
  
  p.HD_it <- min(1, p.HD * fire_it * 
                   (x_i$sex * rr.HD.f + (1-x_i$sex)) *              # risk modification for sex
                   ifelse(x_i$age>=65, rr.HD.65,1))                       # risk modification for age
  
  v.p.it[M_it == "H"] <- c(1-p.HAct_it-p.HD_it , p.HAct_it, 0, p.HD_it)
  v.p.it[M_it == "Act"] <- c(p.ActH, 1-p.ActH-p.ActPrm-p.ActD, p.ActPrm, p.ActD)
  v.p.it[M_it == "Prm"] <- c(0, 0, 1-p.PrmD_it, p.PrmD_it)
  v.p.it[M_it == "D"] <- c(0, 0, 0, 1)
  # print(c(v.p.it,sum(v.p.it)))
  # if (is.na(v.p.it[1])==TRUE) {
  #   print(p.HAct_it)
  #   print(p.HD_it)
  # }
  ifelse(isTRUE(all.equal(sum(v.p.it), 1)), return(v.p.it), print(c("Probabilities do not sum to 1")))     # return error if probabilities do not sum to 1
}

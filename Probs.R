Probs <- function(M_it,                      # health state occupied by individual i at cycle t
                  fire_it,                   # fire experience of individual i at cycle t (binary)
                  x_i,                       # individual characteristics of individual i (row i of m.x)
                  smoke.duration.cum_it,     # cumulative smoke exposure of individual i up to cycle t
                  deathRate_t) {             # death rate at cycle t 
  
  v.p.it <- rep(NA, length(v.n_asthma))
  names(v.p.it) <- v.n_asthma
  
  # p.1.21_it <- p.1.21 * 
  #   (fire_it * rr.1.21.fire + (1-fire_it)) *   # add risk due to fire
  #   (x_i$sex * rr.1.21.f + (1-x_i$sex)) *      # add risk due to sex
  #   ifelse(x_i$age>=65, rr.1.21.age, 1)        # add risk due to age

  #                          
  v.p.it[M_it==0]  <- c(1- p.0.1 - deathRate_t, # 0
                        p.0.1, # 1
                        0, # 21
                        0, # 22
                        0, # 212
                        0, # 223
                        0, # 50
                        deathRate_t) # 100
  
  v.p.it[M_it==1]  <- c(0,  # 00
                        1- p.1.21 - p.1.22 - p.1.212 - p.1.223 - deathRate_t,  # 1
                        p.1.21,  # 21
                        p.1.22,  # 22
                        p.1.212,  # 212
                        p.1.223,  # 223
                        0,  # 50
                        deathRate_t)  # 100
  
  v.p.it[M_it==21] <- c(0,  # 00
                        p.21.1,  # 1
                        1- p.21.1 - p.21.22 - p.21.212 - p.21.223 - p.asthmaMort - deathRate_t,  # 21
                        p.21.22,  # 22
                        p.21.212,  # 212
                        p.21.223,  # 223
                        p.asthmaMort,  # 50
                        deathRate_t)  # 100
  
  v.p.it[M_it==22] <- c(0,  # 00
                        p.22.1,  # 1
                        p.22.21,  # 21
                        1- p.22.1 - p.22.21 - p.22.212 - p.22.223 - p.asthmaMort - deathRate_t,  # 22
                        p.22.212,  # 212
                        p.22.223,  # 223
                        p.asthmaMort,  # 50
                        deathRate_t)  # 100
  
  v.p.it[M_it==212] <- c(0,  # 00
                        p.212.1,  # 1
                        p.212.21,  # 21
                        p.212.22,  # 22
                        1- p.212.1 - p.212.21 - p.212.22 - p.212.223 - p.asthmaMort - deathRate_t,  # 212
                        p.212.223,  # 223
                        p.asthmaMort,  # 50
                        deathRate_t)  # 100  
  
  v.p.it[M_it==223] <- c(0,  # 00
                         p.223.1,  # 1
                         p.223.21,  # 21
                         p.223.22,  # 22
                         p.223.212,  # 212
                         1- p.223.1 - p.223.21 - p.223.22 - p.223.212 - p.asthmaMort - deathRate_t,  # 223
                         p.asthmaMort,  # 50
                         deathRate_t)  # 100  

    v.p.it[M_it==50] <- c(0,  # 00
                         0,  # 1
                         0,  # 21
                         0,  # 22
                         0,  # 212
                         0,  # 223
                         1,  # 50
                         0)  # 100    
    
    v.p.it[M_it==100] <- c(0,  # 00
                          0,  # 1
                          0,  # 21
                          0,  # 22
                          0,  # 212
                          0,  # 223
                          0,  # 50
                          1)  # 100   
  
  
  if (any(is.na(v.p.it==TRUE))) {
    print("NAs produced")
  } else if (any(v.p.it<0)) {
      print("Negative probability calculated") 
      print(v.p.it[v.p.it<0])
  } else if (isTRUE(all.equal(sum(v.p.it), 1))) {
      return(v.p.it)
  } else {
    print("Probabilities do not sum to 1")
    print(sum(v.p.it))
    print(v.p.it)
    stop()
  }
    
}
  # OCS only
  # ED only
  # ED + hosp
  # OCS + ED
  
  # has anyone published shorter cycles within cycles
  

  # if you want a shorter state that's 5 days, you could force people who go into hospital 1 to go into hospital 2
  
  
  
  
  
  # m.p.it <- m.transitionProbs[m.transitionProbs$from==v.n_asthma[M_it], ]        # pull appropriate rows from probability matrix
  # 
  # v.p.it <- m.p.it$prob * 
  #   fire_it * m.p.it$fire_adj + (1-fire_it) *           # add risk due to fire
  #   (x_i$sex * m.p.it$sex_adj + (1-x_i$sex)) *          # add risk due to sex
  #   ifelse(x_i$age>=65, m.p.it$age_adj, 1)              # add risk due to age
  # 
  # v.p.it[1] <- 1 - sum(v.p.it[2:length(v.p.it)])

# The above calculation disregards non-wildfire related exacerbation!

    # update v.p.it with appropriate probabilities (note: v.n is H; Act; Prm; D)
  
  # p.PrmD_it <- min(1, fire_it *
  #                    (1-exp(-r.PrmD * (1 + rp.smoke * smoke.duration.cum_it))))     # calculate p.PrmD conditional on cumulative exposure
  # 
  # p.HAct_it <- min(1, p.HAct * fire_it * 
  #                    (x_i$sex * rr.HAct.f + (1-x_i$sex)) *          # risk modification for sex
  #                    ifelse(x_i$age>=65, rr.HAct.65,1))                   # risk modification for age
  # 
  # p.HD_it <- min(1, p.HD * fire_it * 
  #                  (x_i$sex * rr.HD.f + (1-x_i$sex)) *              # risk modification for sex
  #                  ifelse(x_i$age>=65, rr.HD.65,1))                       # risk modification for age
  # 
  # v.p.it[stateIndex==1] <- c(1-p.HAct_it-p.HD_it , p.HAct_it, 0, p.HD_it)
  # v.p.it[M_it == "Act"] <- c(p.ActH, 1-p.ActH-p.ActPrm-p.ActD, p.ActPrm, p.ActD)
  # v.p.it[M_it == "Prm"] <- c(0, 0, 1-p.PrmD_it, p.PrmD_it)
  # v.p.it[M_it == "D"] <- c(0, 0, 0, 1)
  # print(c(v.p.it,sum(v.p.it)))
  # if (is.na(v.p.it[1])==TRUE) {
  #   print(p.HAct_it)
  #   print(p.HD_it)
  # }
  
  # check if any negative values were calculated, then return error if probabilities do not sum to 1
    

Probs <- function(M_it,                      # health state occupied by individual i at cycle t
                  fire_it,                   # fire experience of individual i at cycle t (binary)
                  x_i,                       # individual characteristics of individual i (row i of m.x)
                  smoke.duration.cum_it,     # cumulative smoke exposure of individual i up to cycle t
                  deathRate_t) {             # death rate at cycle t 
  
  v.p.it <- rep(0, length(v.n_asthma))
  names(v.p.it) <- v.n_asthma

  from_state = as.character(M_it)

    for (to_state_ind in seq_along(v.n_asthma)){
    to_state <- v.n_asthma[to_state_ind]
#    print(c(x_i$sex, x_i$age))
    
    if (from_state != to_state & to_state != '100'){
      v.p.it[to_state_ind] = modifyRisk(
        p0 = riskmodifiers[from_state, to_state, 'transition_prob'],
        x1 = fire_it,
        rr1 = riskmodifiers[from_state, to_state, "rr.fire"],
        x2 = x_i$sex,
        rr2 = riskmodifiers[from_state, to_state, "rr.sex"], 
        x3 = x_i$age>=riskmodifiers[from_state, to_state, "age_threshold"],
        rr3 = riskmodifiers[from_state, to_state, "rr.age"]
        )
    } 
    
    if (from_state != '100') {
            v.p.it['100'] = deathRate_t
    } 
  }
  
  v.p.it[from_state] = 1-sum(v.p.it)

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
    
#     v.p.it[M_it==initial_state]  <- c(1- p.0.1_it - deathRate_t, # 0
#                                       p.0.1_it, # 1
#                                       0, # 21
#                                       0, # 22
#                                       0, # 212
#                                       0, # 223
#                                       0, # 50
#                                       deathRate_t) # 100
#      p.0.1_it <- modifyRisk(
#       p.average = p.0.1,
#       rr.fire = rr.0.1.fire,
#       rr.sex = rr.0.1.f, 
#       rr.age = rr.0.1.age,
#       fire_it = fire_it,
#       sex = x_i$sex,
#       age = x_i$age, age.threshold = 40) 
#  
#   p.1.21_it <- modifyRisk(
#     p.average = p.1.21,
#     rr.fire = rr.0.1.fire(fire_it * rr.1.21.fire + (1-fire_it)) *
#     (x_i$sex * rr.1.21.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.1.21.age, 1)
#   
#   p.1.22_it <-  p.1.22 *
#     (fire_it * rr.1.22.fire + (1-fire_it)) *
#     (x_i$sex * rr.1.22.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.1.22.age, 1)
#   
#   p.1.212_it <- p.1.212 *
#     (fire_it * rr.1.212.fire + (1-fire_it)) *
#     (x_i$sex * rr.1.212.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.1.212.age, 1)
#   
#   p.1.223_it <- p.1.223 *
#     (fire_it * rr.1.223.fire + (1-fire_it)) *
#     (x_i$sex * rr.1.223.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.1.223.age, 1)
#   
#   p.1.50_it <- p.1.50 *
#     (fire_it * rr.1.50.fire + (1-fire_it)) *
#     (x_i$sex * rr.1.50.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.1.50.age, 1)
#   
#   p.21.1_it <- p.21.1 *
#     (fire_it * rr.21.1.fire + (1-fire_it)) *
#     (x_i$sex * rr.21.1.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.21.1.age, 1)
#   
#   p.21.22_it <- p.21.22 *
#     (fire_it * rr.21.22.fire + (1-fire_it)) *
#     (x_i$sex * rr.21.22.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.21.22.age, 1)
#   
#   p.21.212_it <- p.21.212 *
#     (fire_it * rr.21.212.fire + (1-fire_it)) *
#     (x_i$sex * rr.21.212.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.21.212.age, 1)
#   
#   p.21.223_it <- p.21.223 *
#     (fire_it * rr.21.223.fire + (1-fire_it)) *
#     (x_i$sex * rr.21.223.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.21.223.age, 1)
# 
#   p.21.50_it <- p.21.50 *
#     (fire_it * rr.21.50.fire + (1-fire_it)) *
#     (x_i$sex * rr.21.50.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.21.50.age, 1)
#   
#   p.22.1_it <- p.22.1 *
#     (fire_it * rr.22.1.fire + (1-fire_it)) *
#     (x_i$sex * rr.22.1.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.22.1.age, 1)
#   
#   p.22.21_it <- p.22.21 *
#     (fire_it * rr.22.21.fire + (1-fire_it)) *
#     (x_i$sex * rr.22.21.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.22.21.age, 1)
#   
#   p.22.212_it <- p.22.212 *
#     (fire_it * rr.22.212.fire + (1-fire_it)) *
#     (x_i$sex * rr.22.212.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.22.212.age, 1)
#   
#   p.22.223_it <- p.22.223 *
#     (fire_it * rr.22.223.fire + (1-fire_it)) *
#     (x_i$sex * rr.22.223.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.22.223.age, 1)
# 
#   p.22.50_it <- p.22.50 *
#     (fire_it * rr.22.50.fire + (1-fire_it)) *
#     (x_i$sex * rr.22.50.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.22.50.age, 1)
#   
#   p.212.1_it <- p.212.1 *
#     (fire_it * rr.212.1.fire + (1-fire_it)) *
#     (x_i$sex * rr.212.1.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.212.1.age, 1)
#   
#   p.212.21_it <- p.212.21 *
#     (fire_it * rr.212.21.fire + (1-fire_it)) *
#     (x_i$sex * rr.212.21.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.212.21.age, 1)
#   
#   p.212.22_it <- p.212.22 *
#     (fire_it * rr.212.22.fire + (1-fire_it)) *
#     (x_i$sex * rr.212.22.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.212.22.age, 1)
#   
#   p.212.223_it <- p.212.223 *
#     (fire_it * rr.212.223.fire + (1-fire_it)) *
#     (x_i$sex * rr.212.223.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.212.223.age, 1)
# 
#   p.212.50_it <- p.212.50 *
#     (fire_it * rr.212.50.fire + (1-fire_it)) *
#     (x_i$sex * rr.212.50.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.212.50.age, 1)
#   
#   p.223.1_it <- p.223.1 *
#     (fire_it * rr.223.1.fire + (1-fire_it)) *
#     (x_i$sex * rr.223.1.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.223.1.age, 1)
#   
#   p.223.21_it <- p.223.21 *
#     (fire_it * rr.223.21.fire + (1-fire_it)) *
#     (x_i$sex * rr.223.21.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.223.21.age, 1)
#  
#   p.223.22_it <- p.223.22 *
#     (fire_it * rr.223.22.fire + (1-fire_it)) *
#     (x_i$sex * rr.223.22.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.223.22.age, 1)
#   
#   p.223.212_it <- p.223.212 *
#     (fire_it * rr.223.212.fire + (1-fire_it)) *
#     (x_i$sex * rr.223.212.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.223.212.age, 1)
#   
#   p.223.50_it <- p.223.50 *
#     (fire_it * rr.223.50.fire + (1-fire_it)) *
#     (x_i$sex * rr.223.50.f + (1-x_i$sex)) *
#     ifelse(x_i$age>=65, rr.223.50.age, 1)
#   
#   
#   
#     v.p.it[M_it==0]  <- c(1- p.0.1_it - deathRate_t, # 0
#                         p.0.1_it, # 1
#                         0, # 21
#                         0, # 22
#                         0, # 212
#                         0, # 223
#                         0, # 50
#                         deathRate_t) # 100
#   
#   v.p.it[M_it==1]  <- c(0,  # 0
#                         1- p.1.21_it - p.1.22_it - p.1.212_it - p.1.223_it - deathRate_t,  # 1
#                         p.1.21_it,  # 21
#                         p.1.22_it,  # 22
#                         p.1.212_it,  # 212
#                         p.1.223_it,  # 223
#                         0,  # 50
#                         deathRate_t)  # 100
#   
#   v.p.it[M_it==21] <- c(0,  # 0
#                         p.21.1_it,  # 1
#                         1- p.21.1_it - p.21.22_it - p.21.212_it - p.21.223_it - p.21.50_it - deathRate_t,  # 21
#                         p.21.22_it,  # 22
#                         p.21.212_it,  # 212
#                         p.21.223_it,  # 223
#                         p.21.50_it,  # 50
#                         deathRate_t)  # 100
#   
#   v.p.it[M_it==22] <- c(0,  # 0
#                         p.22.1_it,  # 1
#                         p.22.21_it,  # 21
#                         1- p.22.1_it - p.22.21_it - p.22.212_it - p.22.223_it - p.22.50_it - deathRate_t,  # 22
#                         p.22.212_it,  # 212
#                         p.22.223_it,  # 223
#                         p.22.50_it,  # 50
#                         deathRate_t)  # 100
#   
#   v.p.it[M_it==212] <- c(0,  # 0
#                         p.212.1_it,  # 1
#                         p.212.21_it,  # 21
#                         p.212.22_it,  # 22
#                         1- p.212.1_it - p.212.21_it - p.212.22_it - p.212.223_it - p.212.50_it - deathRate_t,  # 212
#                         p.212.223_it,  # 223
#                         p.212.50_it,  # 50
#                         deathRate_t)  # 100  
#   
#   v.p.it[M_it==223] <- c(0,  # 0
#                          p.223.1_it,  # 1
#                          p.223.21_it,  # 21
#                          p.223.22_it,  # 22
#                          p.223.212_it,  # 212
#                          1- p.223.1_it - p.223.21_it - p.223.22_it - p.223.212_it - p.223.50_it - deathRate_t,  # 223
#                          p.223.50_it,  # 50
#                          deathRate_t)  # 100  
# 
#     v.p.it[M_it==50] <- c(0,  # 0
#                          0,  # 1
#                          0,  # 21
#                          0,  # 22
#                          0,  # 212
#                          0,  # 223
#                          1,  # 50
#                          0)  # 100    
#     
#     v.p.it[M_it==100] <- c(0,  # 0
#                           0,  # 1
#                           0,  # 21
#                           0,  # 22
#                           0,  # 212
#                           0,  # 223
#                           0,  # 50
#                           1)  # 100   
#   
#   
  

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
    
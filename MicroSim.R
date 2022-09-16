MicroSim <- function(initStates,                  # vector of initial health states
                     n.i,                         # number of individuals
                     n.t,                         # number of cycles    
                     m.fire,                      # annual fire data by neighborhood
                     v.n_asthma,                  # vector of health state names
                     m.x,                         # matrix with individual characteristics
                     cl,                          # cycle length
                     birthRate_bl,                # baseline birthrate
                     birthRate_change,            # annual change in birth rate
                     allCauseMortality_bl,        # baseline annual all cause mortality
                     allCauseMortality_change,    # annual change in all cause mortality
                     d.c,                         # discount rate for costs
                     d.e,                         # discount rate for utilities
                     intervention,                # is there an intervention? (scalar with Boolean value, could be vector of Booleans if mix of interventions)
                     seed = 1,                    # starting seed number
                     debug = FALSE) {                  
  
  # You need the following functions:
  # Probs: to estimate transition probabilities
  # Costs: to estimate costs of state values
  # Effs: to estimate health outcomes
  
  v.dwc <- 1 / (1 + d.c) ^ (0:n.t)     # calculate discount weights for costs per cycle
  v.dwe <- 1 / (1 + d.e) ^ (0:n.t)     # calculate discount weights for QALYs per cycle
  
  v.birthRate <- birthRate_bl * (1+birthRate_change)^(0:n.t)    # calculate birth rates per cycle
  v.deathRate <- (1+allCauseMortality_change)^(0:n.t+1)         # calculate death rate adjuster per cycle
  
  # initialize matrices to capture fires, health states, costs, and health outcomes each cycle:
  
  m.M <- m.C <- m.E <- matrix(nrow=n.i, ncol=n.t+1, dimnames = list(paste("ind", 1:n.i), paste("cycle",0:n.t)))  
  
  m.M[ ,1] <- initStates    
  m.C[ ,1] <- Costs(M_it=m.M[ ,1], intervention = intervention)
  m.E[ ,1] <- Effs(M_it=m.M[ ,1], x_i=m.x)
  
  v.totalPop  <- rep(NA, n.t+1)    # vector to track population size over each cycle
  v.totalPop[1] <- n.i
  
  if (debug==TRUE) {
    tracker <- matrix(nrow=n.i*(n.t), ncol=3+length(v.n_asthma))
    colnames(tracker) <- c("ind","cycle", "prior M", v.n_asthma)
  }

  for (t in 1:n.t) {         # start loop for time cycles
    
    if (debug==TRUE) {cat("### Cycle =", t, "\n")}
    
    totalBirths_t <- 0     # initialize variable to keep track of number of births (define this in the loop because we want it to reset each cycle)
    
    for (i in 1:n.i) {
      
      if (debug==TRUE) {cat("### Individual =", i, "\n")}
      
      set.seed(seed+i+t)                                                               # set seed for every individual
      neighborhood_index <- match(m.x$neighborhood[i], allNeighborhoods)             # get neighborhood number of individual i
      
      # calculate the transition probability for individual i at cycle t
      # First use the Probs function to calculate the transition probabilities into each "to" state for individual i,
      #   given their individual characteristics/risk factors and the health state they're coming from
      # Then sample the new health state using those probabilities.
      
      v.p <- Probs(M_it = m.M[i,t], 
                   fire_it = m.fire[neighborhood_index, t], 
                   x_i = m.x[i, ], 
                   intervention = ifelse(m.fire[neighborhood_index, t]==1, intervention, FALSE),
                   deathRate_t = v.deathRate[t+1])
      
      if (debug==TRUE) {
        tracker[i+(n.i*(t-1)), ] <- c(i, t, m.M[i,t], v.p)
      }

      m.M[i, t+1] <- sample(v.n_asthma, prob=v.p, size=1)                             # sample the next health state given adjusted probabilities
      
      m.C[i, t+1] <- Costs(M_it=m.M[i, t+1], intervention=intervention)               # estimate costs for individual i at cycle t+1
      m.E[i, t+1] <- Effs(M_it = m.M[i, t+1], x_i=m.x[i, ])                           # estimate QALYs for individual i at cycle t+1
      
      m.x$age[i] <- m.x$age[i] + cl                                                   # increase age by cycle length
      
      
      if (debug==TRUE) {print(m.x$neighborhood[i])}
      
      if (m.fire[neighborhood_index, t]==1) {m.x$exposure[i] <-  TRUE}                # if there has been a fire in their neighborhood, set exposure history to TRUE, otherwise keep the same
      
      
      # if individual is not Dead, determine # of kids born in year t
      
      if (m.M[i,t+1] != "50" & m.M[i, t+1] != "100") {
        kids_it <- rpois(1, v.birthRate[t])           # determine if individual i had children and how many using a Poisson distribution with probability being the birth rate at time t
        
        # if individual i had children, assign characteristics to child and add new row to m.x
        
        if (kids_it > 0) {
          for (kid in 1:kids_it) {
            new_x <- c(m.x[nrow(m.x), 'id'] + 1,                              # get last ID in population and add 1
                       0,                                                     # age is 0
                       rbinom(1,1,0.55),                                      # sex (0=male, 1=female) based on binomial distribution with p=0.55 for female
                       m.x$rural[i],                                          # same rural/urban designation as parent
                       ifelse(m.fire[neighborhood_index, t]==1, TRUE, FALSE), # exposure is TRUE if fire happened that year
                       m.x$neighborhood[i],                                   # same neighborhood as parent
                       m.x$fam_id[i])                                         # same family as parent
            m.x <- rbind(m.x, new_x)           # add the above as new row to m.x, matrix of individuals and risk factors
            
            
            # assign a health state, initial costs, and initial QALYs to the newborn
            
            new_M <-new_C <- new_E <- rep(NA,n.t+1)
            new_M[t+1] <- sample(v.n_asthma[1:2], prob=c(0.8, 0.2), size=1)                                                 # Add initial health state of new child to m.M
            m.M <- rbind(m.M, new_M)
            new_C[t+1] <- Costs(M_it=new_M[t+1], intervention = intervention)
            m.C <- rbind(m.C, new_C)
            new_E[t+1] <- Effs(M_it=new_M[t+1], x_i=new_x)
            m.E <- rbind(m.E, new_E)
          }
        }  
        totalBirths_t <- totalBirths_t + kids_it        # calculate total number of births in cycle t
        
        
      }
      
      
    }  # close loop for individuals
    
    n.i <- n.i + totalBirths_t   # increase n.i for next cycle by the total number of births
    v.totalPop[t+1] <- n.i       # add the new population size to the population size tracking vector
    
  }    # close loop for cycles
  
  
  tc <- m.C %*% v.dwc                # total discounted costs per individual
  te <- (m.E * cl) %*% v.dwe         # total discounted QALYs per individual
  tc_hat <- mean(tc, na.rm=TRUE)     # average discounted costs
  te_hat <- mean(te, na.rm=TRUE)     # average discounted QALYS
  
  
  # create simulation trace (both absolute and percentage)
  
    TR.absolute <- t(apply(m.M, 2, function(x) table(factor(x, levels=v.n_asthma, ordered=TRUE))))
    TR.proportion <- TR.absolute/v.totalPop 
    colnames(TR.absolute) <- v.n_asthma
    rownames(TR.absolute) <- paste("cycle", 0:n.t)
    colnames(TR.proportion) <- v.n_asthma
    rownames(TR.proportion) <- paste("cycle", 0:n.t)

  
  # return outputs as a list
  
  results <- list(
    m.M=m.M, 
    m.C=m.C,
    m.E=m.E,
    tc=tc,
    te=te,
    tc_hat=tc_hat,
    te_hat=te_hat,
    m.x=m.x, 
    TR.absolute=TR.absolute,
    TR.proportion=TR.proportion, 
    if (debug==TRUE) {tracker=tracker}
    )
  
  return(results)
  
}

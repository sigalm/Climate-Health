MicroSim <- function(initStates,                  # vector of initial health states
                     n.i,                         # number of individuals
                     n.t,                         # number of cycles    
                     m.fire,                      # annual fire data by neighborhood
                     m.smoke.duration.cum,        # annual smoke exposure data (i.e., air pollution by neighorhood)
                     v.n,                         # vector of health state names
                     m.x,                         # matrix with individual characteristics
                     cl,                          # cycle length
                     birthRate_bl,                # baseline birthrate
                     birthRate_change,            # annual change in birth rate
                     allCauseMortality_bl,        # baseline annual all cause mortality
                     allCauseMortality_change,    # annual change in all cause mortality
                     d.c,                         # discount rate for costs
                     d.e,                         # discount rate for utilities
                     TR.out = TRUE,               # should the output include a microsimulation trace?
                     TS.out = TRUE,               # should the output include a transition array between states?
                     intn=FALSE,                  # is there an intervention? (scalar with Boolean value, could be vector of Booleans if mix of interventions)
                     seed = 1) {                  # starting seed number
  
  # You need the following functions:
  # Probs: to estimate transition probabilities
  # Costs: to estimate costs of state values
  # Effs: to estimate health outcomes
  
  v.dwc <- 1 / (1 + d.c) ^ (0:n.t)     # calculate discount weights for costs per cycle
  v.dwe <- 1 / (1 + d.e) ^ (0:n.t)     # calculate discount weights for QALYs per cycle
  
  
  # initialize matrices to capture fires, health states, costs, and health outcomes each cycle:
  
  m.M <- m.C <- m.E <- matrix(nrow=n.i, ncol=n.t+1, dimnames = list(paste("ind", 1:n.i), paste("cycle",0:n.t)))  
  
  m.M[ ,1] <- initStates    
  m.C[ ,1] <- Costs(M_it=m.M[ ,1], intervention = intn)
  m.E[ ,1] <- Effs(M_it=m.M[ ,1], intervention=intn, x_i=m.x , cl=1)
  
  v.totalPop  <- rep(NA, n.t+1)
  v.totalPop[1] <- n.i
  
  for (t in 1:n.t) {
    totalBirths_t <- 0     # initialize variable to keep track of number of births (define this in the loop because we want it to reset each cycle)
    birthRate_t <- birthRate_bl * (1-birthRate_change)^(t-1)
    deathRate_t <- allCauseMortality_bl * (1-allCauseMortality_change)^(t-1)
    
    for (i in 1:n.i) {
      set.seed(seed+i)                                                               # set seed for every individual
      neighborhood_index <- match(m.x$neighborhood[i], allNeighborhoods)
      
      # calculate the transition probability for individual i at cycle t
      
      v.p <- Probs(M_it=m.M[i,t], 
                   fire_it=m.fire[neighborhood_index, t], 
                   x_i=m.x[i, ], 
                   m.smoke.duration.cum[neighborhood_index, t])
      # print(c(i,t))
      m.M[i, t+1] <- sample(v.n, prob=v.p, size=1)                                    # sample the next health state given probability to transition
      m.C[i, t+1] <- Costs(M_it=m.M[i, t+1], intervention=intn)                       # estimate costs for individual i at cycle t+1
      m.E[i, t+1] <- Effs(M_it=m.M[i, t+1], intervention=intn, x_i=m.x[i, ], cl)      # estimate QALYs for individual i at cycle t+1
      
      m.x$age[i] <- m.x$age[i] + cl                                                   # increase age by cycle length
      
      if (m.fire[neighborhood_index, t]==1) {m.x$exposure[i] <-  TRUE}                # if there has been a fire, set exposure history to TRUE, otherwise keep the same
      
      
      # if individual in not Dead, determine # of kids born in year t, and whether they die from non-wildfire causes
      
      if (m.M[i,t+1] != 'D') {
        kids_it <- rpois(1, birthRate_t)                       # determine if individual i had children and how many
        
        # if individual i had children, assign characteristics to child and add new row to m.x
        
        if (kids_it > 0) {
          for (kid in 1:kids_it) {
            new_x <- c(m.x[nrow(m.x), 'id'] + 1,                              # get last id and add 1
                       0,                                                     # age
                       rbinom(1,1,0.55),                                    # sex (0=male, 1=female)
                       m.x$rural[i],                                          # same rural/urban designation as parent
                       ifelse(m.fire[neighborhood_index, t]==1, TRUE, FALSE), # exposure is TRUE if fire happened that year
                       m.x$neighborhood[i],                                   # same neighborhood as parent
                       m.x$fam_id[i])                                         # same family as parent
            m.x <- rbind(m.x, new_x)
            # print(new_x)
            
            new_M <-new_C <- new_E <- rep(NA,n.t+1)
            new_M[t+1] <- "H"                                                 # Add initial health state of new child to m.M
            m.M <- rbind(m.M, new_M)
            new_C[t+1] <- Costs(M_it=new_M[t+1], intervention = intn)
            m.C <- rbind(m.C, new_C)
            new_E[t+1] <- Effs(M_it=new_M[t+1], intervention=intn, x_i=new_x, cl)
            m.E <- rbind(m.E, new_E)
          }
        }  
        totalBirths_t <- totalBirths_t + kids_it        # calculate total number of births in cycle t
        
      # if individual is not dead, determine whether they die from other causes (i.e., all cause mortality)
        
        death <- rbinom(n=1, size=1, prob=deathRate_t)
        m.M[i, t+1] <- ifelse(death==1, "D", m.M[i, t+1])   
      }
      
      
    }  # close loop for individuals
    
    n.i <- n.i + totalBirths_t   # increase n.i for next cycle
    v.totalPop[t+1] <- n.i
    # print(n.i)
    # print(t)
    
  }    # close loop for cycles
  
  tc <- m.C %*% v.dwc     # total discounted costs per individual
  te <- m.E %*% v.dwe     # total discounted QALYs per individual
  tc_hat <- mean(tc, na.rm=TRUE)      # average discounted costs
  te_hat <- mean(te, na.rm=TRUE)      # average discounted QALYS
  
  
  if (TS.out == TRUE) {                                    # create matrix of transition across states
    TS <- paste(m.M, cbind(m.M[,-1], "D"), sep = "->")     # this kills off everyone in the last cycle, which is not true. need to fix.
    TS <- matrix(TS, nrow=n.i)
    rownames(TS) <- paste("ind", 1:n.i)
    colnames(TS) <- paste("cycle", 0:n.t)
  } else {
    TS <-  NULL
  }
  
  if (TR.out == TRUE) {                                    # create the distribution trace 
    TR <- t(apply(m.M, 2, function(x) table(factor(x, levels=v.n, ordered=TRUE))))
    TR <- TR/v.totalPop 
    colnames(TR) <- v.n
    rownames(TR) <- paste("cycle", 0:n.t)
  } else {
    TR <- NULL
  }
  
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
    TS=TS, 
    TR=TR)
  
  return(results)
  
}

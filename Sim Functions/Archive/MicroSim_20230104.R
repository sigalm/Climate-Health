MicroSim <- function(initStates,                  # vector of initial health states
                     n.i,                         # number of individuals
                     n.t,                         # number of cycles    
                     m.fire,                      # annual fire data by neighborhood
                     v.n_asthma,                  # vector of health state names
                     m.x,                         # matrix with individual characteristics
                     # riskmodifiers,               # 3D object containing effects sizes for risk modifiers
                     cl,                          # cycle length
                     birthRate_bl,                # baseline birthrate
                     birthRate_change,            # annual change in birth rate
                     allCauseMortality_change,    # annual change in all cause mortality
                     d.c,                         # discount rate for costs
                     d.e,                         # discount rate for utilities
                     intervention,                # intervention coverage/reach
                     intervention_trigger,        # the level of exposure that triggers deployment of intervention
                     seed = 1,                    # starting seed number
                     debug = FALSE) {                  
  
  # You need the following functions:
  # Probs: to estimate transition probabilities
  # Costs: to estimate costs of state values
  # Effs: to estimate health outcomes
  
  v.dwc <- 1 / (1 + d.c) ^ (0:n.t)     # calculate discount weights for costs per cycle
  v.dwe <- 1 / (1 + d.e) ^ (0:n.t)     # calculate discount weights for QALYs per cycle
  
  v.birthRate <- birthRate_bl * (1+birthRate_change)^(0:n.t)    # calculate birth rates per cycle
  v.deathRate <- (1+allCauseMortality_change)^(0:n.t)         # calculate death rate adjuster per cycle
  
  # initialize matrices to capture fires, health states, costs, and health outcomes each cycle:
  
  m.M <- m.I <- m.T <-  m.C <- m.E <- matrix(nrow=n.i, ncol=n.t+1, dimnames = list(paste("ind", 1:n.i), paste("cycle",0:n.t))) 
  
  m.I[ ,1] <- 0                                                    # initial intervention: none
  m.T[ ,1] <- m.x$asthmaCare                                       # initial asthma management: pulled from individual data
  m.M[ ,1] <- initStates                                           # initial health states: pulled from data
  m.C[ ,1] <- Costs(M_it=m.M[ ,1], intervention = m.I[ ,1])        # initial costs: calculated from health state, intervention, and therapy
  m.E[ ,1] <- Effs(M_it=m.M[ ,1], x_i=m.x)                         # initial QALYs: calculated from health state and individual data
  
  # initialize treatment tracking table (keep track of how long people have been on current therapy, if it has been successful in the past)
  temp <- expand.grid(m.x$id, v.therapy)
  txTracker <- data.frame(id=temp$Var1, tx=temp$Var2, txStart=NA, txEnd=NA, txSuccess=NA, txCount=0)
  
  
  # assign start time to each individual for the therapy at initialization
  
  for (i in 1:n.i) {
    tx <-  m.x[i, "asthmaCare"]      # find what therapy they're on
    txTracker[txTracker$id==i & txTracker$tx==tx, ]$txStart <- 0  # note start time 0 for that individual-treatment row
  }  
  
  v.totalPop  <- rep(NA, n.t+1)    # vector to track population size over each cycle
  v.totalPop[1] <- n.i
  
  if (debug) {
    tracker <- matrix(nrow=n.i*n.t, ncol=3+length(v.n_asthma))
    colnames(tracker) <- c("ind","cycle", "prior M", v.n_asthma)
  }
  
  for (t in 1:n.t) {         # start loop for time cycles
    
    if (debug) {cat("### Cycle =", t, "\n")}
    
    totalBirths_t <- 0     # initialize variable to keep track of number of births (define this in the loop because we want it to reset each cycle)
    
    for (i in 1:n.i) {
      
      if (debug) {cat("### Individual =", i, "\n",
                            "age of 201", m.x[201,"age"], class(m.x[201,"age"]),"\n")}
      
      set.seed(seed+i*79+t*71)                                                     # set seed for every individual
      neighborhood_index <- match(m.x$neighborhood[i], allNeighborhoods)    # get neighborhood number of individual i
      
      
      # determine if individual gets intervention
      # if a fire happened in their neighborhood, or if the total exposure to nearby fires is above a given threshold, they MAY receive intervention (based on intervention coverage)
      
      fire_it <- m.fire[neighborhood_index, t]
      exposure_it <- sum(m.fire[ ,t]*neighborhood.effect.size[ ,neighborhood_index]) # eventually will need wind direction
      
      if (debug) {print(exposure_it)}
      
      set.seed(seed+i*79+t*71) 
      if (intervention>0 & (fire_it==1 | exposure_it>=intervention_trigger)) {
        m.I[i,t+1] <- rbinom(n=1, size=1, prob=intervention)
      } else {
        m.I[i,t+1] <- 0
      }
      
      
      
      # calculate the transition probability for individual i at cycle t
      # First use the Probs function to calculate the transition probabilities into each "to" state for individual i,
      #   given their individual characteristics/risk factors and the health state they're coming from
      # Then sample the new health state using those probabilities.
      
      
      v.p <- Probs(M_it = m.M[i,t], 
                   fire_it = fire_it, 
                   x_i = m.x[i, ], 
                   riskmodifiers = riskmodifiers,
                   intervention_it = m.I[i,t],
                   deathRate_t = v.deathRate[t],
                   debug=debug)
      
      
      if (debug) {
        cat(v.p, "\n", "cycle=", t, "\n")
        tracker[i+(n.i*(t-1)), ] <- c(i, t, m.M[i,t], v.p)
      }
      set.seed(seed+i*79+t*71) 
      m.M[i, t+1] <- sample(v.n_asthma, prob=v.p, size=1)                             # sample the next health state given adjusted probabilities
      
      
      # Determine new therapy if therapy changed
      set.seed(seed+i*79+t*71) 
      
      worseControl <- as.integer(m.M[i,t+1])>as.integer(m.M[i, t])                              # assess if asthma control status got worse
      lastDrVisit <-  (t+1) - txTracker$txStart[txTracker$id==i & txTracker$tx==m.T[i,t]]       # assess time since current therapy began
      # print(worseControl)
      # print(m.T[i,t])
      # print(lastDrVisit)
      
      if (worseControl | lastDrVisit>2) {   # if you have poorer control of if you're due for a doctor's visit, you will be reevaluated for changing therapies
        m.T[i, t+1] <- sample(v.therapy, size=1, prob=therapyMatrix[m.M[i,t+1], ])             # determine therapy for next cycle (given prob of each therapy based on new health state)
        txTracker$txEnd[txTracker$id==i & txTracker$tx==m.T[i,t]] <- t                         # note tx end date for previous tx
        txTracker$txStart[txTracker$id==i & txTracker$tx==m.T[i,t+1]] <- t                     # note tx start date for new tx
        txTracker$txCount[txTracker$id==i & txTracker$tx==m.T[i,t]] <- txTracker$txCount[txTracker$id==i & txTracker$tx==m.T[i,t]] + 1  # update lifetime count of that therapy
        
        if (as.integer(m.M[i,t+1])==as.integer(m.M[i, t]) | worseControl) {  # assess whether therapy was successful. if no change in control or poorer control, therapy NOT successful. To be discussed.
          txTracker$txSuccess[txTracker$id==i & txTracker$tx==m.T[i,t]] <- 0
        } else {
          txTracker$txSuccess[txTracker$id==i & txTracker$tx==m.T[i,t]] <- 1
        }
      } else {                                                               # otherwise keep using the same therapy
        m.T[i, t+1] <- m.T[i,t]
      }
      
      m.C[i, t+1] <- Costs(M_it=m.M[i, t+1], intervention_it=m.I[i,t+1])               # estimate costs for individual i at cycle t+1
      m.E[i, t+1] <- Effs(M_it = m.M[i, t+1], x_i=m.x[i, ])                           # estimate QALYs for individual i at cycle t+1
      
      m.x$age[i] <- m.x$age[i] + cl                                                   # increase age by cycle length
      
      
      if (debug) {print(m.x$neighborhood[i])}
      
      if (m.fire[neighborhood_index, t]==1) {m.x$exposure[i] <-  TRUE}                # if there has been a fire in their neighborhood, set exposure history to TRUE, otherwise keep the same
      
      
      # if individual is not Dead, determine # of kids born in year t
      
      if (m.M[i,t+1] != "50" & m.M[i, t+1] != "100") {
        set.seed(seed+i*79+t*71) 
        kids_it <- rpois(1, v.birthRate[t])           # determine if individual i had children and how many using a Poisson distribution with probability being the birth rate at time t
        
        # if individual i had children, assign characteristics to child and add new row to m.x
        
        if (kids_it > 0) {
          cat("I have",kids_it,"baby(ies)klklnknkllk)!", i,"\n")
          for (kid in 1:kids_it) {
            new_x <- c(as.numeric(m.x[nrow(m.x), 'id'] + 1),                              # get last ID in population and add 1
                       as.numeric(0),                                                     # age is 0
                       as.integer(rbinom(1,1,0.55)),                                      # sex (0=male, 1=female) based on binomial distribution with p=0.55 for female
                       as.logical(ifelse(m.fire[neighborhood_index, t]==1, TRUE, FALSE)), # exposure is TRUE if fire happened that year
                       as.integer(m.x$rural[i]),                                          # same rural/urban designation as parent
                       as.character("NoTx"),                                              # define asthma care (none because no asthma at birth)
                       as.numeric(m.x$neighborhood[i]),                                   # same neighborhood as parent
                       as.integer(m.x$fam_id[i]))                                         # same family as parent
            m.x <- rbind(m.x, new_x)           # add the above as new row to m.x, matrix of individuals and risk factors
            
            
            # assign a health state, initial costs, and initial QALYs to the newborn
            
            new_M <- new_I <- new_T <-new_C <- new_E <- rep(NA,n.t+1)
            new_M[t+1] <- 0                                                 # Add initial health state of new child to m.M (age of onset is 5, so no asthma when born)
            m.M <- rbind(m.M, new_M)
            new_C[t+1] <- Costs(M_it=new_M[t+1], intervention = intervention)
            m.C <- rbind(m.C, new_C)
            new_E[t+1] <- Effs(M_it=new_M[t+1], x_i=new_x)
            m.E <- rbind(m.E, new_E)
            m.I <- rbind(m.I, new_I)
            m.T <- rbind(m.T, new_T) 
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
    m.I=m.I,
    m.T=m.T,
    m.C=m.C,
    m.E=m.E,
    tc=tc,
    te=te,
    tc_hat=tc_hat,
    te_hat=te_hat,
    m.x=m.x, 
    TR.absolute=TR.absolute,
    TR.proportion=TR.proportion, 
    txTracker=txTracker,
    if (debug) {tracker=tracker}
  )
  
  return(results)
  
}


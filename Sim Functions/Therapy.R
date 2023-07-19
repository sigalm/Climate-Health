Therapy <- function(seed, i, t, m_asthma_states, tx_tracker, m_asthma_therapies, v_asthma_therapies, m_asthma_therapy_probs) {
  # Determine new therapy if therapy changed
  set.seed(seed+i*79+t*71) 
  worse_control <- as.integer(m_asthma_states[i,t+1]) > 
    as.integer(m_asthma_states[i, t])                                        # assess if asthma control status got worse
  last_dr_visit <-  (t+1) - 
    tx_tracker$tx_start[tx_tracker$id==i & 
                          tx_tracker$tx==m_asthma_therapies[i,t]]            # assess time since current therapy began
  
  # if you have poorer control of if you're due for a doctor's visit, 
  # you will be reevaluated for changing therapies
  
  if (worse_control | last_dr_visit>2) {                                 
    
    m_asthma_therapies[i, t+1] <- 
      sample(v_asthma_therapies, size=1, 
             prob=m_asthma_therapy_probs[m_asthma_states[i,t+1], ])          # determine therapy for next cycle (given prob of each therapy based on new health state)
    
    tx_tracker$tx_end[tx_tracker$id==i & 
                        tx_tracker$tx==m_asthma_therapies[i,t]] <- t         # note tx end date for previous tx
    
    tx_tracker$tx_start[tx_tracker$id==i & 
                          tx_tracker$tx==m_asthma_therapies[i,t+1]] <- t     # note tx start date for new tx
    
    tx_tracker$tx_count[tx_tracker$id==i & 
                          tx_tracker$tx==m_asthma_therapies[i,t]] <- 
      tx_tracker$tx_count[tx_tracker$id==i & 
                            tx_tracker$tx==m_asthma_therapies[i,t]] + 1      # update lifetime count of that therapy
    
    # assess whether therapy was successful
    # if no change in control or poorer control, therapy NOT successful.
    # To be discussed.
    
    if (as.integer(m_asthma_states[i,t+1]) == 
        as.integer(m_asthma_states[i, t]) | worse_control) {                 
      tx_tracker$tx_success[tx_tracker$id==i & 
                              tx_tracker$tx==m_asthma_therapies[i,t]] <- 0
    } else {
      tx_tracker$tx_success[tx_tracker$id==i & 
                              tx_tracker$tx==m_asthma_therapies[i,t]] <- 1
    }
  } else {                                                                   # otherwise keep using the same therapy
    m_asthma_therapies[i, t+1] <- m_asthma_therapies[i,t]
  }
}
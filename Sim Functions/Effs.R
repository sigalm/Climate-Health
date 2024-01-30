Effs <- function(M_it,               # M_it: health state occupied by individual i at cycle t
                 time_rf_it,         # risk factor for recency of severe exacerbation
                 x_i=NULL) {         # x_i: matrix of individual characteristics

  u_it <- v_asthma_hsu[match(M_it, v_asthma_state_names)] + v_hsu_decrements[(max_dur_decrement - time_rf_it)]

  
}
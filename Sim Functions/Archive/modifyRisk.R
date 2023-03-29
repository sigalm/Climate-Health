# This function calculates a single "risk modifier" given an individuals risk factors and relevant risk ratios


modifyRisk <- function(v.x,       # Vector of binary risk factors (as 1/0 logicals)
                       v.rr) {    # vector of respective risk ratios
  
    # check that same number of risk factors and risk ratios were provided
  
    if (length(v.x) != length(v.rr)) {
    print("incorrect number of risk ratios given")
    cat("v.x = ", v.x)
    cat("v.rr = ", v.rr)
    stop()
  }
  
  
  # Calculate and return overall risk modifier by multiplying the two vectors item-wise
  
  modifier <- prod((v.x * v.rr + (1-v.x)))

  return(modifier)
  
}


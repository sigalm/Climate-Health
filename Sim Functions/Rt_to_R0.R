# Rt is total risk (i.e., average risk across all groups)
# R0 is the risk for those without the risk factor
# p1 is the proportion of the population WITH the risk factor
# RR1 is the risk ratio for the risk factor
# R1 is the risk for those WITH the risk factor



Rt_to_R0 <- function(
    Rt,
    p1,
    RR1
    ) {
  
  R0 = Rt / ((1-p1) + RR1 * p1)
  R1 = R0 * RR1
  
  if(R0 <=1 & R1 <=1) {
    return(c(R0, R1))
  } else {
    print(c("Probability greater than 1 calculated", R0=R0, R1=R1))
  }
  
}

# R0*p0 + R1*p1 + R2*p2 = Rt
# R0*p0 + R0*RR1*p1 + R0*RR2*p2 = Rt
# 
# R0 * (p0 + RR1*p1 + RR2*p2) = Rt


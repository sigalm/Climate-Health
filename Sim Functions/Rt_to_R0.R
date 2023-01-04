
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


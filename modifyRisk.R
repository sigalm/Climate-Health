modifyRisk <- function(p0,
                       x1 = FALSE,
                       rr1 = 1,
                       x2 = FALSE,
                       rr2 = 1,
                       x3 = FALSE,
                       rr3 = 1) {
  
  p1 <- p0 *
    (x1 * rr1 + (1-x1)) *
    (x2 * rr2 + (1-x2)) *
    (x3 * rr3 + (1-x3))
  
  return(p1)
  
}

###################################################
##  Functions for continuous size calculations
##
##  Author: Alex Godwood
##
##  Date: 16 Dec 2010
##
####################################################

### Continuous

cont.ss = function(alpha, power, sd, delta, r){
  theCall <- match.call()
  print(theCall)
  effect.size <- delta/sd
  n1 <- ceiling(((r+1)*((qnorm(power) + qnorm(1-alpha/2))^2)*sd^2)/(r*delta^2))
  n2 <- r*n1
  dmin <- sqrt(((r+1)*sd^2*(qnorm(1-alpha/2))^2)/(n1*r))
  res <- data.frame(delta, sd, effect.size, power, alpha, r, n1, n2, dmin)
  return(res)
}

### Continuous equivalence Julious equation 5.30

cont.eq = function(alpha, power, sd, d=0, delta, r){
  # d = assumed treatment difference. Default is no difference
  # alpha is one-sided for this, so alpha = 0.025 would give 95% CI
  theCall <- match.call()
  print(theCall)

  if (d == 0) {
    n1 <- ceiling(((r+1)*((qnorm(1-(1-power)/2) + qnorm(1-alpha))^2)*sd^2)/(r*(delta)^2))
  } else {
    n1 <- ceiling(((r+1)*((qnorm(1-(1-power)) + qnorm(1-alpha))^2)*sd^2)/(r*(d-delta)^2))
  }
 # n1 <- ifelse(d==0, ceiling(((r+1)*((qnorm(1-(1-power)/2) + qnorm(1-alpha))^2)*sd^2)/(r*(delta)^2)),
 #             ceiling(((r+1)*((qnorm(1-(1-power)) + qnorm(1-alpha))^2)*sd^2)/(r*(d-delta)^2)))
  n2 <- r*n1
  #dmin = sqrt(((r+1)*sd^2*(qnorm(1-alpha/2))^2)/(n1*r))
  res <- data.frame(delta, sd, power, alpha, r, n1, n2)
  return(res)
}

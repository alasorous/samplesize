###################################################
##  Functions for count data sample size calculations
##
##  Author: Alex Godwood
##
##  Date: 16 Dec 2010
##
####################################################

##  Poisson sample size

poisson.ss <- function(alpha, power, control, od, perc.drop){
  ## od = overdispersion
  ## control = control mean
  ## perc.drop = % reduction to detect
  theCall <- match.call()
  print(thecall)
  exper <- control*(100-perc.drop)/100
  n1 <- (od*(control+exper)*(qnorm(power)+qnorm(1-alpha/2))^2)/(control-exper)^2

  res <- data.frame(alpha, power, control, od, perc.drop, control, experimental=exper, n1)
  return(res)
}

#poisson.ss(alpha=0.05, power=0.9, control=1.5, od=1.3, perc.drop=20)

##  Negative binomial sample size

neg.binom.ss <- function(alpha, power, control, k, perc.drop){
  ## k = dispersion parameter
  ## control = control mean
  ## perc.drop = % reduction to detect
  theCall <- match.call()
  print(theCall)
  exper <- control*(100-perc.drop)/100
  n1 <- (((qnorm(power)+qnorm(1-alpha/2))^2)/(log(control/exper))^2)*(((control+exper)/(control*exper))+2*k)

  res <- data.frame(alpha, power, control, k, perc.drop, control, experimental=exper, overdispersion=k, n1=n1)
  return(res)
}

#neg.binom.ss(alpha=0.05, power=0.9, control=1.5, k=1.3, perc.drop=20)
#neg.binom.ss(alpha=0.05, power=0.9, control=1, k=0.56, perc.drop=20)

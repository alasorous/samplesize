###################################################
##  Functions for binary sample size calculations
##
##  Author: Alex Godwood
##
##  Date: 16 Dec 2010
##
####################################################

## Includes continuity correction

prop.ss <- function(alpha, power, p1, p2, r){
  theCall <- match.call()
  print(theCall)
  delta <- abs(p2-p1)
  Pbar <- (p1 + p2*r)/(r+1)
  Qbar <- 1-Pbar
  n1dash <- ((qnorm(1-alpha/2)*sqrt((r+1)*Pbar*Qbar) + qnorm(power)*sqrt(r*p1*(1-p1) + p2*(1-p2)))^2
            /(r*delta^2))
  n1 <- ceiling((n1dash/4)*(1 + sqrt(1 + 2*(r+1)/(n1dash*r*delta)))^2)
  n2 <- r*n1
  res <- data.frame(alpha, power, p1, p2, r, n1, n2)
  return(res)
}

## Binary NI Julious 2012

prop.ni <- function(alpha, power, p1, p2, d){
  theCall <- match.call()
  print(theCall)
  za <- qnorm(1-alpha/2)
  zb <- qnorm(power)
  n <- ceiling(((za*sqrt(p1*(1-p1) + p2*(1-p2)) + zb*sqrt(p1*(1-p1) + p2*(1-p2)))^2)/(((p1-p2)-d)^2))
  res <- data.frame(alpha, power, p1, p2, d, n1)
  return(res)
}

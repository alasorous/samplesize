###################################################
##  Functions for survival data sample size calculations
##
##  Author: Alex Godwood
##
##  Date: 16 Dec 2010
##
####################################################
####################################################

##  Survival sample size

surv.ss <- function(alpha, power, HR, r){
  theCall <- match.call()
  print(theCall)
  events.req <- (((r+1)*(qnorm(1-alpha/2)+qnorm(power)))/(sqrt(r)*log(HR)))^2
  chr <- exp(-1*((r+1)*(qnorm(1-alpha/2)))/sqrt(r*events.req))
  res <- data.frame(alpha, power, HR, r, events=events.req, criticalHR= chr)
  return(res)
}
#surv.ss(alpha=0.05, power=0.9, HR=0.8, r=1)

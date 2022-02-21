## Binary NI Julious 2012
## Julious sample size book 11.3.1.1 Method 1: Using Anticipated Responses

prop.ni <- function(alpha, power, p1, p2, d){
  theCall <- match.call()
  print(theCall)
  za <- qnorm(1-alpha/2)
  zb <- qnorm(power)
  n <- ceiling((za + zb)^2*(p1*(1-p1) + p2*(1-p2))/(((p1-p2)-d)^2))
  res <- data.frame(alpha, power, p1, p2, d, n)
  return(res)
}

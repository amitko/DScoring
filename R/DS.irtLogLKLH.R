DS.irtLogLKLH <- function(theta,response,parameters,o = DS.options()) {
  
  p <- DS.irtPCR(parameters,theta,o)
  
  loglike <- sum(log(p) * response + log(1 - p) * (1 - response))
  
  return(loglike)
  
}
DS.irtPCR <- function(parameters,theta,o = DS.options()) {
  
  res = matrix(nrow = length(theta) ,ncol = nrow(parameters));
  
  if ( o$model == 1)
    p1 = as.numeric(parameters[,1])
  
  if ( o$model >= 2) {
    p2 = as.numeric(parameters[,1])
    p1 = as.numeric(parameters[,2])
  }
  
  if ( o$model == 3)
    p3 = as.numeric(parameters[,3])
  
  for (k in 1:length(theta) ) {
    d = as.numeric(theta[k])
    if ( o$model == 1){
      res[k,] = 1 / (1 + exp(-1.7 * (d - p1)))
    }
    if ( o$model == 2){
      res[k,] = 1 / (1 + exp(-1.7 * p1 * (d - p2)))
    }
    if ( o$model == 3){
      res[k,] = p3 + (1-p3) / (1 + exp(-1.7 * p1 * (d - p2)))
    }
  }
  
  res[ res < 0.00001 ] <- 0.00001
  res[ res > 0.99999 ] <- 0.99999
  
  return(res)
}

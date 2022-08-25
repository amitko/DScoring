DS.PCR <- function(parameters,Dscore,o = dS.options()) {

  res = matrix(nrow = nrow(Dscore),ncol = nrow(parameters));

  for (k in 1:nrow(Dscore)) {
    d = as.numeric(Dscore[k,1]);
    if ( o$model == 1){
      res[k,] = 1 / ( 1 + ((1-d)*parameters[,1]) / ((1 - parameters[,1]) * d));
    }
    if ( o$model == 2){
      res[k,] = 1 / ( 1 + ((1-d)*parameters[,1]) / ((1 - parameters[,1]) * d)^parameters[,2]);
    }
    if ( o$model == 3){
      res[k,] = parameters[,3] + (1-parameters[,3])*(1 / ( 1 + ((1-d)*parameters[,1]) / ((1 - parameters[,1]) * d)^parameters[,2]));
    }
  }

  res[ res < 0.00001 ] <- 0.00001;
  res[ res > 0.99999 ] <- 0.99999;

  return(res);
}

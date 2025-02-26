DS.PCR <- function(parameters,Dscore,o = DS.options()) {

  nrow = nrow(Dscore)
  res = matrix(nrow = nrow(Dscore),ncol = nrow(parameters));

  if ( o$model >= 1)
    p1 = as.numeric(parameters[,1]);

  if ( o$model >= 2)
    p2 = as.numeric(parameters[,2]);

  if ( o$model == 3)
    p3 = as.numeric(parameters[,3]);

  for (k in 1:nrow(Dscore)) {
    d = as.numeric(Dscore[k,1]);
    if ( o$model == 1){
      res[k,] = 1 / ( 1 + ((1-d)*p1) / ((1 - p1) * d));
    }
    if ( o$model == 2){
      res[k,] = 1 / ( 1 + (((1-d)*p1) / ((1 - p1) * d))^p2);
    }
    if ( o$model == 3){
      res[k,] = p3 + (1-p3)*(1 / ( 1 + (((1-d)*p1) / ((1 - p1) * d))^p2));
    }
  }

  res[ res < 0.00001 ] <- 0.00001;
  res[ res > 0.99999 ] <- 0.99999;

  return(res);
}

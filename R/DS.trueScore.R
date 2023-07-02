DS.trueScore <- function(deltas, parameters, Dscore, o = DS.options())
{

  P = DS.PCR(parameters,Dscore,o);

  res = matrix(nrow = nrow(Dscore),ncol = 1);
  se  = matrix(nrow = nrow(Dscore),ncol = 1);
  for ( k in 1:nrow(Dscore) ) {
    res[k,] = sum( P[k,] * deltas ) / sum(deltas);
    se[k,] = sqrt( sum(deltas^2 * P[k,] * (1-P[k,]))) / sum(deltas);
  }

  return(list("trueScore" = res, "SE" = se));
}

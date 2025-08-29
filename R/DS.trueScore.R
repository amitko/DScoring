DS.trueScore <- function(deltas, parameters, Dscore, o = DS.options() 
{

  P = DS.PCR(parameters,Dscore,o);

  w = deltas / sum(deltas)

  res = matrix(nrow = nrow(Dscore),ncol = 1)
  se  = matrix(nrow = nrow(Dscore),ncol = 1)

  for ( k in 1:nrow(Dscore) ) {

      res[k,] = sum( P[k,] * deltas ) / sum(deltas)
      se[k,] = sqrt( sum(deltas^2 * P[k,] * (1-P[k,]))) / sum(deltas)
  }

  rel <- DS.reliability(deltas, parameters, Dscore, o = o, DscoreVAR = 0)
  return(
    list(
         "trueScore" = res,
         "SE" = se,
         "REL" = rel$REL
         )
    );
}

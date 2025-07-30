DS.trueScore <- function(deltas, parameters, Dscore, o = DS.options(), DscoreVAR = NULL)
{

  P = DS.PCR(parameters,Dscore,o);

  w = deltas / sum(deltas)

  res = matrix(nrow = nrow(Dscore),ncol = 1)
  se  = matrix(nrow = nrow(Dscore),ncol = 1)
  rel  = matrix(nrow = nrow(Dscore),ncol = 1)

  if (is.null(DscoreVAR)) {
    DscoreVAR = var(Dscore)
  }

  for ( k in 1:nrow(Dscore) ) {

      res[k,] = sum( P[k,] * deltas ) / sum(deltas)
      se[k,] = sqrt( sum(deltas^2 * P[k,] * (1-P[k,]))) / sum(deltas)

      NUM = sum( (w * parameters[,2] * P[k,] * (1-P[k,])) / (Dscore[k,]*(1 - Dscore[k,])))^2
      EVAR =  sum(w^2 * P[k,] * (1-P[k,]))
      TVAR = NUM * DscoreVAR
      SNR = TVAR/EVAR
      rel[k,] = SNR / (1 + SNR)
  }

  return(
    list(
         "trueScore" = res,
         "SE" = se,
         "REL" = rel
         )
    );
}

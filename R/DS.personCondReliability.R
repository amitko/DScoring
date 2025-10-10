DS.personCondReliability <- function(Dscore, deltas, parameters, o = DS.options(), DscoreVAR = NULL)
{
  w = deltas / sum(deltas)
  P = DS.PCR(parameters,Dscore,o)
  rel  = matrix(nrow = nrow(Dscore),ncol = 1)
  snr = matrix(nrow = nrow(Dscore),ncol = 1)

  for ( k in 1:nrow(Dscore) ) {
    NUM = sum( (w * parameters[,2] * P[k,] * (1-P[k,])) / (Dscore[k,]*(1 - Dscore[k,])))^2
    EVAR =  sum(w^2 * P[k,] * (1-P[k,]))
    TVAR = NUM * DscoreVAR
    snr[k,] = TVAR/EVAR
  }
  rel = snr/(1+snr)
  return(
        list(Reliability = rel,
             SNR = snr
             )
         )
}

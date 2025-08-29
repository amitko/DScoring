DS.reliability <- function(deltas, parameters, Dscore, o = DS.options(), DscoreVAR = NULL)
{

  P = DS.PCR(parameters,Dscore,o);

  w = deltas / sum(deltas)

  fit <- fitdistrplus::fitdist(Data[,1], "beta")

  res = matrix(nrow = nrow(Dscore),ncol = 1)
  se  = matrix(nrow = nrow(Dscore),ncol = 1)
  rel  = matrix(nrow = nrow(Dscore),ncol = 1)

  if (is.null(DscoreVAR)) {
    DscoreVAR <- var(Dscore)
  }

  if (DscoreVAR == 0) {
	DscoreVAR <- (fit$estimate[1]*fit$estimate[2])/(((fit$estimate[1]+fit$estimate[2])^2)*(fit$estimate[1] + fit$estimate[2] + 1))
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

  uD = unique(Dscore)
  mREL = 0

  for ( k in 1:lenght(uD) ) {
	ri = which(Dscore == uD[k])[1] 
	mREL = mREL + rel[ri,1] * dbeta(uD[k], fit$estimate[1],fit$estimate[2])
  }
  return(
    list(
         "REL" = rel,
	 "meanREL" = mean(rel),
	 "marginalREL" = mREL
         )
    );
}

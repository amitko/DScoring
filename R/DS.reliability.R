DS.reliability <- function(deltas, parameters, Dscore, SE,o = DS.options(), DscoreVAR = NULL)
{

  o$plausibleValuesDistribution = 'logitNormal'
  o$plausibleValues = 1


  Dscore[which(Dscore > 0.99)] = 0.99
  Dscore[which(Dscore < 0.01)] = 0.01
  pv <- DS.plausibleValues(DScores = Dscore, SE = SE, options = o)

  #P = DS.PCR(parameters,Dscore,o)

  P = DS.PCR(parameters,matrix(pv[,1], ncol = 1),o)

  w = deltas / sum(deltas)

  fit <- fitdistrplus::fitdist(as.numeric(Dscore), "beta")

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

      #NUM = sum( (w * parameters[,2] * P[k,] * (1-P[k,])) / (Dscore[k,]*(1 - Dscore[k,])))^2
      NUM = sum( (w * parameters[,2] * P[k,] * (1-P[k,])) / (pv[k,1]*(1 - pv[k,1])))^2
      EVAR =  sum(w^2 * P[k,] * (1-P[k,]))
      TVAR = NUM * DscoreVAR
      SNR = TVAR/EVAR
      rel[k,] = SNR / (1 + SNR)
  }

  mREL = 0

  edg <- seq(0.01, 0.99, by = 0.01)
  fD <- pbeta(edg, fit$estimate[1],fit$estimate[2])

  for ( k in 2:length(edg) ) {
	  ri = which( Dscore[,1] > edg[k-1] & Dscore[,1] <= edg[k])
	  if (length(ri) > 0) {
	    mREL = mREL + ( mean(rel[ri,1]) * (fD[k] - fD[k-1]) )
	  }
  }

  return(
    list(
         "REL"         = rel,
      	 "meanREL"     = mean(rel),
	       "marginalREL" = mREL,
         "DScoreDistrParams" = fit$estimate,
         "plausibleValues"   = pv
         )
    );
}

DS.reliability <- function(deltas, parameters, Dscore, SE,o = DS.options(), DscoreVAR = NULL)
{

  o$plausibleValuesDistribution = 'logitNormal'
  o$plausibleValues = 1

  Dscore[which(Dscore > 0.99)] = 0.99
  Dscore[which(Dscore < 0.01)] = 0.01
  pv <- DS.plausibleValues(DScores = Dscore, SE = SE, options = o)

  w = deltas / sum(deltas)

  fit <- fitdistrplus::fitdist(as.numeric(pv[,1]), "beta")

  if (is.null(DscoreVAR)) {
    DscoreVAR <- var(Dscore)
  }

  if (DscoreVAR == 0) {
	DscoreVAR <- (fit$estimate[1]*fit$estimate[2])/(((fit$estimate[1]+fit$estimate[2])^2)*(fit$estimate[1] + fit$estimate[2] + 1))
  }

  Rel  = DS.personCondReliability(Dscore, deltas, parameters, o = o, DscoreVAR = DscoreVAR)

  edg <- seq(0.01, 0.99, by = 0.01)
  fD <- dbeta(edg, fit$estimate[1],fit$estimate[2])

  P = DS.PCR(parameters,matrix(edg,ncol = 1),o)
  cR = DS.personCondReliability(matrix(edg,ncol = 1), deltas, parameters, o = o, DscoreVAR = DscoreVAR)

  rho = cR$Reliability

  Sc = matrix(nrow = length(edg), ncol = 1)
  Ec = matrix(nrow = length(edg), ncol = 1)
  for (k in 1:length(edg))
  {
    Sc[k,1] = sum(w * P[k,])
    Ec[k,1] = sum((w^2) * P[k,] * (1 - P[k,]))
  }

  TV = sum((Sc^2) * fD/sum(fD)) - sum(Sc * fD/sum(fD))^2
  EV = sum(Ec * fD/sum(fD))

  return(
    list(
         condReliability   = Rel$Reliability,
         condSNR           = Rel$SNR,
      	 mean              = mean(Rel$Reliability),
	       eREL              = sum(rho*fD)/sum(fD),
         mREL              = TV/(TV+EV),
         DScoreDistrParams = fit$estimate,
         plausibleValues   = pv,
         DScoreVariance    = DscoreVAR
         )
    );
}

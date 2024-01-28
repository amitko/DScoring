DS.aposteriorPlausibleValues <- function(itemResponse, DScores,itemParameters,options = DS.options())
{
  res = list()
  d = options$dScale

  # fit beta prior beta distribution
  fit <- fitdistrplus::fitdist(DScores[,1], distr =  "beta", method = "mme")

  # prior distribution at beans
  g = list()
  g$values = d
  g$probs = dbeta(d,fit$estimate[1],fit$estimate[1])

  PV = matrix(nrow = nrow(itemResponse), ncol = options$plausibleValues)

  res$posteriorDistribution <- matrix(nrow = nrow(itemResponse), ncol = length(g$values))
  res$plausibleValues <- matrix(nrow = nrow(itemResponse), ncol = options$plausibleValues)
  for (k in 1:nrow(itemResponse)) {

    #print(k)
    personResponse <- itemResponse[k,]
    pR <- matrix(rep(personResponse, length(g$values)),
                 ncol=ncol(itemResponse),
                 byrow=T
                )

    res$posteriorDistribution[k,] <- DS.aposteriorLikelihood(personResponse,itemParameters,g)
    pp = res$posteriorDistribution[k,]
    pp[which(is.nan(pp))] = 0.01
    res$plausibleValues[k,] <- sample(g$values,options$plausibleValues,
                                      prob = pp,
                                      replace=TRUE
                                      )

  }


  return(res);
}

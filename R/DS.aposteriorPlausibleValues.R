DS.aposteriorPlausibleValues <- function(itemResponse, DScores,itemParameters,options = DS.options())
{
  res = list()
  d = options$dScale

  # fit beta prior beta distribution
  fit <- fitdist(DScores[,1],distr = "beta", method = "mme")

  # prior distribution at beans
  g = list()
  g$values = d
  g$probs = dbeta(d,fit$estimate[1],fit$estimate[1])

  PV = matrix(nrow = nrow(itemResponse), ncol = options$plausibleValues)

  res$posteriorDistribution <- matrix(nrow = nrow(itemResponse), ncol = length(g$values))
  res$plausibleValues <- matrix(nrow = nrow(itemResponse), ncol = options$plausibleValues)
  for (k in 1:nrow(itemResponse)) {

    print(k)
    personResponse <- itemResponse[k,]
    pR <- matrix(rep(personResponse, length(g$values)),
                 ncol=ncol(itemResponse),
                 byrow=T
                )
    DS <- matrix(rep(DScores[k], length(g$values)),
                 ncol = 1,
                 byrow = T
                 )

    res$posteriorDistribution[k,] <- DS.aposteriorLikelihood(personResponse,itemParameters,g)
    res$plausibleValues[k,] <- sample(g$values,options$plausibleValues,prob = res$posteriorDistribution[k,])
  }


  return(res);
}

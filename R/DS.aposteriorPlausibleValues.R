DS.aposteriorPlausibleValues <- function(itemResponse, DScores,itemParameters,options = DS.options())
{
  res = list()
  #d = options$dScale
  d = DScores[,1]

  # fit beta prior beta distribution
  #fit <- fitdistrplus::fitdist(DScores[,1], distr =  "beta", method = "mme")

  bp <- beta_parameters(mean(DScores[,1]),sd(DScores[,1])^2)

  # prior distribution at beans
  g = list()
  g$values = d
  g$probs = dbeta(d,bp[1],bp[2])

  PV = matrix(nrow = nrow(itemResponse), ncol = options$plausibleValues)
  res$fitP1 <- fit$estimate[1]
  res$fitP2 <- fit$estimate[2]

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


beta_parameters <- function(m,s2) {
  a = m*(((m*(1-m))/(s2))-1)
  b = ((1-m)/m)*a
  return(c(a,b))
}

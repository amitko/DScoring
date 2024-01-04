DS.aposteriorLikelihood <- function (itemResponse, itemParameters, DScore, priorProbability ,priorDistribution ,o=DS.options() ) {

  if (nrow(itemResponse) != nrow(DScore) | nrow(DScore) != nrow(priorProbability)) {
    stop("itemResponse and DScore and priorProbability rows should match")
  }

  res <- matrix(nrow = nrow(DScore),ncol = 1);
  for (k in 1:nrow(DScore)) {
    lklh     <- DS.likelihood(as.numeric(itemResponse[k,]), itemParameters, as.matrix(DScore[k,1]), o)
    lklhB    <- DS.likelihood(as.numeric(itemResponse[k,]), itemParameters, as.matrix(priorDistribution$values),o)
    den      <- sum(lklhB*priorDistribution$probs)
    res[k,1] <- lklh * priorProbability[k] / den
  }

  return(res)

}

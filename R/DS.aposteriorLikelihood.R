DS.aposteriorLikelihood <- function (itemResponse, itemParameters, priorDistribution ,o=DS.options() ) {

  res <- matrix(ncol = length(priorDistribution$values),nrow = 1);
  #for ( k in 1:length(priorDistribution$values) ) {
    #lklh     <- DS.likelihood(as.numeric(itemResponse[k,]), itemParameters, as.matrix(DScore[k,1]), o)
    #lklhB    <- DS.likelihood(as.numeric(itemResponse[k,]), itemParameters, as.matrix(priorDistribution$values),o)
    lklhB    <- DS.likelihood(as.numeric(itemResponse), itemParameters, as.matrix(priorDistribution$values),o)
    den      <- sum(lklhB*priorDistribution$probs)
    #res[k,1] <- lklhB * priorDistribution$probs / den
    res <- lklhB * priorDistribution$probs / den
  #}

  return(res)

}

DS.aposteriorLikelihood <- function (itemResponse, itemParameters, priorDistribution ,o=DS.options() ) {

#  priorDistribution = list()
#  priorDistribution$values 
#  priorDistribution$probs


  res <- matrix(ncol = length(priorDistribution$values),nrow = 1);
    lklhB    <- DS.likelihood(as.numeric(itemResponse), itemParameters, as.matrix(priorDistribution$values),o)
    den      <- sum(lklhB*priorDistribution$probs)
    res <- lklhB * priorDistribution$probs / den

  return(res)

}

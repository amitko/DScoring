DS.NLogLikelihood <- function (itemResponse, itemParameters, DScore ,o=DS.options() ) {
  pp = DS.PCR(itemParameters,as.matrix(DScore),o);
  res = itemResponse * log(pp) + (1 - itemResponse) * log( 1 - pp)
  return(-sum(res))
}

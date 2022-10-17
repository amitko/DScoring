DS.itemMAD <- function(itemParameters, observedLogitDelta, o=DS.options() ) {

  res = matrix(ncol = 1, nrow = nrow(itemParameters))
  d_prev = 0;
  for (k in 1:nrow(itemParameters) ) {
    sk = which(observedLogitDelta[,k] > 0)
    prc = DS.PCR(matrix(itemParameters[k,], nrow = 1),matrix(o$dScale, ncol = 1),o)
    res[k] = mean(abs(prc[sk] - observedLogitDelta[sk,k]))
  }
  return(res)
}

DS.likelihood <- function (personResponse, itemParameters, DScore ,o=DS.options() ) {
  pp <- DS.PCR(itemParameters,as.matrix(DScore),o);

  res <- matrix(ncol = nrow(DScore),nrow = 1);
  for (k in 1:nrow(DScore)) {
    res[1,k] <- prod(( pp[k,]^personResponse ) * (( 1 - pp[k,])^(1 - personResponse)))
  }
  return(res)
}

DS.generateItemResponse <- function(itemParameters, D, o = DS.options()  ) {

  res = matrix(nrow = length(D), ncol = nrow(itemParameters))

  P = DS.PCR(itemParameters, D, o)

  for ( k in 1:length(D) ) {
      res[k,] = rbinom( nrow(itemParameters), 1, P[k,])
  }

  return(res)

}

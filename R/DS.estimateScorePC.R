DS.estimateScorePC <- function( itemData, dScores, itemParameters, o=DS.options() ) {

  res = matrix(ncol = 1, nrow = nrow(itemData));
  #for ( i in 1:nrow(itemData) ) {
  for ( i in c(10,500,1000,1500, 2000, 2500, 2900) ) {
    #bb = mle2(minuslogl = function(d) {mllklh( d =  d, itemScores = itemData[i,], itemParameters, o )},
    #               start = list('d' = 0.5) );
    bb = maxNM(fn = function(d) {mllklh( d =  d, itemScores = itemData[i,], itemParameters, o )},
                start = c('d' = dScores[i]),
  #              finalHessian = FALSE,
  #              grad = NULL,
  #              hess = NULL,
               #constraints = list('ineqA' = c(), 'ineqB' = c()),
               );

    res[i] = bb$estimate;
  }
  return( res );
}

mllklh <- function( d, itemScores, itemParameters, o) {
  #res = matrix(nrow = nrow(itemParameters), ncol = 1);
  res = 0

  if( d < 0.00001 || d > 0.99999 )
    return(NA)

  ff = itemScores * DS.itemImpact(matrix(d), itemParameters, o)  - log(1 + exp(DS.itemImpact(matrix(d), itemParameters, o)));


#  for ( j in 1:nrow(itemParameters) ) {
#        res = res - itemScores[j]*DS.itemImpact(matrix(d), itemParameters[j,], o)  + log(1 - DS.PCR(itemParameters[j,], matrix(d), o));
#  }

#  return(res)
  #  return( -sum(as.numeric(res)) );
  #return( -sum(ff) );
  return(ff)
}

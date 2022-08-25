DS.estimateScorePC <- function( itemData, dScores, itemParameters, o=DS.options() ) {
  
  res = matrix(ncol = 1, nrow = nrow(itemData));
  #for ( i in 1:nrow(itemData) ) {
  for ( i in c(10,500,1000,1500, 2000, 2500, 2900) ) {
    bb = mle2(minuslogl = function(d) {mllklh( d =  d, itemScores = itemData[i,], itemParameters, o )},
                  , start = list('d' = dScores[i]) );
  
    res[i] = bb@coef;
  }
  return( res );
}

mllklh <- function( d, itemScores, itemParameters, o) {
  res = 0;

  ff = itemScores * DS.itemImpact(matrix(d), itemParameters, o)  - log(1 - DS.PCR(itemParameters, matrix(d), o)); 

  res = - sum(ff);

  #for ( j in 1:nrow(itemParameters) ) {
  #      res = res - itemScores[j]*DS.itemImpact(matrix(d), itemParameters[j,], o)  - log(1 - DS.PCR(itemParameters[j,], matrix(d), o));
  #}
  return( res );
}
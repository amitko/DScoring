DS.estimateScorePC <- function( itemData, dScores, itemParameters, o=DS.options() ) {

  res = matrix(ncol = 1, nrow = nrow(itemData));
  #for ( i in 1:nrow(itemData) ) {
  #for ( i in c(10,500,1000,1500, 2000, 2500, 2900) ) {
  #  bb = mle2(minuslogl = function(d) {mllklh( d =  d, itemScores = itemData[i,], itemParameters, o )},
  #                 start = list('d' = dScores[i]) );
  #   bb = maxLik(logLik  = function(d) {mllklh( d =  d, itemScores = itemData[i,], itemParameters, o )},
  #              start = c('d' = dScores[i]),
  #              method = 'NR',
  #              finalHessian = FALSE,
  #              grad = NULL,
  #              hess = NULL,
               #constraints = list('ineqA' = c(), 'ineqB' = c()),
  #             );
   bb = maxLik(logLik  = function(d) {mllklh1( d =  d, itemScores = itemData, itemParameters, o )},
              start = c('d' = dScores),
              method = 'NR',
   );

    res[i] = bb$estimate;
  #  res[i] = bb@coef;
  #}
  return( res );
}

######################################

mllklh <- function( d, itemScores, itemParameters, o) {
  #res = matrix(nrow = nrow(itemParameters), ncol = 1);
  res = 0

  if( d < 0.00001 || d > 0.99999 )
    return(NA)

  ff = (itemScores-1) * DS.itemImpact(matrix(d), itemParameters, o)  - log(1 + hI(d,itemParameters[,1])^itemParameters[,2]);


#  for ( j in 1:nrow(itemParameters) ) {
#        res = res - itemScores[j]*DS.itemImpact(matrix(d), itemParameters[j,], o)  + log(1 - DS.PCR(itemParameters[j,], matrix(d), o));
#  }

#  return(res)
  #  return( -sum(as.numeric(res)) );
  return( sum(ff) );
  #return(ff)
}

###################################

mllklh1 <- function( d, itemScores, itemParameters, o) {

  ff = matrix(nrow = nrow(itemScores), ncol = 1);
  for ( i in 1:nrow(itemData) ) {

  ff[i] = ff[i] + (itemScores[i,]-1) * DS.itemImpact(matrix(d[i]), itemParameters, o)  - log(1 + hI(d[i],itemParameters[,1])^itemParameters[,2]);

  }

  return( sum(ff) );
}


###################################

hI <- function(d,b) {
  return( ((1-d)*b) / ((1-b)*d) )
}

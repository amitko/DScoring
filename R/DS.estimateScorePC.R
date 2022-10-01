DS.estimateScorePC <- function( itemData, dScores, itemParameters, o=DS.options() ) {

  #idx = c(1000:2000)
  idx = 1:nrow(itemData)
  res = list(Dscore = matrix(ncol = 1, nrow = nrow(itemData)));

    for ( i in idx ) {
    bb = maxLik(logLik  = function(d) {mllklh_score( d =  d, itemScores = itemData[i,], itemParameters, o )},
              start = c('d' = dScores[i]),
              method = 'NR',
             );

  res$Dscore[i] = bb$estimate;
  }

  return(res)

}

######################################

mllklh_score <- function( d, itemScores, itemParameters, o) {

  if( d < 0.00001 || d > 0.99999 )
    return(NA)
  ff = (1-itemScores) * itemParameters[,2] * log( hI(d,itemParameters[,1]) )- log(1 + hI(d,itemParameters[,1])^itemParameters[,2]);
  return( sum(ff) );
}

###################################
hI <- function(d,b) {
  return( ((1-d)*b) / ((1-b)*d) )
}

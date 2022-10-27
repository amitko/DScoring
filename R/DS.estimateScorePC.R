DS.estimateScorePC <- function( itemData, dScores, itemParameters, o=DS.options() ) {

  #library(maxLik)

  #idx = c(1000:1010)
  idx = 1:nrow(itemData)
  res = list(Dscore = matrix(ncol = 1, nrow = nrow(itemData)), SE = matrix(ncol = 1, nrow = nrow(itemData)));

    for ( i in idx ) {
    if (sum(as.numeric(itemData[i,])) == 0 ) {
      res$Dscore[i] = 0
      next
    }
    bb = maxLik::maxLik(logLik  = function(d) {mllklh_score( d =  d, itemScores = as.numeric(itemData[i,]), itemParameters, o )},
              start = c('d' = dScores[i]),
              method = 'NR',
             );

  res$Dscore[i] = bb$estimate;
  res$SE[i] = miscTools::stdEr(bb);
  }

  return(res)

}

######################################

mllklh_score <- function( d, itemScores, itemParameters, o) {

  if( d < 0.00001 || d > 0.99999 )
    return(NA)
  ff = (1-itemScores) * itemParameters[,2] * log( hI(d,itemParameters[,1]) )- log(1 + hI(d,itemParameters[,1])^itemParameters[,2]);
  return( sum( ff[!is.nan(ff)] ) );
}

###################################
hI <- function(d,b) {
  return( ((1-d)*b) / ((1-b)*d) )
}

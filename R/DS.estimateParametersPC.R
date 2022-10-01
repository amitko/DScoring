DS.estimateParametersPC <- function( itemData, dScores, itemParameters, o=DS.options() ) {

  idx = 1:ncol(itemData)
  res = list(Parameters = matrix(ncol = 2, nrow = ncol(itemData)));
  for ( i in idx ) {
    bb = maxLik(logLik  = function(p) {mllklh_item( p, itemScores = itemData[,i], dScores, o )},
                start = c('p' = c(itemParameters[i,1], itemParameters[i,2])),
                method = 'NM',
                #                finalHessian = FALSE,
                #                grad = NULL,
                #                hess = NULL,
                constraints = list('ineqA' = matrix(c(1,0,-1,0,0,1,0,-1),nrow = 4,ncol = 2,byrow = TRUE),
                                   'ineqB' = matrix(c(0,1,0,5),nrow = 4,ncol = 1,byrow = TRUE)
                                   ),
    );

    res$Parameters[i,] = bb$estimate;
  }

  return(res)

}

######################################

mllklh_item <- function( p, itemScores, Dscores, o) {

  pp = matrix(p, nrow = 1, ncol = 2);
  ff = (1-itemScores) * p[2]* log( hII(Dscores,p[1] )) - log(1 + hII(Dscores,p[1])^p[2]);
  return( sum(ff) );
}


###################################

hII <- function(d,b) {
  return( ((1-d)*b) / ((1-b)*d) )
}

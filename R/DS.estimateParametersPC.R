DS.estimateParametersPC <- function( itemData, dScores, itemParameters, o=DS.options() ) {

  #library(maxLik)

  b_lim_min = 0.01;
  b_lim_max = 0.99;

  s_lim_min = 0.01;
  s_lim_max = 5;

  idx = 1:ncol(itemData)
  res = list(Parameters = matrix(ncol = 2, nrow = ncol(itemData)), SE = matrix(ncol = 2, nrow = ncol(itemData)), MAD = matrix( matrix(ncol = 1, nrow = ncol(itemData)))  );
  for ( i in idx ) {
    bb = maxLik::maxLik(logLik  = function(p) {mllklh_item( p, itemScores = itemData[,i], dScores, o )},
                start = c('p' = c(itemParameters[i,1], itemParameters[i,2])),
                method = 'NM',
                #                finalHessian = FALSE,
                #                grad = NULL,
                #                hess = NULL,
                constraints = list('ineqA' = matrix(c(1,0,-1,0,0,1,0,-1),nrow = 4,ncol = 2,byrow = TRUE),
                                   'ineqB' = matrix(c(b_lim_min,b_lim_max,s_lim_min,s_lim_max),nrow = 4,ncol = 1,byrow = TRUE)
                                  )
    );

    rr = bb$estimate;
    if (rr[1] < b_lim_min) {
      rr[1] = b_lim_min
    }
    if (rr[2] < s_lim_min) {
      rr[2] = s_lim_min
    }
    if (rr[1] > b_lim_max) {
      rr[1] = b_lim_max
    }
    if (rr[2] > s_lim_max) {
      rr[2] = s_lim_max
    }

    res$Parameters[i,] = rr;
    res$SE[i,] = miscTools::stdEr(bb);
  }

  res$MAD = DS.itemMAD(matrix(res$Parameters, ncol=2, nrow = ncol(itemData) ),DS.observedLogitDelta(itemData, dScores, o), o)
  return(res)

}

######################################

mllklh_item <- function( p, itemScores, Dscores, o) {

  pp = matrix(p, nrow = 1, ncol = 2)
  ff = (1-itemScores) * p[2]* log( hII(Dscores,p[1] )) - log(1 + hII(Dscores,p[1])^p[2])
  return( sum(ff[!is.nan(ff) ] ))
}


###################################

hII <- function(d,b) {
  return( ((1-d)*b) / ((1-b)*d) )
}

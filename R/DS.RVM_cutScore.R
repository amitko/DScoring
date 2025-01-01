DS.RVM_cutScore <- function(responseVector, itemParameters, o=DS.options(),opt = NULL ) {
  latentDScore<-DS.estimateScorePC(responseVector,rep(0.5,nrow(responseVector)),itemParameters,o)

  Ds = as.matrix(o$dScale);
  lklh = matrix(nrow = nrow(responseVector),ncol = nrow(Ds) )
  for (l in 1:nrow(responseVector) ) {
    for (k in 1:nrow(Ds)) {
      lklh[l,k] = -DS.NLogLikelihood(responseVector[l,],itemParameters,Ds[k],o)
    }
  }


  maxL <- apply(lklh, 1, max)
  cS   <- Ds[apply(lklh, 1, which.max)]

  return(list("likelihood" = lklh,
              'MaxLklh' = maxL,
              'cutScore' = cS
              )
         )
}


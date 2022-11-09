DS.RVM_cutScore <- function(responseVector, itemParameters, o=DS.options(),opt = NULL ) {
  latentDScore<-DS.estimateScorePC(responseVector,0.5,itemParameters,o)

  Ds = as.matrix(o$dScale);
  lklh = matrix(nrow = 1,ncol = nrow(Ds) )
  for (k in 1:nrow(Ds)) {
    lklh[k] = -DS.NLogLikelihood(responseVector,itemParameters,Ds[k],o)
  }

  return(list("likelihood" = lklh,
              'MaxLklh' = max(lklh),
              'cutScore' = Ds[which.max(lklh)]
              )
         )
}


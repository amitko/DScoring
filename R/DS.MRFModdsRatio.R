DS.MRFModdsRatio <- function(parameters,Dscore) {

# for Multidimensional RFM
# (b, s_{1},...,s_{k})

  res = matrix(nrow = nrow(Dscore),ncol = nrow(parameters))

  for (k in 1:nrow(Dscore)) 
  {
    d = as.numeric(Dscore[k,])
    
	or = rep(1,nrow(parameters))
	
    for ( l in 1:length(d) )
    {
      or = or*(((1-d[k])/d[k])^parameters[,k+1])
    }
    res[k,] = or * (parameters[,1] / ( 1 - parameters[,1] ) )
  }

  res[ res < 0.00001 ] <- 0.00001;
  res[ res > 0.99999 ] <- 0.99999;

  return(res);
}

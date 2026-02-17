DS.MPCR <- function(parameters,Dscore,o = DS.options()) {

  res = matrix(nrow = nrow(DScore), ncol = nrow(parameters))
 
  res = 1 / (1 + DS.MRFModdsRatio(parameters, DScore)) 
	
  res[ res < 0.00001 ] <- 0.00001;
  res[ res > 0.99999 ] <- 0.99999;

  return(res);
}

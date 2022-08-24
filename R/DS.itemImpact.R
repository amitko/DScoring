DS.itemImpact <- function ( dscore, parameters, o=DS.options() ) {
  p = DS.PCR(parameters, dscore, o);
  
  if (p < 0.000001 | p > 0.999999 | is.nan(p) ) {
    return(0)
  }
  else {
   return ( log(p/(1-p)) );
  }
}
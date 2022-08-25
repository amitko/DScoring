DS.itemImpact <- function ( dscore, parameters, o=DS.options() ) {
  p = DS.PCR(parameters, dscore, o);
  return ( log(p/(1-p)) );
}

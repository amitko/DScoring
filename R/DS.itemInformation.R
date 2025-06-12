DS.itemInformation <- function ( parameters, dscore ,o=DS.options() ) {
  p = DS.PCR(parameters, dscore, o);
  
  return (p*(1-p)*parameters[,2]^2)/ (dscore^2 * (1-dscore)^2)
}

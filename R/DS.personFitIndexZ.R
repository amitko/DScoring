DS.personFitIndexZ <- function(Dscores = NULL, item_parameters=NULL,item_response = NULL,o = DS.options(), personLikelihood = NULL)
{
  # based on
  # D. Dimitrov, R. Smith. Adjusted Rasch Person-Fit Statistics. J. of
  # Applied measurement. 2006
  
  # Dimitar Atanasov, 2025
  # datanasov@nbu.bg
  
  if ( is.null(personLikelihood))
  {
    personLikelihood = DS.personLikelihood(Dscores,item_parameters,item_response)
  }
  
  # Eqn 4
  result <- (personLikelihood$likelihood - personLikelihood$expected) / sqrt (personLikelihood$variance)
  
  return( list(
          Z = result,
          index = result < -2
          ))
}
  
  
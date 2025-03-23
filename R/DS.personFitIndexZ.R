DS.personFitIndexZ <- function(personLikelihood)
{
  # based on
  # D. Dimitrov, R. Smith. Adjusted Rasch Person-Fit Statistics. J. of
  # Applied measurement. 2006
  
  # Dimitar Atanasov, 2025
  # datanasov@nbu.bg
  
  # Eqn 4
  result <- (personLikelihood$likelihood - personLikelihood$expected) / sqrt (personLikelihood$variance)
  
  return(result)
}
  
  
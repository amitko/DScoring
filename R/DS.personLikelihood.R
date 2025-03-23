DS.personLikelihood <- function(Dscores,item_parameters,item_response,o = DS.options())
{
# Calculates the likelihood for a specific response pattern in item_response from a
# person with ability in dScores, over a set of items in item_parameters.
#
#
# based on
# D. Dimitrov, R. Smith. Adjusted Rasch Person-Fit Statistics. J. of
# Applied measurement. 2006
#

# Dimitar Atanasov, 2025
# datanasov@nbu.bg

    probOfPerformance <- DS.PCR(item_parameters, matrix(Dscores, ncol = 1),o)
    
    likelihood <- matrix(nrow = length(Dscores), 1)
    expected   <- matrix(nrow = length(Dscores), 1)
    variance   <- matrix(nrow = length(Dscores), 1)
    
    for ( k in 1:nrow(probOfPerformance) )
    {
      # Eqn 5
      likelihood[k,1] <- sum(item_response[k,] * log(probOfPerformance[k,]) + ( 1 - item_response[k,]) * log(1 - probOfPerformance[k,]))
      
      # Eqn 6
      expected[k,1] = sum( probOfPerformance[k,] * log(probOfPerformance[k,])  + (1 - probOfPerformance[k,]) * log(1 - probOfPerformance[k,]) )
      
      # Eqn 7
      variance[k,1] = sum( probOfPerformance[k,] * ( 1 - probOfPerformance[k,]) * (  log (probOfPerformance[k,] /  (1 - probOfPerformance[k,]) )^2  ))
    }
    
    
    return(list ( likelihood = likelihood,
                  expected   = expected,
                  variance   = variance
                  ) )
}
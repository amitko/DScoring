DS.personFitU <- function(Dscores,item_parameters,item_response,o = DS.options())
{
  sortedParams <- item_parameters[order(item_parameters[,1]),]
  
  result <- matrix(nrow = length(Dscores), ncol = 1)

  prc <- DS.PCR(sortedParams,matrix(Dscores, ncol = 1),o)
  prc[prc > 0.9999] <- 0.999
  Lnd <- log(prc/(1 - prc ))
  
  for (k in 1:length(Dscores))
  {
    
    GuttmanResponse <- as.numeric(sortedParams[,1] <= Dscores[k])
    ReverseGuttman  <- as.numeric(sortedParams[,1]  > Dscores[k])
    
    result[k,] =  ( sum(Lnd[k,] * GuttmanResponse) - sum(Lnd[k,] * item_response[k,])) / (sum(Lnd[k,] * GuttmanResponse) - sum(Lnd[k,] * ReverseGuttman))

  }
  return(result)
}
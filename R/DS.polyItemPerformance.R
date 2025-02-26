DS.polyItemPerformance <- function(itemParameters, DScores, o = DS.options())
{
  
  res = DS.PCR(itemParameters, DScores, o)
#  for ( k in 1:nrow(itemParameters) )
#  {
#    res = rbind(res, )
#  }
 return(res) 
}
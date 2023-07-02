DS.equatingParametersRFM <- function(baseTestParameters, testParameters, commonItems, type = 'direct', o = DS.options() )
{
  rescaledParams = matrix(ncol = ncol(testParameters), nrow = nrow(testParameters))
  const = DS.equatingConstants(baseTestParameters[,1], testParameters[,1], commonItems = commonItems)
  rescaledParams[,1] = DS.equatingRescale(testParameters[,1],constants = const)

  shapeConstants = list()

  if ( ncol(testParameters) > 1 )
  {
    if ( type == 'direct' )
    {
      shapeConstants$A = sd(baseTestParameters[,2])/sd(testParameters[,2])
      shapeConstants$B = mean(baseTestParameters[,2]) - shapeConstants$A * mean(testParameters[,2])
      rescaledParams[,2] = shapeConstants$A*testParameters[,2] + shapeConstants$B
    }
  }

  return( list(
            'parameters'     = rescaledParams,
            'constants'      = const,
            'shapeConstants' = shapeConstants
          ))
}

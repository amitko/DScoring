DS.equatingDScoresRFM <- function(testParams, rescaledTestParams, DScores, o = DS.options(), type = 'm1') {
  print('Not testes DS.equatingDScoresRFM')
  if (type == 'm1') {
    P = DS.PCR(testParams,DScores,o)
    if ( ncol(rescaledTestParams) == 2 ){
      R = ( (1-P) / P )^(1/rescaledTestParams[,2])
    }
    else {
      R = ( (1-P) / P )
    }

    Di = rescaledTestParams[,1]/( rescaledTestParams[,1] + (1 - rescaledTestParams[,1])*R )

  }
  return( mean(Di) )
}

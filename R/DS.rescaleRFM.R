DS.rescaleRFM <- function(NewTestParams,BaseTestParams,commonItems,o = DS.options(), method = 'direct') {

  NewTestRescaled = matrix(nrow=nrow(NewTestParams),ncol=ncol(NewTestParams))
  ConstB <- DS.equatingConstants(baseTestDeltas = BaseTestParams[,1], newTestDeltas = NewTestParams[,1],commonItems)

  NewTestRescaled[,1] = DS.equatingRescale(NewTestParams[,1],constants = ConstB)

  if ( ncol(NewTestParams) > 1 ) {
    if ( method == "direct" ) {
      sA = sd(BaseTestParams[,2])/sd(NewTestParams[,2])
      sB = mean(BaseTestParams[,2]) - sA * mean(NewTestParams[,2])
      NewTestRescaled[,2] =  NewTestParams[,2]*sA +sB
    }
    if ( method == "trough_a") {

    }

  }

  if ( ncol(NewTestParams) > 2 ) {
    NewTestRescaled[,3] = NewTestParams[,3]
  }

  return(NewTestRescaled)

}


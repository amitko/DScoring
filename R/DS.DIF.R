DS.DIF <- function( focalParameters, referenceParameters ,o=DS.options(), alpha = 0.05 ) {


  focalParamsRescaled = DS.rescaleRFM(focalParameters, referenceParameters, matrix(c(1:nrow(focalParameters),1:nrow(referenceParameters)), ncol=2, byrow = FALSE ))

  dSt = seq(0.05,0.95,0.03)
  dScale = matrix(dSt,nrow = length(dSt))
  focalPCR <- DS.PCR(focalParamsRescaled,dScale,o)
  referencePCR <- DS.PCR(referenceParameters,dScale,o)

  fZ = z_transform(focalPCR)
  rZ = z_transform(referencePCR)

  HA = list()
  for ( k in 1:nrow(focalParameters) ) {
    HA[[k]] = var.test(fZ[,k],rZ[,k]);
  }

  HB = list()
  for ( k in 1:nrow(focalParameters) ) {
    HB[[k]] = t.test(fZ[,k],rZ[,k]);
  }

  DIF = rep(0,nrow(focalParameters))
  STATS = matrix(nrow = nrow(focalParameters),ncol=5)
  for ( k in 1:nrow(focalParameters) ) {
    if ( HA[[k]]$p.value < alpha ) {
      DIF[k] = 1
    }
    if ( HA[[k]]$p.value < alpha & HB[[k]]$p.value < alpha ) {
      DIF[k] = 2
    }
    STATS[k,] = c(DIF[k],as.numeric(HA[[k]]$statistic),as.numeric(HA[[k]]$p.value), as.numeric(HB[[k]]$statistic), as.numeric(HB[[k]]$p.value))
  }

  STATS = data.frame(STATS)
  colnames(STATS) <- c("DIF", "F.stat", "F.p.value", "T.stat", "T.p.value")
  return(list(
  "DIF" = DIF,
  "HA" = HA,
  "HB" = HB,
  "STATS" = STATS
  ))
}

################################################
z_transform <- function (values) {
  return ( (1/1.702)*log(values / (1 - values)) )
}

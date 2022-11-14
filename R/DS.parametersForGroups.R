DS.parametersForGroups <- function(itemData, focalIndicator) {

  focal<-which(focalIndicator == 1)
  reference<-which(focalIndicator == 0)

  focalDelta = DS.deltaBootstrap(itemData[focal,]);
  focalScore = DS.personDscore(itemData[focal,],focalDelta$delta)
  focalParams = DS.logitDeltaFit(itemData[focal,],Dscore = focalScore)

  referenceDelta = DS.deltaBootstrap(itemData[reference,]);
  referenceScore = DS.personDscore(itemData[reference,],referenceDelta$delta)
  referenceParams = DS.logitDeltaFit(itemData[reference,],Dscore = referenceScore)

  return(list(
    "focal" = focalParams,
    "reference" = referenceParams
  ))
}

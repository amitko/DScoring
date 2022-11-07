DS.estimatePC <- function( itemData, DScore = NULL, itemDelta = NULL, start = list("parameters" = NULL, "DScores"=NULL) ,o=DS.options() ) {

  if ( is.null(itemDelta) ) {
    db<-DS.deltaBootstrap(itemData)
    itemDelta = db$delta
  }

  if ( is.null( DScore ) ) {
    DScore<-DS.personDscore(itemData,db$delta)
  }

  if (is.null(start$parameters) ) {
    fit<-DS.logitDeltaFit(itemData,DScore,o)
    start$parameters = fit$parameters;
  }

  if ( is.null(start$DScores )) {
    start$DScores = DScore;
  }
  LatentParameters <- DS.estimateParametersPC(itemData, DScore, start$parameters, o)
  LatentDScores <- DS.estimateScorePC(itemData,start$DScore, LatentParameters$Parameters, o)

  return(list("Persons" = LatentDScores,
              "Items"   = LatentParameters
              ))
}

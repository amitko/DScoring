library("DScoring")

# Load item response data
  itemData = read.csv('item_scores.csv', header = FALSE)

# Estimate of expected item difficulty ('delta')
  db<-DS.deltaBootstrap(itemData)
  db

# Compute classical D-scores
  PS<-DS.personDscore(itemData,db$delta)
  PS

# Select RFM model (1=RFM1, 2=RFM2, 3=RFM3)
  o = DS.options()
  o$model = 2

# Estimate item parameters (nonlinear RFM regression)
  Fit<-DS.logitDeltaFit(itemData,Dscore = PS,o)
  Fit

# Compute true D-scores
  TS<-DS.trueScore(deltas = db$delta, parameters = Fit$parameters, Dscore = PS)
  write.csv(data.frame(PS,TS$trueScore,TS$SE),"D-scores.csv")

# Estimate Latent item parameters (only for RFM2)
  lParameters <- DS.estimateParametersPC(itemData, PS, Fit$parameters, o)
  write.csv(data.frame(lParameters$Parameters, lParameters$SE, lParameters$MAD),"latent-item-parameters.csv")

# Estimate Latent DScores (only for RFM2)
  lDScores <- DS.estimateScorePC(itemData,PS, lParameters$Parameters, o)
  write.csv(data.frame(lDScores$Dscore, lDScores$SE),"latent-DScores.csv")

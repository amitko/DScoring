library("DScoring")
library("boot")
itemData = read.csv('item_scores.csv')
  db<-DS.deltaBootstrap(itemData)
  db
  PS<-DS.personDscore(itemData,db$delta)
  PS
  Fit<-DS.logitDeltaFit(itemData,Dscore = PS)
  Fit
  TS<-DS.trueScore(deltas = db$delta, parameters = Fit$parameters, Dscore = PS)


  itemData2 = read.csv('item_scores.csv')
  db2<-dS.deltaBootstrap(itemData2)

  #Equating
  constants = DS.equatingConstants(db$delta,db2$delta,matrix( c(1, 3, 5, 10, 7, 15, 8, 4, 11, 5), nrow = 5, ncol = 2, byrow = TRUE))
  rescaledDeltas = DS.equatingRescale(db2$delta, constants)


  # Latent
  library(bbmle)
  library(tictoc)
  tic("start")
  ls = DS.estimateScorePC(itemData, PS, Fit$parameters)
  toc()

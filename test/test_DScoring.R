library("DScoring")

  itemData = read.csv('item_scores.csv')
  db<-DS.deltaBootstrap(itemData)
  db
  write.csv(db,"delta.csv")

  PS<-DS.personDscore(itemData,db$delta)
  PS

  Fit<-DS.logitDeltaFit(itemData,Dscore = PS)
  Fit
  write.csv(data.frame(Fit$parameters, Fit$SE, Fit$MAD),"item-parameters.csv")

  TS<-DS.trueScore(deltas = db$delta, parameters = Fit$parameters, Dscore = PS)
  write.csv(data.frame(PS,TS$trueScore,TS$SE),"D-scores.csv")

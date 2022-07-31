library("deltaScoring")
library("boot")
itemData = read.csv('item_scores.csv')
  db<-dS.deltaBootstrap(itemData)
  db
  PS<-dS.personDscore(itemData,db$delta)
  PS
  Fit<-dS.logitDeltaFit(itemData,Dscore = PS$Dscores)
  Fit
  TS<-dS.trueScore(deltas = db$delta, parameters = Fit$parameters, Dscore = PS$Dscores)
  
  
  itemData2 = read.csv('item_scores.csv')
  db2<-dS.deltaBootstrap(itemData2)
  
  #Equating
  constants = dS.equatingConstants(db$delta,db2$delta,matrix( c(1, 3, 5, 10, 7, 15, 8, 4, 11, 5), nrow = 5, ncol = 2, byrow = TRUE))
  rescaledDeltas = dS.equatingRescale(db2$delta, constants)
  
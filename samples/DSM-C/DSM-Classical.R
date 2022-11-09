library("DScoring")

# Load item response data
itemData = read.csv('item_scores.csv', header = FALSE)

# Estimate the expected item difficulty ('delta')
db<-DS.deltaBootstrap(itemData)
db$delta

# Compute classical D-scores
PS<-DS.personDscore(itemData,db$delta)
PS

# Select RFM model (1=RFM1, 2=RFM2, 3=RFM3)
o = DS.options()
o$model = 2

# Estimate item parameters (nonlinear RFM regression)
Fit<-DS.logitDeltaFit(itemData,Dscore = PS,o)
Fit$parameters

write.csv(db$delta, "cls-delta.csv")
D<-list("Dw" = PS)
write.csv(D, "cls-DScore.csv")
write.csv(data.frame(Fit$parameters,Fit$MAD), "cls-parameters.csv")




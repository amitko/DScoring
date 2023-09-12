test_that("bootstrapping Deltas",{

db<-DS.deltaBootstrap(DS.data.itemScores)
db
PS<-DS.personDscore(DS.data.itemScores,db$delta)
PS
Fit<-DS.logitDeltaFit(itemData = DS.data.itemScores,Dscore = PS)
expect_type(db,'list')
expect_type(PS,'double')
expect_type(Fit,'list')
}
)

test_that("bootstrapping Deltas",{

itemData = read.csv('item_scores.csv')

db<-DS.deltaBootstrap(itemData)
db
PS<-DS.personDscore(itemData,db$delta)
PS
Fit<-DS.logitDeltaFit(itemData = itemData,Dscore = PS$Dscores)
expect_type(db,'list')
expect_type(PS,'list')
expect_type(Fit,'list')
}
)

DS.equatingDScores <- function(newTestDScore, baseTestDscore) {

  baseTestDscore[which(baseTestDscore == 0 | is.na(baseTestDscore) ) ] = 0.001
  newTestDScore[which(newTestDScore == 0 | is.na(newTestDScore) )]   = 0.001

  baseTestDscore[which(baseTestDscore == 1)] = 0.999
  newTestDScore[which(newTestDScore == 1)]   = 0.999

  Const = DS.equatingConstants(baseTestDscore,newTestDScore);
  Zx <- log(newTestDScore/(1 - newTestDScore))/1.702
  RZx <- (Const$A * Zx) + Const$B
  return(1/(1 + exp(-1.702 * RZx)))

}

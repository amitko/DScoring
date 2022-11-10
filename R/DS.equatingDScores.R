DS.equatingDScores <- function(newTestDScore, baseTestDscore) {

  Const = DS.equatingConstants(newTestDScore,baseTestDscore);
  Zx <- log(newTestDScore/(1 - newTestDScore))/1.702
  RZx <- (Const$A * Zx) + Const$B
  return(1/(1 + exp(1.702 * RZx)))

}

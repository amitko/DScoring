DS.polySCR <- function( itemParameters, o = DS.options() )
{

  x <- o$dScale;

  clr <- c("black", "red", "green", "blue", "purple", "brown")
  lt <- 1
  cl <- 1
  prc <- DS.polyItemPerformance(itemParameters, DScores = matrix(o$dScale, nrow = length(o$dScale)))

  prc <- cbind(rep(1,nrow(prc)), prc)
  prc <- cbind(prc, rep(0,nrow(prc)))

  plot(o$dScale, prc[,1] - prc[,2], type = 'n', xlim = c(0,1), ylim = c(0,1), xlab = "D-score", ylab = "Probability for correct response")
  legType <- c()
  legClr <- c()
  legTitl <- c()
  for (k in 1:(ncol(prc) - 1) )
  {
    lines(o$dScale, prc[,k] - prc[,k+1], lty = lt, col = clr[cl] )
    legType <- c(legType, lt)
    legClr <- c(legClr,clr[cl])
    legTitl <- c(legTitl, paste('Category ', as.character(k-1), sep = ''))
    lt <- lt +1
    if (lt == 6) {
      cl <- cl+1
    }
  }
  legend("top", legend = legTitl, lty = legType, col = legClr)
}

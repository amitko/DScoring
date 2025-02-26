DS.polyCCR <- function( itemParameters,o = DS.options() )
{
  x <- o$dScale;
  
  clr <- c("black", "red", "green", "blue", "purple", "brown")
  lt <- 1
  cl <- 1
  prc <- DS.polyItemPerformance(itemParameters, DScores = matrix(o$dScale, nrow = length(o$dScale)))
 
  plot(o$dScale, prc[,1] - prc[,2], type = 'n', xlim = c(0,1), ylim = c(0,1), xlab = "D-score", ylab = "Probability for correct response")
  legType <- c()
  legClr <- c()
  legTitl <- c()
  
  for ( k in 1:nrow(itemParameters) )
  {
    lines(o$dScale, prc[,k], lty = lt, col = clr[cl] )
    
    legType <- c(legType, lt)
    legClr <- c(legClr,clr[cl])
    legTitl <- c(legTitl, paste('Fitted ', as.character(k), sep = ''))
    lt <- lt +1
    if (lt == 6) {
      cl <- cl+1
    }  
  }
  legend("topleft", legend = legTitl, lty = legType, col = legClr) 
}
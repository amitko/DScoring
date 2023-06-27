DS.equatingConstants <- function(baseTestDeltas, newTestDeltas, commonItems = NULL)
{
  baseTestDeltas[baseTestDeltas > 0.99999] <- 0.99999
  baseTestDeltas[baseTestDeltas < 0.00001] <- 0.00001

  newTestDeltas[newTestDeltas > 0.99999] <- 0.99999
  newTestDeltas[newTestDeltas < 0.00001] <- 0.00001

  Zb = log(baseTestDeltas / (1 - baseTestDeltas)) / 1.702 ;
	Zn = log(newTestDeltas  / (1 - newTestDeltas)) / 1.702;

	if ( ! is.null(commonItems) ) {
	  Zbc = Zb[commonItems[,1]];
	  Znc = Zn[commonItems[,2]];
	}
	else {
	  Zbc = Zb
	  Znc = Zn
	}
	A = sd(Zbc)/sd(Znc);
	B = mean(Zbc) - A * mean(Znc);

	return( list( "A" = A, "B" = B) );

}

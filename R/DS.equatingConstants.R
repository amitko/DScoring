DS.equatingConstants <- function(baseTestDeltas, newTestDeltas, commonItems = NULL)
{
  baseTestDeltas[which(baseTestDeltas == 0)] = 0.001
  newTestDeltas[which(newTestDeltas == 0)]   = 0.001

  baseTestDeltas[which(baseTestDeltas == 1)] = 0.999
  newTestDeltas[which(newTestDeltas == 1)]   = 0.999

	Zb = 1/1.702 * log(baseTestDeltas / (1 - baseTestDeltas));
	Zn = 1/1.702 * log(newTestDeltas  / (1 - newTestDeltas));

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

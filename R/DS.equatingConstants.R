DS.equatingConstants <- function(baseTestDeltas, newTestDeltas, commonItems = NULL)
{


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

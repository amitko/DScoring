DS.equatingConstants <- function(baseTestDeltas, newTestDeltas,commonItems)
{
	Zb = 1/1.702 * log(baseTestDeltas / (1 - baseTestDeltas));
	Zn = 1/1.702 * log(newTestDeltas / (1 - newTestDeltas));
	
	Zbc = Zb[commonItems[,1]];
	Znc = Zn[commonItems[,2]];
	
	A = sd(Zbc)/sd(Znc);
	B = mean(Zbc) - A * mean(Znc);
	
	return( list( "A" = A, "B" = B) );
	
}
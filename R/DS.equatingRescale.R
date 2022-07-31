DS.equatingRescale <- function(deltas, constants)
{
	Z = 1/1.702 * log(deltas/(1-deltas));
	Zx = constants$A * Z + constants$B;
	res = (1 / (1 + exp(-1.702 * Zx)));
	
	return(res);
}
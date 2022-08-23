DS.estimatePC <- function( itemData, o=DS.options() ) {

o$model = 2;

# init values
b = rep(0.5,ncol(itemData));
s = rep(1,ncol(itemData));
d = rep(0.5,nrow(itemData));

res = 0;
  for ( n in 1:nrow(itemData) ) {
	  for ( j in 1:ncol(itemdata) ) {
		  res = itemData[n,j]*logProb(d[n],b[j],s[x]) - log(1 + exp(logProb(d[n],b[j],s[x])));
	  }
  }		  

  return(res);
}

logProb <- function(theta, b, s) {
	res = log(1 + ( (1-theta) /theta) )^s + (b/(1-b))^s;
	return(res);
}
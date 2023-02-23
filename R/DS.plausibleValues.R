DS.plausibleValues <- function(DScores,SE,options = DS.options())
{
  res = matrix(nrow = nrow(DScores), ncol = options$plausibleValues);

  for (k in 1:nrow(DScores) ) {
        m = DScores[k];
        s2 = SE[k]^2;
        a = m*(((m*(1-m))/(s2))-1);
        b = ((1-m)/m)*a;
        res[k,] = rbeta(options$plausibleValues, a, b);
        #res[k,] = runif(options$plausibleValues, max( c(DScores[k]-2*SE[k],0)), min(c(DScores[k]+2*SE[k],1) ));
  }

  return(res);
}


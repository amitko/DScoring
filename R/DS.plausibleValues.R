DS.plausibleValues <- function(DScores,SE,options = DS.options())
{
  res = matrix(nrow = nrow(DScores), ncol = options$plausibleValues);

  kk = 1.08

  for (k in 1:nrow(DScores) ) {
        m = DScores[k];
        s2 = SE[k]^2;

        if ( m < .99 & m > 0.01 ) {
          a = m*(((m*(1-m))/(s2))-1);
          b = ((1-m)/m)*a;
          res[k,] = rbeta(options$plausibleValues, a, b)*kk;
          if (any(is.nan(res[k,]))) {
            print(m)
            print(s2)
          }
          #res[k,] = runif(options$plausibleValues, max( c(DScores[k]-2*SE[k],0)), min(c(DScores[k]+2*SE[k],1) ));
        }
        else {
            res[k,] = rep(m, options$plausibleValues)
        }
  }

  return(res);
}


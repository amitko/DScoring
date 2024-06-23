DS.plausibleValues <- function(DScores,SE,options = DS.options())
{
  res = matrix(nrow = nrow(DScores), ncol = options$plausibleValues);

  kk = 1.2

  for (k in 1:nrow(DScores) ) {
        m = DScores[k];
        s2 = SE[k]^2;

        if ( m < .99 & m > 0.01 ) {

          if ( options$plausibleValuesDistribution == 'beta' ) {
            a = m*(((m*(1-m))/(s2))-1);
            b = ((1-m)/m)*a;
            res[k,] = rbeta(options$plausibleValues, a, b)*kk;
            if (any(is.nan(res[k,]))) {
              print(m)
              print(s2)
            }
          }

          if ( options$plausibleValuesDistribution == 'logitNormal' ) {
            T = log( m/(1-m))
            seT = SE[k]/sqrt(m*(1-m))
            res[k,] = logitnorm::rlogitnorm(n = options$plausibleValues, mu = T, sigma = seT)*kk
          }

        }
        else {
            res[k,] = rep(m, options$plausibleValues)
        }
  }

  return(res);
}


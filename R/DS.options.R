DS.options <- function(){
  return(list("dScale" = seq(from = 0, to = 1, by = 0.05),
              "model"  = 2,
               "Models" = c(
                "y ~ 1/(1+(((1-x)*b)/(x*(1-b))))",
                "y ~ 1/(1+((((1-x)*b)/(x*(1-b)))^s))",
                "y ~ c + ((1-c)/(1+(((1-x)*b)/(x*(1-b)))^s))"
               ),
              "modelCoefficients" = c("b", "s", "c", "d"),
              "bruteForceSstep" = 0.01,
              "plausibleValues" = 10,
              "plausibleValuesDistribution" = 'beta'
              )
        );
}

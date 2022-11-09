DS.logitDeltaFit <- function(itemData,Dscore,o = DS.options(), algorithm = 'nls' ) {

  oldt = DS.observedLogitDelta(itemData,Dscore,o);

   if (o$model == 1) {
     startList = list("b" = 0.5);
     lowerList = list("b" = 0.01);
     upperList = list("b" = 0.99);
   }
   else if (o$model == 2) {
     startList = list("b" = 0.5, "s" = 1);
     lowerList = list("b" = 0.01, "s" = 0.3);
     upperList = list("b" = 0.99, "s" = 5);

   }
  else if (o$model == 3) {
    startList = list("b" = 0.5, "s" = 1, "c" = 0.1);
    lowerList = list("b" = 0.01, "s" = 0.3, "c" = 0);
    upperList = list("b" = 0.99, "s" = 5, "c" = 0.5);
  }

params    = data.frame(matrix(ncol = o$model, nrow = ncol(oldt)))
params_se = data.frame(matrix(ncol = o$model, nrow = ncol(oldt)))
params_p  = data.frame(matrix(ncol = o$model, nrow = ncol(oldt)))
mad       = data.frame(matrix(ncol = 1,       nrow = ncol(oldt)))

models = list()
for (k in 1:ncol(oldt)) {
  tt = oldt[,k];
  tt[which(tt == 0)] = 0.001;
  dd = data.frame(o$dScale,tt);
  colnames(dd) <- c('x','y');
  if (algorithm == 'nls' ) {
    m<-nls(o$Models[o$model],data = dd,
           start = startList,
           lower = lowerList,
           upper = upperList,
           algorithm = "port"
            );
  }
  if (algorithm == 'nls2') {
    st2<-expand.grid(b= db$delta, s=seq(lowerList$s, upperList$s ,o$bruteForceSstep))
    m<-nls2::nls2(o$Models[o$model],data = dd,
            start = st2,
            algorithm = "brute-force"
            );
  }
  models[[k]] = m;
  params[k,] = summary(m)$coefficients[,1];
  params_se[k,] = summary(m)$coefficients[,2];
  params_p[k,] = summary(m)$coefficients[,4];
}

colnames(params) <- o$modelCoefficients[1:o$model];
colnames(params_se) <- o$modelCoefficients[1:o$model];
colnames(params_p) <- o$modelCoefficients[1:o$model];

return(list("parameters"   = params,
            "pValues"      = params_p,
            "SE"           = params_se,
            "formula"      = o$Models[o$model],
            "fittedModels" = models,
            "MAD"          = DS.itemMAD(params,oldt,o)
            ));



}

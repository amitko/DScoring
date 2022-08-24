DS.logitDeltaPlot <- function(Fit, items = 1:nrow(Fit$parameters), o = DS.options() ) {
 
  library(RColorBrewer);
  
  clr <- brewer.pal(9,'Set1');
  pType  = 0;
  cType = 1;
  
  plot(o$dScale,predict(Fit$fittedModels[[items[1]]]), type="o", col=clr[cType], 
                        pch = pType, 
                        ylab = "Probability for correct performance", 
                        xlab = "D-score",
                        lty=1)
  
  legCol = c();
  legP = c();

  legCol = c(legCol, clr[cType]);
  legP = c(legP, pType);
  
  cType = cType + 1;
  
  if (length(items) == 1) {
    return();
  }
  
  for (i in 2:length(items)) {
    points(o$dScale,predict(Fit$fittedModels[[items[i]]]),pch = pType,col=clr[cType]);
    lines(o$dScale,predict(Fit$fittedModels[[items[i]]]),col=clr[cType],lty=1);
    
    legCol = c(legCol, clr[cType]);
    legP = c(legP, pType);
    
    
    cType = cType + 1;
    if (cType == 10) {
      cType = 1;
      pType = pType + 1;
    }
    if (pType == 25) {
      pType = 0;
    }
  }
  
  legend("topleft", legend=items,
        col = legCol,
        pch = legP,
        lty = 1
        );
  
  
}
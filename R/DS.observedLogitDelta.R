DS.observedLogitDelta <- function(itemData,Dscore,o = DS.options() ) {
  
  d_prev = 0;
  R = data.frame((matrix(ncol = ncol(itemData), nrow = length(o$dScale))));
  for (k in 1:length(o$dScale) ) {
    I = which(Dscore <= o$dScale[k] & Dscore > d_prev );
    for (i in 1:ncol(itemData)) {
      if (length(I) == 0) {
        R[k,i] = 0;
      }
      else { 
        prop = sum(itemData[I,i])/length(I);
        R[k,i] = prop;
      }
    }
    d_prev = o$dScale[k];
  }
  
  return(R);
}
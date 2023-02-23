DS.lsdm <- function(itemPerformance, Q, type) {

  if (type == 1) {
    a = 0;
    b = 0;
  }
  if (type == 2) {
    a = 1;
    b = 1;
  }
  if (type == 3) {
    a = 1;
    b = 0;
  }
  if (type == 4) {
    a = 0;
    b = 1;
  }

  cols <- ncol(itemPerformance);
  nAttr <- ncol(Q);
  attrPerf = matrix(nrow = nAttr, ncol = nrow(itemPerformance));
  for (k in 1:nrow(itemPerformance)) {
      logPerf <- log(itemPerformance[k,])*(1-b) + log(1 - itemPerformance[k,])*b;
      ss <- stats::optim( rep(-1,nAttr), fn=l2norm, y=as.matrix(logPerf, nrow=cols), Q=as.matrix(Q));
      attrPerf[,k] <- ss$par;
  }

  return( exp(attrPerf)*(1-a) + (1-exp(attrPerf))*a );


}

l2norm <- function(x,y,Q) {
  d <- x - t(Q) %*% y;
  return(sum(d^2));
}



# Estimate item Deltas by bootstrapping
# Returns list with $delta, $conf

library(boot);

DS.deltaBootstrap <- function(itemData) {

  res = matrix(ncol = 1, nrow = ncol(itemData));
  colnames(res) <- c('delta');
  ci  = matrix(ncol = 2, nrow = ncol(itemData));
  se  = matrix(ncol = 1, nrow = ncol(itemData));
  N   = nrow(itemData);
  colnames(ci) <- c('0.05','0.95');
  for(i in 1:ncol(itemData)) {
    b<-boot::boot(data=itemData[,i],statistic = bmean,1000);
    bm<-median(b$t);
    res[i,] = bm;
    ci[i,]  = quantile(b$t, probs = c(0.05, 0.95), names = FALSE);
    se[i]   = sqrt(var(b$t)/nrow(b$t));
  }
  return(list("delta" = res,"conf" = ci, "se" = se));
}

bmean <- function(dat,indices) {
  d <- dat[indices];
  return(1-mean(d));
}


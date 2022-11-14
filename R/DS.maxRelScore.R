DS.maxRelScore<-function(itemData) {

  colnames(itemData)<-paste0('item',1:ncol(itemData))

  # Raykov, Dimitrov

  model<-paste('factor',paste(paste0('item',1:ncol(itemData)),collapse='+'),sep='=~ NA*')
  cc<-"factor ~~ 1*factor"

  fit<-lavaan::cfa(c(model,cc),itemData)

  dd = diag(lavaan::inspect(fit,"cov.ov"))
  ff = lavaan::inspect(fit,"std")
  w = ff$lambda/sqrt(dd)

  DM = matrix(ncol=1,nrow=nrow(itemData))

  sw = sum(w)
  for (k in 1:nrow(itemData) ) {
    DM[k] = sum((w / sw )* itemData[k,]);
  }

  return(DM)
}

DS.personDscore <- function(itemData, deltas, type="relative_to_d" ) {
  res = matrix(ncol = 1, nrow = nrow(itemData));
  for(k in 1:nrow(itemData)) {
    if (type == "total") {
      res[k,] = sum(as.numeric(itemData[k,]) * deltas)
    }
    if (type == "relative_to_d") {
      res[k,] = sum(as.numeric(itemData[k,]) * deltas)/ sum(deltas);
    }
    if (type == "relative_to_n") {
      res[k,] = sum(as.numeric(itemData[k,]) * deltas)/ nrow(itemData);
    }
    
  }
  return( res);
}
DS.matchingVector <- function(mat, vec) {
  t(apply(mat, 1, function(row) as.integer(row == vec)))
}
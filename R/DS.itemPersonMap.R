DS.itemPersonMap <- function(itemDelta, personScores, N_of_bins = 20) {

  range = (0:N_of_bins)/N_of_bins;
  hgI <- hist(itemDelta, breaks = range, plot = FALSE)
  hgP <- hist(personScores, breaks = range, plot = FALSE)


  z = matrix(c(hgI$counts/sum(hgI$counts),hgP$counts/sum(hgP$counts)),nrow = 2,byrow = TRUE)
  barplot(height = z,beside = TRUE)
}

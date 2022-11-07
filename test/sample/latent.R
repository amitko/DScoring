library("DScoring")

# Load item response data
itemData = read.csv('item_scores.csv', header = FALSE)

# Latent estimation of the RFM2(default) model
# Default values of the optimization parameters
# i.e. starting points, tolerance, etc are used
latent=DS.estimatePC(itemData)

latent$Persons$Dscore
latent$Items$Parameters

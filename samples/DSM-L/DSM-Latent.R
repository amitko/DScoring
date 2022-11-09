library("DScoring")

# Load item response data
itemData = read.csv('item_scores.csv', header = FALSE)

# Latent estimation of the RFM2(default) model
# Default values of the optimization parameters
# i.e. starting points, tolerance, etc. 
latent=DS.estimatePC(itemData)

latent$Persons$Dscore
latent$Items$Parameters

P = data.frame(latent$Items$Parameters, latent$Items$MAD)
colnames(P)<-c('b','s','MAD')
write.csv(P,'latent_parameters.csv')

D=data.frame(latent$Persons$Dscore,latent$Persons$SE)
colnames(D)<-c('DL','SE')
write.csv(D,'latent_DScores.csv')

library(DScoring)
itemParameters = read.csv("parameters.csv",header = FALSE)
responseVector = read.csv("response_vector.csv",header = FALSE)

RVM = DS.RVM_cutScore(responseVector,itemParameters)

RVM$cutScore

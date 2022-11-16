library(DScoring)

itemData = read.csv("item_scores.csv",header=FALSE)
focalIndicator = read.csv("focal.csv",header=FALSE)

parameters = DS.parametersForGroups(itemData, focalIndicator)
DIF = DS.DIF(parameters$focal$parameters,parameters$reference$parameters)
DIF$STATS

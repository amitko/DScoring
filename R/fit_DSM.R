# ======= Fit DSM classic and latent =========
fit_DSM <- function(no_output = FALSE, inputFile = NULL, plots = FALSE, itemData = NULL)
{
library(tools)

# ================ READ DATA =============
if (is.null(itemData)) {
	if ( is.null(inputFile)) {
		message("Choose RESPONSE data file. First column is PersonID and there is a HEADER ROW!!!")
		inputFile <- file.choose()
	}
	data <- read.csv(file = inputFile, header = TRUE)
}
else {
	data <- itemData
}

# ================ CHECK DATA =============
if ( all(sapply(data[,-1], function(x) all(x %in% c(0,1))) ) )   # dichotomous
{
   itemData <- data[,-1]
   Poly <- NULL
   OL <- names(itemData)
   itemDataD <- DS.poly2dih(data[,-1])
} else {    # polytpmous
   itemDataD <- DS.poly2dih(data[,-1])
   itemData <- data.frame(itemDataD$Response)
   colnames(itemData) <- itemDataD$Poly$Labels
   Poly <- itemDataD$Poly$Items
   OL <- itemDataD$Org$Labels
}

# ================ FIT DATA ===============
itemDelta  <- DS.deltaBootstrap(itemData)
DScore     <- DS.personDscore(itemData, itemDelta$delta)
fit <- NULL
itemParams <- tryCatch(
  {
    fit <- DS.logitDeltaFit(itemData,DScore)
    DS.estimateParametersPC(itemData, DScore, fit$parameters)
    #when it throws an error, the following block catches the error
  }, error = function(msg){
    # try with different start point
    fit <- NULL
    sp <- matrix(c(rep(0.5,ncol(itemData)), rep(1,ncol(itemData))), nrow = ncol(itemData), byrow = FALSE)
    DS.estimateParametersPC(itemData, DScore, sp)
  })
DScoreL <- tryCatch(
  {
    DS.estimateScorePC(itemData,DScore,itemParams$Parameters)
    #when it throws an error, the following block catches the error
  }, error = function(msg){
    # try with different start point
    list(Dscore = DScore, SE = rep(NA,length(DScore)))
  }
)

TrueScore <- DS.trueScore(deltas = itemDelta$delta,parameters = itemParams$Parameters, Dscore = DScore)

Aberrant <- list( U = DS.personFitU(DScoreL$Dscore, item_parameters = itemParams$Parameters, item_response = itemData),
                  Z = DS.personFitIndexZ(personLikelihood = DS.personLikelihood(DScoreL$Dscore,item_parameters = itemParams$Parameters, item_response = itemData))
                )


# ============== PREPARE RESULTS ==========
colnames(itemParams$Parameters) <- c('b','s')
colnames(itemParams$SE) <- c('SE_b','SE_s')

itemRES <- data.frame( Item    = names(itemData),
                        delta   = itemDelta$delta,
                        dSE     = itemDelta$se,
                        iP      = itemParams$Parameters,
                        iSE     = itemParams$SE,
                        iMAD    = itemParams$MAD
                        )
personRES <- data.frame( PersonID = data[,1],
                          DScore   = DScore,
                          DScoreL  = DScoreL$Dscore,
                          DScoreLSE = DScoreL$SE,
                          TrueScore = TrueScore$trueScore,
                          TrueScoreSE = TrueScore$SE,
                          TrueScoreREL = TrueScore$REL,
                          IndexU   = Aberrant$U,
                          IndexZ   = Aberrant$Z$Z,
                          Aberrant = Aberrant$Z$index
                          )
if ( ! no_output )
{
  write.table(itemRES,paste(file_path_sans_ext(inputFile),'_items.csv',sep = ''), sep = ",", row.names = FALSE)
  write.table(personRES,paste(file_path_sans_ext(inputFile),'_persons.csv',sep = ''), sep = ",", row.names = FALSE)
}

if ( plots ) {

for ( k in 1:length(itemDataD$Poly$Items) ) {
    if ( itemDataD$Poly$Items[[k]]$is == 0 )  # Dichotomous
    {
      if ( !is.null(fit) )
      {
        png(filename = paste(file_path_sans_ext(inputFile),'-', OL[k] ,'.png',sep = '') )
        DS.logitDeltaPlot(Fit = fit, items = k)
        dev.off()
      } else {
        message("Plots unavailable!!!")
      }
    }
    else
    {
        png(filename = paste(file_path_sans_ext(inputFile) ,OL[k], '-CCR.png',sep = '') )
        DS.polyCCR(itemParameters = itemRES[Poly[[k]]$items,c(4,5)])
        dev.off()
        png(filename = paste(file_path_sans_ext(inputFile) ,OL[k], '-SCR.png',sep = '') )
        DS.polySCR(itemParameters = itemRES[Poly[[k]]$items,c(4,5)])
        dev.off()
    }
}



return(
  list(items   = itemRES,
       persons = personRES,
       poly    = itemDataD$Poly$Items
       )
      )
}
}

# ======= Fit DSM classic and latent =========
fit_DSM <- function(no_output = FALSE, inputFile = NULL)
{
library(tools)

# ================ READ DATA =============
if ( is.null(inputFile)) {
  message("Choose RESPONSE data file. First column is PersonID and there is a HEADER ROW!!!")
  inputFile <- file.choose()
}

  data <- read.csv(file = inputFile, header = TRUE)


# ================ CHECK DATA =============
if ( all(sapply(data[,-1], function(x) all(x %in% c(0,1))) ) )   # dichotomous
{
   itemData <- data[,-1]
} else {    # polytpmous
   itemData <- DS.poly2dih(data[,-1])$Response
}

# ================ FIT DATA ===============
itemDelta  <- DS.deltaBootstrap(itemData)
DScore     <- DS.personDscore(itemData, itemDelta$delta)
itemParams <- tryCatch(
  {
    DS.estimateParametersPC(itemData, DScore, DS.logitDeltaFit(itemData,DScore)$parameters)
    #when it throws an error, the following block catches the error
  }, error = function(msg){
    # try with different start point
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

# ============== PREPARE RESULTS ==========
colnames(itemParams$Parameters) <- c('b','s')
colnames(itemParams$SE) <- c('SE_b','SE_s')

itemRES <- data.frame( Item    = names(data[,-1]),
                        delta   = itemDelta$delta,
                        dSE     = itemDelta$se,
                        iP      = itemParams$Parameters,
                        iSE     = itemParams$SE,
                        iMAD    = itemParams$MAD
                        )
personRES <- data.frame( PersonID = data[,1],
                          DScore   = DScore,
                          DScoreL  = DScoreL$Dscore,
                          DScoreSE = DScoreL$SE
                          )
if ( ! no_output )
{
  write.table(itemRES,paste(file_path_sans_ext(inputFile),'_items.csv',sep = ''), sep = ",", row.names = FALSE)
  write.table(personRES,paste(file_path_sans_ext(inputFile),'_persons.csv',sep = ''), sep = ",", row.names = FALSE)
}
return(
  list(items   = itemRES,
       persons = personRES
       )
)
}

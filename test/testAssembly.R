
data <- read.csv("../test/dataAssembly.csv", header = FALSE)

ss = DS.assemblySingleTest(nOfItems = 20,
                           itemDeltas = data$V4,
                           meanOfDeltas = 0.4,
                           meanOfDeltasTolerance = 0.1,
                           requiredItems = c(100, 120),
                           excludedItems = c(1:50)
                          )


ss = DS.assemblyMultipleTest( nOfTests = 3,
                              nOfItems = 10,
                              itemDeltas = data$V4,
                              meanOfDeltas = 0.5,
                              meanOfDeltasTolerance = 0.3,
                              requiredItems = matrix(data = c(100, 120, 101, 121, 102, 122), nrow = 3, byrow = TRUE),
                              excludedItems = c(500 : 700),
                            )


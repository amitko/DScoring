DS.assemblySingleTest <- function(nOfItems, itemDeltas,
                                  o = DS.options(),
                                  excludedItems = NULL,
                                  requiredItems = NULL,
                                  addEqualitiesLHS = NULL,
                                  addEqualitiesRHS = NULL,
                                  addInequalitiesLHS = NULL,
                                  addInequalitiesRHS = NULL,
                                  meanOfDeltas = NULL,
                                  meanOfDeltasTolerance = 0.3,
                                  deltaDistribution = NULL,
                                  targetFunction = rep(1,numItems)
                                  )

{

  numItems <- length(itemDeltas)
  lb <- rep(0,numItems)
  ub <- rep(1,numItems)

  IntCon <- 1:numItems

  #if ( is.null(meanOfDeltas) ) {
  #  w = seq(from = 0, to = 1, by = 1/(length(deltaDistribution)-1) )
  #  meanOfDeltas <- weighted.mean(w,deltaDistribution)

  #}

  # === Equality constraints ====

  # number of items in the test
  Ae <- rep(1,numItems);
  be <- nOfItems;


  # === Inequality constraints ====

  A <- matrix(nrow = 0, ncol = length(itemDeltas))
  b <- c();

  if (!is.null(meanOfDeltas)) {
    # sum of deltas greater then the meanOfDeltas - meanOfDeltasTolerance;
    A <- rbind(A, -itemDeltas / nOfItems);
    b <- c(b, -meanOfDeltas + meanOfDeltasTolerance);

    # sum of deltas less then the meanOfDeltas + sumOfDeltasTolerance;
    A <- rbind(A, itemDeltas / nOfItems);
    b <- c(b, +meanOfDeltas + meanOfDeltasTolerance);
  }

  # excluded items forced to be 0 : Ax = 0
  if ( !is.null(excludedItems) )
  {
    zr <- rep(0,numItems)
    zr[excludedItems]  = 1
    Ae <- rbind(Ae, zr)
    be <- c(be, 0)
  }

  if ( !is.null(deltaDistribution) )
  {
    if ( rowSums(deltaDistribution) != nOfItems )
    {
      stop("Distribution of deltas does not match nOfItems")
    }

    nRange <- ncol(deltaDistribution)


    # FIXME : Check mean of deltaDistribution to correspond to required
    # meanOfDeltas and meanOfDeltasTolerance,

    #expectedMeanDelta <- (deltaDistribution ./ nOfItems) * ((0:(nRange - 1)) /(nRange - 1))';

    for ( k in 0:(nRange-1))
    {
      AA <- rep(0, numItems)
      itemsInRange <- which(itemDeltas > k/nRange & itemDeltas <= (k+1)/ nRange)
      AA[itemsInRange] <- 1
      Ae <- rbind(Ae, AA)
      be <- c(be, deltaDistribution[k+1] )
    }

  }


# required items forced to be 1 : Ax = size(requiredItems)
  if ( !is.null(requiredItems) )
  {
    zr <- rep(0,numItems)
    zr[requiredItems] <- 1
    Ae <- rbind(Ae, zr)
    be <- c(be, length(requiredItems) )
  }


# additional equalities
  if ( !is.null(addEqualitiesLHS) )
  {
    Ae <- rbind(Ae, addEqualitiesLHS)
    be <- c(be, addEqualitiesRHS)

  }

# additional inequalities
if ( !is.null(addInequalitiesLHS) && nrow(addInequalitiesLHS) > 0)
{
  A <- rbind(A, addInequalitiesLHS)
  b <- c(be, addInequalitiesRHS)

}

  sol = modopt.matlab::intlinprog(targetFunction,IntCon,A,b,Ae,be,lb,ub)

  return ( which(sol$x == 1) )

}


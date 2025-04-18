DS.assemblyMultipleTest <- function( nOfTests, nOfItems, itemDeltas,
                                    o = DS.options(),
                                    excludedItems = NULL,
                                    requiredItems = NULL,
                                    addEqualitiesLHS = NULL,
                                    addEqualitiesRHS = NULL,
                                    addInequalitiesLHS = NULL,
                                    addInequalitiesRHS = NULL,
                                    meanOfDeltas = NULL,
                                    meanOfDeltasTolerance = NULL,
                                    deltaDistribution = NULL,
                                    targetFunction = rep(1,numItems)
                                    )
{

# ====== Check =====
  if ( !is.null(requiredItems)  )
  {
    if ( nrow(requiredItems) != nOfTests )
    stop("Required items does not match teh number of tests!!!")
  }

  # ====== Init Values =====
  numItems <- length(itemDeltas)

  result <- matrix(nrow = 0, ncol = nOfItems)

  for ( k in 1:nOfTests )
  {
    # === Equality constraints ====

    shadowTestScale <- (nOfTests - k)

    if ( shadowTestScale == 0 )
    {
      f <- targetFunction
    } else {
      f <-  c( targetFunction, targetFunction )
    }

    # number of items in the test
    if ( shadowTestScale == 0 )
    {
      Ae <- rep(1,numItems)
      be <- nOfItems
    } else {
      Ae = matrix( data = c( rep(1,numItems), rep(0,numItems), rep(0,numItems) , rep(1,numItems) ), nrow = 2, byrow = TRUE)
      be = c( nOfItems, nOfItems * shadowTestScale )
    }

    # === Inequality constraints ====
    A <- matrix(nrow = 0, ncol = (length(itemDeltas) * 2 ))
    b <- c()

    if ( !is.null(deltaDistribution) )
    {
      if ( sum(deltaDistribution) != nOfItems )
      {
        stop("Distribution of deltas does not match nOfItems")
      }

      nRange <- length(deltaDistribution)

      for ( k in 0:(nRange-1))
      {
        AA <- rep(0, numItems)
        itemsInRange <- which(itemDeltas > k/nRange & itemDeltas <= (k+1)/ nRange)
        AA[itemsInRange] <- 1

        if ( shadowTestScale == 0 )
        {
          Ae = rbind(Ae, AA)
          be = c(be, deltaDistribution[k+1] )
        }  else {
          Ae = rbind(Ae, c(AA, rep(0,numItems) ), c(rep(0,numItems), AA))
          be = c(be, deltaDistribution[k+1], deltaDistribution[k+1] * shadowTestScale )
        }
      }

    }

    # excluded items forced to be 0 : Ax = 0
    if ( !is.null(excludedItems) )
    {
      if ( shadowTestScale == 0 )
      {
        zr = rep(0,numItems)
        zr[excludedItems] <- 1
      }  else {
        zr <- rep(0,numItems * 2);
        zr[ c(excludedItems, excludedItems + numItems) ] <- 1;
      }
      Ae <- rbind(Ae, zr)
      be <- c(be, 0)
    }

    # required items forced to be 1 : Ax = size(requiredItems)
    if ( !is.null(requiredItems) )
    {
      reqItems <- requiredItems[k,which(requiredItems[k,] > 0)]
      if ( shadowTestScale == 0 )
      {
        zr <- rep(0,numItems);
      } else {
        zr = rep(0,numItems * 2);
      }

      zr[reqItems] <- 1;
      Ae <- rbind(Ae, zr)
      be <- c(be, length(requiredItems[k,]) )
    }

    # additional equalities
    if ( !is.null(addEqualitiesLHS) )
    {
      if ( shadowTestScale == 0 )
      {
        Ae <- rbind(Ae, addEqualitiesLHS)
        be <- c(be, addEqualitiesRHS)
      } else {
        Ae <- rbind(Ae, cbind( addEqualitiesLHS, matrix(data = rep(0, (ncol(addEqualitiesLHS)*nrow(addEqualitiesLHS))), ncol=ncol(addEqualitiesLHS))))
        be <- c(be, addEqualitiesRHS)
      }

    }

    # additional inequalities
    if ( !is.null(addInequalitiesLHS) )
    {
      if (nrow(addInequalitiesLHS) > 0 ) {
      if ( shadowTestScale == 0 )
      {
        A <- rbind(A, addInequalitiesLHS)
        b <- c(b, addInequalitiesRHS)
      } else {
        A <- rbind(A, cbind( addInequalitiesLHS, matrix(data = rep(0,ncol(addInequalitiesLHS)*nrow(addInequalitiesLHS), ncol=ncol(addInequalitiesLHS)))))
        b <- c(b, addInequalitiesRHS)
      }
      }
    }

    if ( shadowTestScale == 0 )
    {
      ID <- itemDeltas
    } else {
      ID <- c(itemDeltas, itemDeltas)
    }


    if (shadowTestScale == 0)
    {
      f = targetFunction;
    } else {
      f = c( targetFunction, targetFunction);
    }


    inCurrentTest <- DS.assemblySingleTest(nOfItems + nOfItems*shadowTestScale,
                                           ID,
                                           addEqualitiesLHS = Ae,
                                           addEqualitiesRHS = be,
                                           addInequalitiesLHS = A,
                                           addInequalitiesRHS = b,
                                           targetFunction = f,
                                           meanOfDeltasTolerance = meanOfDeltasTolerance,
                                           meanOfDeltas = meanOfDeltas,
                                           deltaDistribution = deltaDistribution
                                           )
    if ( is.null(inCurrentTest) )
    {
        inCurrentTest = rep(0,nOfItems);
        return(result)
    }

    result = rbind(result, inCurrentTest[1:nOfItems])

    excludedItems = c(excludedItems, inCurrentTest[1:nOfItems])

  } # for nOfTests loop

  return(result)
}

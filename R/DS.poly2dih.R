DS.poly2dih <- function(Response)
{

  DIHscores <- matrix(nrow = nrow(Response), ncol = 0)
  Poly <- list(Labels = c(),
               Items = list()
               )
  Org <- list( Labels = c(),
               isPoly = matrix(ncol = ncol(Response), nrow = 1)
              )
  c <- 1
  
  cn <- colnames(Response)
  
  for ( k in 1:ncol(Response) )
    {
    if ( all( Response[,k] %in% c(0, 1) ) )  # dihotomous
    {
      DIHscores <- cbind(DIHscores, Response[,k])
      Poly$Labels <- c(Poly$Labels, paste(as.character(c), '-', cn[k] , '-' , as.character(k)));
      p <- list( is = 0,
                 items = c
                )
      Poly$Items[[k]] <- p
      Org$Labels <- c(Org$Labels, paste(cn[k] , as.character(k)))
      Org$isPoly[1,k] <- 0
      c <- c+1
    } else {      #polytomous
      levels = sort(unique(Response[,k]))
      pp = c()
      for (l in levels[-1])
      {
        Correct <- as.integer(Response[,k] >= l)
        DIHscores <- cbind( DIHscores, Correct)
        Poly$Labels =  c(Poly$Labels, paste(as.character(c), '-', cn[k] , '-' , as.character(k), '[', as.character(l), ']', sep = ''))
        pp <- c(pp, ncol(DIHscores))
        c <- c+1
      }
      Org$Labels = c(Org$Labels, paste('+', cn[k], ' ', as.character(k), sep = ''))
      Org$isPoly[1,k] <- 1
      p <- list( is = 1,
                 items = pp
      )
      Poly$Items[[k]] <- p
    }
  }

  colnames(DIHscores) <- Poly$Labels
  return( list(Response = DIHscores,
               Poly = Poly,
               Org = Org
               )
          )
}

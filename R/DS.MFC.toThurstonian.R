library(r2r)

DS.MFC.toThurstonian <- function(data, blocks, traits = null)
{

if ( is.null(traits) ) {
    traits = rep(1,lenghth(blocks))
}

itemsInTrait <- hashmap()

# i1i2, i1i3, i2i3
m3 <- hashmap()
m3[[c(1,2,3)]] <- c(1,1,1)
m3[[c(1,3,2)]] <- c(1,1,0)
m3[[c(3,1,2)]] <- c(0,0,1)
m3[[c(2,1,3)]] <- c(0,1,1)
m3[[c(2,3,1)]] <- c(1,0,0)
m3[[c(3,2,1)]] <- c(0,0,0)

# i1i2
m2 <- hashmap()
m2[[c(1,2)]] <- 1
m2[[c(1,2)]] <- 0

# i1i2, i1i3, i1i4, i2i3, i2i4, i3i4
m2 <- hashmap()
m2[[c(1,2,3,4)]] <- c(1,1,1,1,1,1)
m2[[c(2,1,3,4)]] <- c(0,1,1,1,1,1)
m2[[c(3,1,2,4)]] <- c(1,0,1,0,1,1)
m2[[c(1,3,2,4)]] <- c(1,1,1,0,1,1)
m2[[c(2,3,1,4)]] <- c(0,0,1,1,1,1)
m2[[c(3,2,1,4)]] <- c(0,0,1,0,1,1)
m2[[c(3,2,4,1)]] <- c(0,0,0,0,1,1)
m2[[c(2,3,4,1)]] <- c(0,0,0,1,1,1)
m2[[c(4,3,2,1)]] <- c(0,0,0,0,0,0)
m2[[c(3,4,2,1)]] <- c(0,0,0,0,0,1)
m2[[c(2,4,3,1)]] <- c(0,0,0,1,1,0)
m2[[c(4,2,3,1)]] <- c(0,0,0,1,0,0)
m2[[c(4,1,3,2)]] <- c(1,1,0,0,0,0)
m2[[c(1,4,3,2)]] <- c(1,1,1,0,0,0)
m2[[c(3,4,1,2)]] <- c(1,0,0,0,0,1)
m2[[c(4,3,1,2)]] <- c(0,1,0,0,0,1)
m2[[c(1,3,4,2)]] <- c(1,1,1,0,0,1)
m2[[c(3,1,4,2)]] <- c(1,0,1,0,0,1)
m2[[c(2,1,4,3)]] <- c(0,1,1,1,1,0)
m2[[c(1,2,4,3)]] <- c(1,1,1,1,1,0)
m2[[c(4,2,1,3)]] <- c(0,1,0,1,0,0)
m2[[c(2,4,1,3)]] <- c(0,0,1,1,1,0)
m2[[c(1,4,2,3)]] <- c(1,1,1,1,0,0)
m2[[c(4,1,2,3)]] <- c(1,1,0,1,0,0)

last <- 0
result <- data.frame(matrix(NA, nrow = nrow(data), ncol = 0))

for (blk in unique(blocks))
{
  items_in_block <- which(blocks == blk)

  if ( length(items_in_block) == 2 )
  {
    map <- m2
  } else if ( length(items_in_block) == 3 )
  {
    map <- m3
  } else if ( length(items_in_block) == 4 )
  {
    map <- m4
  } else
  {
    stop('Unsupported block size!!!')
  }


  colNames <- c()

  CC = combn(items_in_block,2)
  for (ii in 1:ncol(CC))
  {
    last <- last + 1
    colNames <- c(colNames, paste( "i", as.character(CC[,ii][1]), "i", as.character(CC[,ii][2]), sep = ''))
    itemsInTrait[[ as.numeric(traits[ CC[,ii][1] ]) ]] <- c(itemsInTrait[[ as.numeric(traits[ CC[,ii][1] ]) ]], last )
    itemsInTrait[[ as.numeric(traits[ CC[,ii][2] ]) ]] <- c(itemsInTrait[[ as.numeric(traits[ CC[,ii][2] ]) ]], last )
  }

  dataT <- matrix(ncol = ncol(CC), nrow = nrow(data));

  blockData = data[,items_in_block]
  for (r in 1:nrow(blockData) ) {
    dataT[r,] <- map[[ as.numeric( blockData[r,] ) ]]
  }

  dataTF = data.frame(dataT)
  colnames(dataTF) <- colNames;

  result <- cbind(result, dataTF )
}

return (
        list(
            ThurstonianData = result,
            itemsInTrait   = itemsInTrait
        )
        )
}

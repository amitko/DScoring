# DS.MFC.cleanData
# data - data to be cleaned
# blocks - vector, indicating from which block is the item
# negative_indicatior - vector with size number of blocks, 1 if items are negative within the block

DS.MFC.cleanData <- function(data, blocks, negative_block_indicatior)
{

    data[is.na(data)] <- 0

    skipped = matrix(ncol=2,nrow=0)

    responses = matrix(ncol = ncol(data), nrow = 0)


    for (k in 1:nrow(data) ) {

      skip = 0;

      response <- c()
      for ( blk in unique(blocks) ) {

        itemData = as.numeric(data[k,])[which(blocks == blk)]

        itemData[is.na(itemData)] <- 0

        n = length(itemData)
        if (sum(itemData) != n*(n+1)/2) {
          skip = 1
          print(itemData)
          break
        }

        if (skip == 1) {
          skipped = rbind(skipped, c(k, blk))
          next
        }
        else {

          if ( negative_block_indicatior[blk] )
          {
            itemData = (max(itemData) + 1) - itemData
          }

          response <- cbind(response,t(itemData))

        }
      }

      responses = rbind(responses,response)
    }

    return(list(
            responses = responses,
            skipped   = skipped
            )
            )

}

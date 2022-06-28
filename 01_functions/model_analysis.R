
## Packages
############################################################## 






## Average results accros replications
############################################################## 
# average across list, via extracting numeric ("averageable") variables, averaging them (mean/SD) and put together
# in original data frame format
averageResults <- function(resList, FUN, replicationFUN) {
  initiate <- FUN(resList[[1]])
  numCols <- onlyNum(initiate)
  outArray <- array(data = NA, dim = c(nrow(numCols), ncol(numCols),length(resList)))
  
  # conduct functions over all repetitions
  for(i in seq_along(resList)) {
    outArray[,,i] <- as.matrix(onlyNum(FUN(resList[[i]])))
  }
  outArray
  # average over array
  avResults <- apply(outArray, 1:2, replicationFUN)
  
  ## put together result frame
  resFrame <- initiate
  firstNum <- which(vapply(initiate, is.numeric, FUN.VALUE = logical(1)))[1]
  # fill in averages
  resFrame[, firstNum:ncol(initiate)] <- avResults
  resFrame
}

# select only numerical variables of data frame/tbl
onlyNum <- function(dat) {
  Filter(is.numeric, dat)
}





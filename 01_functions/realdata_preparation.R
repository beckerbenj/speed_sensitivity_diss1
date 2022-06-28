


### count NAs per item
countNAs <- function(df){
  l <- lapply(df, function(var) sum(is.na(var))/length(var))
  unlist(l)
}


### pick items based on NA counting
pickItems <- function(NAvec, cutOff = 1){
  out <- names(NAvec[NAvec < cutOff])
  dropNum <- which(out %in% c("BOOKID", "VER_DAT"))
  out[-c(1, length(out))]
}


## find number of overlapping items to other test forms
getOverlapp <- function(booklet, itemNames) {
  ol <- lapply(itemNames, function(x) sum(x %in% booklet))
  ol <- unlist(ol)
  sort(ol, decreasing = T) 
}

### recode Partial credit items
# single one with max = 2
recodePC <- function(var) {
  out <- var
  out[!is.na(var)] <- 0
  out[var == max(var, na.rm = TRUE)] <- 1
  out
}

# for whole data set
dichoPC <- function(subDat, grepExclude) {
  out <- subDat
  
  for(i in seq(ncol(subDat))) {
    if(!grepl(grepExclude, names(subDat)[i])) {
      out[, i] <- recodePC(subDat[, i])
    }
  }
  out
}





## functions to prepare data for analysis in jags
jagsDat_wide <- function(dat, RT_conversion_factor, verbose = TRUE) {
  ## pick variables with item responses
  RTs <- grepDF(c("T\\>"), dat)
  items <- grepDF(c("C\\>", "S\\>"), dat)
  if(verbose) {
    cat("Item Names for Responses: \n", names(items))
    cat("Item Names for Response Times: \n", names(RTs))
  }
  
  if(ncol(RTs) != ncol(items)) stop("Different amount of response and response time items.")
  
  ## convert RTs to seconds (educated guess, 30 minutes testing time for each cluster)
  RTs <- RTs / RT_conversion_factor
  
  # sort items for RT and Y equally
  names(items) <- substr(names(items), 3, 8)
  names(RTs) <- substr(names(RTs), 3, 8)
  RTs <- RTs[, eatGADS:::compare_and_order(names(RTs), names(items))$in_both_ordered]
  
  # prepare data for jags
  datList <- list(y = items, RT = RTs, items = ncol(items), N = nrow(items))
  datList
}


# old (do not use)
jagsDat_long <- function(dat) {
  counter <- 1
  y <- item <- person <- NULL
  n_item <- ncol(dat)
  n_person <- nrow(dat)
  n <- n_item * n_person
  
  for(i in seq(n_person))
  {
    for(j in seq(n_item))
    {
      y[counter] <- dat[i,j]
      item[counter] <- j
      person[counter] <- i
      counter <- counter + 1
    }
  }
  
  # prepare data for jags
  data <- list(y = y, item = item, person = person, 
               items = n_item, N = n_person)
  data
}

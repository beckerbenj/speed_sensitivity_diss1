
### subset data frame by regular expressions
grepDF <- function(strings, df) {
  out <- vector("list", length(strings))
  for(i in seq_along(strings)) {
    out[[i]] <- grep(strings[i], names(df)) 
  }
  indices <- unlist(out)
  df[, indices, drop = FALSE]
}

### subset named vector by regular expressions
grepVEC <- function(strings, vec) {
  ind <- grep(strings, names(vec))
  vec[ind]
}

### round data frame (all numeric variables)
roundDF <- function(df, roundN = 3) {
  roundCol <- function(var, roundN) {
    if(!is.numeric(var)) return(var)
    round(var, digits = roundN)
  }
  
  out <- as.data.frame(lapply(df, roundCol, roundN = roundN), stringsAsFactors = FALSE)
  names(out) <- names(df)
  out
}
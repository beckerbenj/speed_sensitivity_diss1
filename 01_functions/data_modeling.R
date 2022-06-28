## Packages
############################################################## 
library(TAM)


## Dealing with missings
############################################################## 
Incorrect <- function(Yreal) {
  ifelse(is.na(Yreal), 0, Yreal)
}

Ignore <- function(Yreal) {
  Yreal
}


## Format input for TAM
############################################################## 
# name data set according to tam rules
nameItems <- function(dat) {
  newNames <- paste("I", seq(ncol(dat)), sep = "")
  colnames(dat) <- newNames
  dat
}

# fix item parameters (alphas -> difficulties, betas -> discrimination parameters)
# TAM has very specific formatting which has to be used as input into function
fixDiff <- function(vec) {
  out <- cbind(seq_along(vec), vec)
  colnames(out) <- c("", "xsi0")
  out
} 
fixDiscr <- function(vec) {
  values <- cbind(seq_along(vec), rep(2, length(vec)), rep(1, length(vec)), vec)
  weiredStuff <- cbind(seq_along(vec), rep(1, length(vec)), rep(1, length(vec)), rep(0, length(vec)))
  out <- rbind(values, weiredStuff)
  colnames(out) <- NULL
  out <- out[order(out[, 1], out[, 2]), ]
  out
} 

## Analysis Functions
############################################################## 
## Analysis with fixed item parameters
fixedIRT <- function(dat, a, b) {
  # fixing item parameters
  fixedDiff <- fixDiff(b)
  fixedDiscr <- fixDiscr(a)
  mod <- tam.mml.2pl(resp = dat, irtmodel = "2PL", xsi.fixed = fixedDiff, B.fixed = fixedDiscr, verbose = FALSE)
  #browser()
  # estimate WLE's (EAP's underlie shrinkage effect), add them to tam.mml object
  mod$wle <- tam.wle(mod)
  mod
}


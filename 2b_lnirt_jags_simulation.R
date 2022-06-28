
library(rjags)
load.module("glm")

source("01_functions/realdata_preparation.R")
source("01_functions/data_simulation.R")


datList <- replicate(1, simulateData(n = 2000, mu_abil = 0, sd_abil = 1, 
                                       speedValues = c(-1, -0.5, 0.5, 1), 
                                       timeLimit = 3600,
                                       n_items = 30, mu_a = 1, var_a = 0.3^2, mu_b = 0, var_b = 1, 
                                       mu_lambda = 4.0, var_lambda = 0.5^2, mu_phi = 1, var_phi = 0.3^2, 
                                       mu_varEpsilon = 0.5, var_varEpsilon = 0.1^2,
                                       model = "Fox", discr_shift = 0.4),
                     simplify = F)

#### true correlations
cor(datList[[1]]$itemPar$lowDis)

# a) LNIRT --------------------------------------------------------
lnirtTest <- LNIRT::LNIRT(Y = datList[[1]]$complResp$lowDis, RT = log(datList[[1]]$respTimes$lowDis$responseTimes), 
                         WL = FALSE, XG = 1000, residual = T)
summary(lnirtTest)
a <- summary(lnirtTest)$SigmaIcor
a[c(2,1,4,3),c(2,1,4,3)] # same order as in my matrices


# b) JAGS --------------------------------------------------------
# prepare data for jags
datList_jags <- jagsDat_wide(resp = datList[[1]]$complResp$lowDis, RT = datList[[1]]$respTimes$lowDis$responseTimes) 

### define inverse wishart prior (identity matrix?)
datList_jags$M <- diag(x=1, nrow = 4)
datList_jags$M_vanderLinden <- diag(x=1, nrow = 3)
str(datList_jags)

### ! initial values are picked automatically
itemPars <- matrix(c(rep(0, datList_jags$items),
                     rep(1, datList_jags$items),
                     rep(1, datList_jags$items), 
                     rep(1, datList_jags$items)), nrow = datList_jags$items, ncol = 4)
PersPar <- matrix(rep(0, datList_jags$N * 2), nrow = datList_jags$N, ncol = 2)
errVar <- rep(0.4, datList_jags$items)

initVal <- list(itemPar = itemPars, PersPar = PersPar, errVar = errVar)

parameters <- c("SigmaP", "ItemPar", "muI", "SigmaI", "correlI", "VarErrVar", "errVar")


### initial model estimation
jags <- jags.model(file = "03_jagsModels/jags_fox.jags",
                   data = datList_jags,
                   inits = initVal,
                   n.chains = 2,
                   n.adapt = 1000)
# throw away burn in
update(jags, 1000)

### sampling from posterior distribution
sCoda_fox <- coda.samples(jags,
                          parameters,
                          n.iter = 1000,
                          thin = 1)

sCoda_fox

# extract results and post means
scoda_tables <- extract_from_scoda(sCoda_fox)



# c) Compare --------------------------------------------------------
data.frame ( a[c(2,1,4,3),c(2,1,4,3)]) - data.frame (scoda_tables$CORtable)[,-1]

## very similar results!







# d) Correlations --------------------------------------------------------
######## Related: Instability of correlations relative to correlation in population
twoRan <- function(m) {
  out <- lapply(c(30, 30), function(x) rnorm(x, m , 1))
  out
}
a <- lapply(rep(0, 1000), twoRan)
cors <- unlist(lapply(a, function(x) cor(x[[1]], x[[2]])))
hist(cors)
cor.test(a[[1]][[1]], a[[1]][[2]])

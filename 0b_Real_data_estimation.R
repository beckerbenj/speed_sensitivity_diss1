
# setwd("N:/RT jags/response-time-simulation-study-1")
library(rjags)
load.module("glm")

#source("01_functions/realdata_preparation.R")
#source("01_functions/tools.R")
source("N:/01_functions/realdata_preparation.R")
source("N:/01_functions/tools.R")


# a) Data preparation --------------------------------------------------------
# dat <- readRDS("c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_analysisDat_math.rds")
dat <- readRDS("N:/PISA_analysisDat_math.rds")
# dat <- readRDS("PISA_analysisDat.rds")
# dat <- readRDS("c:/Users/weirichs/Dropbox/PISA_analysisDat.rds")

# prepare data for jags
datList <- jagsDat_wide(dat = dat, RT_conversion_factor = 1000) 


# b) Model Preperation --------------------------------------------------------
### define inverse wishart prior (identity matrix?)
datList$M <- diag(x=1, nrow = 4)
datList$M_vanderLinden <- diag(x=1, nrow = 3)
str(datList)

### ! initial values are picked automatically
itemPars <- matrix(c(rep(0, datList$items),
                    rep(1, datList$items),
                    rep(1, datList$items), 
                    rep(1, datList$items)), nrow = datList$items, ncol = 4)
PersPar <- matrix(rep(0, datList$N * 2), nrow = datList$N, ncol = 2)
errVar <- rep(0.4, datList$items)

initVal <- list(itemPar = itemPars, PersPar = PersPar, errVar = errVar)

parameters <- c("SigmaP", "ItemPar", "muI", "SigmaI", "correlI", "VarErrVar", "errVar")

# c) Fox --------------------------------------------------------
### initial model estimation
jags <- jags.model(file = "03_jagsModels/jags_fox.jags",
# jags <- jags.model(file = "N:/03_jagsModels/jags_fox.jags",
                   data = datList,
                   inits = initVal,
                   n.chains = 2,
                   n.adapt = 10000)
# throw away burn in
update(jags, 10000)

### sampling from posterior distribution
sCoda_fox <- coda.samples(jags,
                       parameters,
                       n.iter = 10000,
                       thin = 1)

## model fit
dic_fox <- dic.samples(jags, 10000, type="pD")
# results
saveRDS(dic_fox, file = "N:/04_codaSamples/dic_fox.rds")
saveRDS(sCoda_fox, file = "N:/04_codaSamples/sCoda_fox.rds")

# d) van der Linden --------------------------------------------------------
### initial model estimation
jags_vdL <- jags.model(file = "N:/03_jagsModels/jags_vanderLinden.jags",
                   data = datList,
                   inits = initVal,
                   n.chains = 2,
                   n.adapt = 10000)
# throw away burn in
update(jags_vdL, 10000)

### sampling from posterior distribution
sCoda_vdL <- coda.samples(jags_vdL,
                      parameters,
                      n.iter = 10000,
                      thin = 1)

## model fit
dic_vdL <- dic.samples(jags_vdL, 10000, type="pD")
# results
saveRDS(dic_vdL, file = "N:/04_codaSamples/dic_vdL.rds")
saveRDS(sCoda_vdL, file = "N:/04_codaSamples/sCoda_vdL.rds")


# e) fox, identification as in LNIRT --------------------------------------------------------
## for alternative fox model estimation
itemPars[datList$items, 4] <- NA
rho <- 0
invVar <- 2
muI <- c(0, 1, 4, 1)
initVal <- list(itemPar = itemPars, PersPar = PersPar, errVar = errVar,
                rho = rho, invVar = invVar, 
                muI = muI)


### initial model estimation
jags_fox_prod <- jags.model(file = "03_jagsModels/jags_fox_prod.jags",
#jags_fox_prod <- jags.model(file = "N:/03_jagsModels/jags_fox_prod.jags",
                       data = datList,
                       inits = initVal,
                       n.chains = 2,
                       n.adapt = 10000)
# throw away burn in
update(jags_fox_prod, 10000)

### sampling from posterior distribution
sCoda_fox_prod <- coda.samples(jags_fox_prod,
                          parameters,
                          n.iter = 10000,
                          thin = 1)

## model fit
dic_fox_prod <- dic.samples(jags_fox_prod, 10000, type="pD")
# results
saveRDS(dic_fox_prod, file = "N:/04_codaSamples/dic_fox_prod.rds")
saveRDS(sCoda_fox_prod, file = "N:/04_codaSamples/sCoda_fox_prod.rds")






# setwd("N:/RT jags/response-time-simulation-study-1")
library(rjags)
load.module("glm")

#source("01_functions/realdata_preparation.R")
#source("01_functions/tools.R")
source("N:/01_functions/realdata_preparation.R")
source("N:/01_functions/tools.R")


# a) Data preparation --------------------------------------------------------
# dat <- readRDS("c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_analysisDat_math.rds")
dat_list <- readRDS("N:/PISA_analysisDat_math_list.rds")
#dat_list <- readRDS("N:/PISA_analysisDat_read_list.rds")
# dat <- readRDS("PISA_analysisDat.rds")
# dat <- readRDS("c:/Users/weirichs/Dropbox/PISA_analysisDat.rds")

save_path <- "N:/04_codaSamples/3rd run/math/"
#save_path <- "N:/04_codaSamples/reading/"

### check how for second item cluster scored items and time variables relate!
# prepare data for jags
dat_list <- lapply(dat_list, function(l) jagsDat_wide(l, RT_conversion_factor = 1000))

#dat_list <- dat_list[4]

lapply(names(dat_list), function(nam) {
  datList <- dat_list[[nam]]
  
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
  saveRDS(dic_vdL, file = paste0(save_path, "dic_vdL", nam, ".rds"))
  saveRDS(sCoda_vdL, file = paste0(save_path, "sCoda_vdL", nam, ".rds"))
})

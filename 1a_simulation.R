################################################################################################################

#####################     Data simulation and IRT modeling   ######################

################################################################################################################

### packages
######################################

### sources
######################################
source("01_functions/data_simulation.R")
source("01_functions/data_modeling.R")
source("01_functions/graphs.R")


set.seed(54738)

## vary for different conditions
discr_shift <- c(med = 0.1, high = 0.4)
#discr_shift <- 0.4 ## Original

save_path <- "02_data"

## I. Pick parameters for simulation, draw parameters, simulate data
########################################################################################
# Simulates data  with ability parameters that are identical across the different speed parameters;
# person parameters are identical across test forms
# test forms are shifted in their discrimination parameter; van der Linden (2006) und Fox (2007) model are implemented

## set up Item parameter mean vector and covariance matrix
mu_I <- c(mu_b = 0.54, mu_a = 1.12, mu_lambda = 4.26, mu_phi = 0.3)

Sigma_I <- matrix(rep(0, 16), ncol = 4, nrow = 4)
diag(Sigma_I) <- rep(1, 4)
Sigma_I[lower.tri(Sigma_I)] <- c(0.07, 0.25, 0.27, -0.05, 0.12, 0.22) # correlations
Sigma_I[upper.tri(Sigma_I)] <- t(Sigma_I)[upper.tri(t(Sigma_I))] # mirror on upper half
sd_vec <- c(var_b = 1, var_a = 0.67, var_lambda = 0.5, var_phi = 0.1) # vector of standard deviations

Sigma_I <- MBESS::cor2cov(Sigma_I, sd = sd_vec)

## Fox data
# parameters are chosen in accordance to the simulation study by Fox & Marianti (2017)
datList <- replicate(500, simulateData(n = 2000, mu_abil = 0, sd_abil = 1, 
                                      speedValues = c(-1, -0.5, 0.5, 1), 
                                      timeLimit = 3900,
                                      n_items = 30, mu_I = mu_I, Sigma_I = Sigma_I,
                                      mu_varEpsilon = 0.2, var_varEpsilon = 0.1^2,
                                      model = "Fox", discr_shift = discr_shift),
                          simplify = F)


### check responses
check0(datList)

mean(unlist(lapply(datList, function(x) mean(x$persPar$abil[1:500]))))


## Ib. Treat Missings
########################################################################################
# incorrect and ignore
for(r in seq_along(datList)) {
  for(heft in seq_along(datList[[1]]$resp)) {
    datList[[r]]$resp[[heft]]$scoredIncorrect <- Incorrect(datList[[r]]$resp[[heft]]$unscoredData)
    datList[[r]]$resp[[heft]]$scoredIgnore <- Ignore(datList[[r]]$resp[[heft]]$unscoredData)
  }
}

saveRDS(datList, file.path(save_path, "datList.rds"))

## II. Analysis of response data 
########################################################################################
# loop over test forms
modList_Incorrect <- lapply(datList, function(dat) 
  Map(function(Y, i) 
    fixedIRT(dat = Y$scoredIncorrect, a = i$a, b = i$b),
    Y = dat$resp, i = dat$itemPar))

#modList_Ignore <- lapply(datList, function(dat) 
#  Map(function(Y, i) 
#    fixedIRT(dat = Y$scoredIgnore, a = i$a, b = i$b),
#    Y = dat$resp, i = dat$itemPar))


## Save Simulation results (parameters, data, TAM results)
########################################################################################
saveRDS(modList_Incorrect, file.path(save_path, "modList_Incorrect.rds"))
#saveRDS(modList_Ignore, file.path(save_path, "modList_Ignore.rds"))



# ----------------------------------------
#load_path <- "02_data_cond2"
#setwd("N:/09_simulation")
load_path <- "02_data"

### load data
######################################
datList <- readRDS(file.path(load_path, "datList.rds"))
modList_Incorrect <- readRDS(file.path(load_path, "modList_Incorrect.rds"))
#modList_Ignore <- readRDS("02_data/modList_Ignore.rds")
modList <- modList_Incorrect
# ----------------------------------------


## III. Setup result data frame
########################################################################################
#### set up result data set
resultList <- vector("list", length(modList))
testforms <-  names(datList[[1]]$itemPar)
testformRes <- list()
for(arr in seq_along(modList)) {
  # extract results for both testforms
  for(i in seq_along(testforms)) {
    testformRes[[i]] <- data.frame(estimatedAbil = modList[[arr]][[testforms[[i]]]]$wle$theta,
                                   missings = apply(datList[[arr]]$resp[[testforms[[i]]]]$unscoredData, 1, function(x) 
                                     sum(is.na(x))),
                                   cumRT = datList[[arr]]$respTimes[[testforms[[i]]]]$cumResponseTimes[, 30])
  }
  # assemble results in one data frame
  resultList[[arr]] <- createPlotData(common = datList[[arr]]$persPar, lowdis = testformRes[[1]], meddis = testformRes[[2]],
                                      highdis = testformRes[[3]])
}

str(resultList[1:3])

#saveRDS(resultList, file = "c:/Benjamin_Becker/02_Repositories/simulations/response-time-simulation-study-1/02_data/resultList.RDS")
saveRDS(resultList, file = "02_data/resultList.RDS")




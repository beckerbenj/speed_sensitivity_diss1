################################################################################################################

#####################     Data simulation and IRT modeling   ######################

################################################################################################################

### packages
######################################

### sources
######################################
source("01_functions/data_simulation.R")
source("01_functions/data_modeling.R")


set.seed(54738)

## I. Pick parameters for simulation, draw parameters, simulate data
########################################################################################
# Simulates data  with ability parameters that are identical across the different speed parameters;
# person parameters are identical across test forms
# test forms are shifted in their discrimination parameter; van der Linden (2006) und Fox (2007) model are implemented

## Fox data
# parameters are chosen in accordance to the simulation study by Fox & Marianti (2017)
datList <- replicate(5, simulateData(n = 3000, mu_abil = 0, sd_abil = 1, 
                                      mu_speed = 0, sd_speed = 1, timeLimit = 2500,
                                      n_items = 30, mu_a = 1, sd_a = 0.3, mu_b = 0, sd_b = 1, 
                                      mu_lambda = 4.0, sd_lambda = 0.5, mu_phi = 0.2, sd_phi = 0.1, 
                                      mu_varEpsilon = 0.8, sd_varEpsilon = 0.5,
                                      model = "Fox", discr_shift = 0.3),
                          simplify = F)



## Ib. Treat Missings
########################################################################################
# incorrect and ignore
for(r in seq_along(datList)) {
  for(heft in seq_along(datList[[1]]$resp)) {
    datList[[r]]$resp[[heft]]$scoredIncorrect <- Incorrect(datList[[r]]$resp[[heft]]$unscoredData)
    datList[[r]]$resp[[heft]]$scoredIgnore <- Ignore(datList[[r]]$resp[[heft]]$unscoredData)
  }
}

## II. Analysis of response data 
########################################################################################
# loop over test forms
modList_Incorrect <- lapply(datList, function(dat) 
  Map(function(Y, i) 
    fixedIRT(dat = Y$scoredIncorrect, a = i$a, b = i$b),
    Y = dat$resp, i = dat$itemPar))

modList_Ignore <- lapply(datList, function(dat) 
  Map(function(Y, i) 
    fixedIRT(dat = Y$scoredIgnore, a = i$a, b = i$b),
    Y = dat$resp, i = dat$itemPar))


## Save Simulation results (parameters, data, TAM results)
########################################################################################
saveRDS(datList, "02_data/datList.rds")
saveRDS(modList_Incorrect, "02_data/modList_Incorrect.rds")
saveRDS(modList_Ignore, "02_data/modList_Ignore.rds")





################################################################################################################

#####################     Analysing and plotting data simulation and models  ######################

################################################################################################################

### packages
######################################
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

### sources
######################################
source("01_functions/graphs.R")
source("01_functions/tools.R")
source("01_functions/model_analysis.R")
## set fonts
windowsFonts(Times=windowsFont("TT Times New Roman"))


#load_path <- "02_data_cond2"
load_path <- "02_data"

### load data
######################################
datList <- readRDS(file.path(load_path, "datList.rds"))
modList_Incorrect <- readRDS(file.path(load_path, "modList_Incorrect.rds"))
#modList_Ignore <- readRDS("02_data/modList_Ignore.rds")

### chose missing approach results
modList <- modList_Incorrect

## III. Setup result data frame
########################################################################################
#### set up result data set
resultList <- vector("list", length(modList))
testformRes <- testforms <- list("lowDis", "highDis")
for(arr in seq_along(modList)) {
  # extract results for both testforms
  for(i in seq_along(testforms)) {
    testformRes[[i]] <- data.frame(estimatedAbil = modList[[arr]][[testforms[[i]]]]$wle$theta,
                                   missings = apply(datList[[arr]]$resp[[testforms[[i]]]]$unscoredData, 1, function(x) 
                                     sum(is.na(x))),
                                   cumRT = datList[[arr]]$respTimes[[testforms[[i]]]]$cumResponseTimes[, 30])
  }
  # assemble results in one data frame
  resultList[[arr]] <- createPlotData(common = datList[[arr]]$persPar, lowdis = testformRes[[1]], highdis = testformRes[[2]])
}

str(resultList[1:5])

#saveRDS(resultList, file = "c:/Benjamin_Becker/02_Repositories/simulations/response-time-simulation-study-1/02_data_all/resultList_cond2.RDS")
saveRDS(resultList, file = "c:/Benjamin_Becker/02_Repositories/simulations/response-time-simulation-study-1/02_data_all/resultList_cond1.RDS")


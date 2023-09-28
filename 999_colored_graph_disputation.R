################################################################################################################

#####################     Analysing and plotting data simulation and models  ######################

################################################################################################################

### packages
######################################
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(wesanderson) # color palettes

### sources
######################################
source("01_functions/graphs.R")
source("01_functions/tools.R")
source("01_functions/model_analysis.R")
## set fonts
windowsFonts(Times=windowsFont("TT Times New Roman"))


### load data
######################################
load_path <- "02_data"
resultList <- readRDS(file.path(load_path, "resultList.rds"))
datList <- readRDS(file.path(load_path, "datList.rds"))

# set wd for graphs
setwd("c:/Repositories/Dissertation/disputation/Graphs")
#setwd("c:/Benjamin_Becker/00_Promotion/20_Presentations/Graphs")

## IV. Plots and result analysis
########################################################################################
# pick single repetition for plots
repNum <- 1
plotData <- resultList[[repNum]]
singleDat <- datList[[repNum]]

plotList <- list()

levels(plotData$speedCat) <- c("slowest subgroup", "slow subgroup", "fast subgroup", "fastest subgroup")




################## Plots ###############
### plots (estimated and true ability depending on test form and speed)
plotData$Booklet <- factor(plotData$Booklet, levels = c("low ϕ", "medium ϕ", "high ϕ")) 

plotList$abil_subGroup <- ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = Booklet), size = 0.6) +
  geom_smooth(mapping = aes(colour = Booklet), method = lm, se = FALSE, size = 1.1) +
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ speedCat) +
  apatheme + 
  labs(x = "True Ability", y = "Estimated Ability") +
  scale_color_manual(values = wes_palette("GrandBudapest1", n = 3))
  #scale_color_manual(values=c("#999999", "#555555", "#000000"))



## print all saved plots
plotList

### save all graphs to wd
######################################
Map(saveGraph, graph = plotList, fileName = names(plotList))








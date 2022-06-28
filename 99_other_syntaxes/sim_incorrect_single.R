################################################################################################################

#####################     Time sensitivity and differential test functioning    ######################

################################################################################################################
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

source("01_functions/data_simulation.R")
source("01_functions/data_analysis.R")
source("01_functions/graphs.R")
set.seed(54738)

#### to do:
# set up repetitions?
# how to change data analysis then?
# divide syntax into simulation/modeling and graphs/outputs

## I. Pick parameters for simulation, draw parameters, simulate data
########################################################################################
# Simulates data  with ability parameters that are identical across the different speed parameters;
# person parameters are identical across test forms
# test forms are shifted in their discrimination parameter; van der Linden (2006) und Fox (2007) model are implemented

## van der Linden data
#dat <- simulateData(n_speedGroup = 250, mu_abil = 0, sd_abil = 1, speedValues = c(-0.4, -0.2, 0.2, 0.4), timeLimit = 3100,
#                        n_items = 30, mu_a = 0, sd_a = 1, mu_b = 1, sd_b= 0, 
#                       mu_lambda = 4.0, sd_lambda = 0.5, mu_alpha = 1.3, sd_alpha = 0,
#                      model = "vanderLinden", discr_shift = 0.9)

## Fox data
dat <- simulateData(n_speedGroup = 250, mu_abil = 0, sd_abil = 1, speedValues = c(-0.4, -0.2, 0.2, 0.4), timeLimit = 2500,
                    n_items = 30, mu_a = 1, sd_a = 0, mu_b = 0, sd_b= 1, 
                    mu_lambda = 4.0, sd_lambda = 0.5, mu_psi = 0.5, sd_psi = 0, mu_varEpsilon = 0.2, sd_varEpsilon = 0,
                    model = "Fox", discr_shift = 0.3)



## II. Analysis of response data 
########################################################################################
# missing = incorrect
# loop over test forms
modList <- Map(function(Y, i) fixedIRT(dat = Y$scoredData, a = i$a, b = i$b),
               Y = dat$resp, i = dat$itemPar)


## III. Setup result data frame
########################################################################################
#### set up result data set
# extract results for both testforms
testformRes <- testforms <- list("lowDis", "highDis")
for(i in seq_along(testforms)) {
  testformRes[[i]] <- data.frame(estimatedAbil = modList[[testforms[[i]]]]$person$EAP,
                                 missings = apply(dat$resp[[testforms[[i]]]]$unscoredData, 1, function(x) sum(is.na(x))),
                                 cumRT = dat$respTimes[[testforms[[i]]]]$cumResponseTimes[, 30])
}

# assemble results in one data frame
plotData <- createPlotData(common = dat$persPar, lowdis = testformRes[[1]], highdis = testformRes[[2]])
str(plotData)


## IV. Plots and result analysis
########################################################################################
################## Univariate Plots ###############
# Missing pattern of test forms
by(plotData, INDICES = list(plotData$speed, plotData$Heft), function(dat) dat$missings)

# exemplary distribution of response times of a single item
compareDist(dat$respTimes$lowDis$responseTimes[, 1],
            dat$respTimes$highDis$responseTimes[, 1], limX = c(0, 1000), width = 10)

# distribution of test time (cumulated response time)
compareDist(dat$respTimes$lowDis$cumResponseTimes[, 30],
            dat$respTimes$highDis$cumResponseTimes[, 30], limX = c(0, 8000), width = 70)


################## Descriptives ###############
##### per test forms
plotData %>%
  group_by(Heft) %>%
  summarise(meanSumTime = mean(cumRT),
            meanPercMissings = mean(missings)/30,
            corCumRT_Speed = cor(cumRT, speed),
            cor1RT_Speed = NA,
            corMiss_Speed = cor(speed, missings))

#### per Test forms and speed
# set up grouped data frame
grouped_data <- plotData %>%
  group_by(Heft, speed)

grouped_data %>%
  summarise(mean_cumRT = mean(cumRT), sd_cumRT = sd(cumRT),
            SD_Mis = sd(missings),
            meanPercMissings = mean(missings)/30,
            corAbil = cor(abil, estimatedAbil))

### univerate plots per subgroup  
# testing time per subgroup
plotData %>%
  ggplot(aes(cumRT)) +
  geom_histogram(binwidth = 80) +
  geom_vline(xintercept = 3100, color = "red") +
  facet_wrap(speed ~ Heft, nrow = 4)
# missings per subgroup
plotData %>%
  ggplot(aes(missings)) +
  geom_histogram(binwidth = 1) +
  xlim(1, 30) +
  facet_wrap(speed ~ Heft, nrow = 4)

################## Ability Estimation ###############
# Bias in ability estimation between testforms and per speed level
grouped_data %>%
  summarise(Mean = mean(estimatedAbil - abil)) %>%
  dcast(speed ~ Heft, value.var = "Mean") %>%
  mutate(Diff = lowDis - highDis)
# with(plotData, tapply(estimatedAbil - trueAbil, INDEX = list(speed, Heft), mean))

# RMSE
with(plotData, tapply(estimatedAbil - abil, INDEX = list(speed, Heft), function(x) sqrt(mean(x^2))))
plotData %>%
  group_by(Heft, speed) %>%
  summarise(RMSE = sqrt(mean((estimatedAbil - abil)^2))) %>%
  dcast(speed ~ Heft, value.var = "RMSE")


################## Plots ###############
### plots (estimated and true ability depending on test form and speed)
ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = Heft)) +
  geom_smooth(mapping = aes(colour = Heft), method = lm) +
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ speed)

ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = speed)) +
  geom_smooth(mapping = aes(linetype = speed < 0)) + 
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ Heft)

ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point() +
  geom_smooth() + 
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(Heft ~ speed, nrow = 2)



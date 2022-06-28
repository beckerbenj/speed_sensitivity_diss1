################################################################################################################

#####################     Time sensitivity and differential test functioning    ######################

################################################################################################################
library(MASS)
library(dplyr)
library(ggplot2)
library(reshape2)

source("functions_irt.R")
set.seed(54738)

## I. Parameters for simulation
########################################################################################
### Person parameters 
# two dimensions (ability and speed)
# 1000 person per test version (4 different speed values)
group1 <- mvrnorm(250, mu = 0, Sigma = 1)
l <- length(group1)
pers <- data.frame(abil = c(group1, group1, group1, group1), 
                   speed = c(rep(-0.4, l), rep(-0.2, l), rep(0.2, l), rep(0.4, l)))

### Item Parameters
# generate a testform (low discriminating)
# 30 items (2pl for ability, 2pl for speed), but all discriminations equal
# parameters chosen in accordance to van der Linden (2006)
items <- vector("list")
items[[1]] <- data.frame(aDif = mvrnorm(30, 0, 1, empirical = T), 
                    aDis = rep(1, 30),
                    sDif = mvrnorm(30, 4.0, 0.5, empirical = T),
                    sDis = rep(1.3, 30))

## second testform, identical 
items[[2]] <- items[[1]]
# shift disc. for speed for second test form
items[[2]][, 4] <- items[[1]]$sDis + .8
# to ensure similar test length, set time intensitiy lower for higher discriminating testform
# items[[2]]$sDif <- items[[1]]$sDif - 0.5

## II. Simulate response data 
########################################################################################
# according to van der Linden RT model (responses and response times)
### responses
logits <- irtLogits(theta = pers$abil, alpha = items[[1]]$aDis, beta = items[[1]]$aDif)
probits <- logit2prob(logits)

Ytrue <- drawResponseDF(probits)

### response times
rtData <- list()
for(cond in seq(length(items))) {
  rtData[[cond]] <- doRTs(speed = pers$speed, sDif = items[[cond]]$sDif, sDis = items[[cond]]$sDis, formula = "Linden")
}
#rtData[[1]]$responseTimes

# exemplary distribution of response times of a single item
par(mfrow=c(1,2))
hist(rtData[[1]]$responseTimes[, 1], breaks = 30)
hist(rtData[[2]]$responseTimes[, 1], breaks = 30)

# testing time distributions
hist(rtData[[1]]$cumResponseTimes[, 30], breaks = 30)
hist(rtData[[2]]$cumResponseTimes[, 30], breaks = 30)

# insert NAs for not reached items (set time Limit!) and score them as incorrect
Yreal <- lapply(rtData, function(x) generateNAs(Ytrue = Ytrue, cumRT = x$cumResponseTimes, timeLimit = 2400))


## III. Analysis of response data 
########################################################################################

## alternative: ignore missing responses
modList <- list()
for(cond in seq(length(items))) {
  modList[[cond]] <- fixedIRT(dat = Yreal[[cond]]$unscoredData, alphas = items[[cond]]$aDif, betas = items[[cond]]$aDis)
}

# old version: missing = incorrect
# modList <- list()
# for(cond in seq(length(items))) {
#   modList[[cond]] <- fixedIRT(dat = Yreal[[cond]]$scoredData, alphas = items[[cond]]$aDif, betas = items[[cond]]$aDis)
# }


## set up result data set
plotData <- createPlotData(abil = pers$abil, speed = pers$speed, 
                           lowdisEAP = modList[[1]]$person$EAP, highdisEAP = modList[[2]]$person$EAP)
str(plotData)


## IV. Plots and result analysis
########################################################################################
# Missing pattern of test forms
apply(Yreal[[1]]$unscoredData, 1, function(x) sum(is.na(x)))
apply(Yreal[[2]]$unscoredData, 1, function(x) sum(is.na(x)))

### plots (estimated and true ability depending on test form and speed)
ggplot(data = plotData, mapping = aes(x = trueAbil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = Heft)) +
  geom_smooth(mapping = aes(colour = Heft), method = lm) +
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ speed)

ggplot(data = plotData, mapping = aes(x = trueAbil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = speed)) +
  geom_smooth(mapping = aes(linetype = speed < 0)) + 
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ Heft)


### Descriptives
# differences in average ability estimation between testforms and per speed level
plotData %>%
  group_by(Heft, speed) %>%
  summarise(Mean = mean(estimatedAbil)) %>%
  dcast(speed ~ Heft, value.var = "Mean") %>%
  mutate(Diff = lowDis - highDis)

# average time per test version
mean(rtData[[1]]$cumResponseTimes[, 30])
mean(rtData[[2]]$cumResponseTimes[, 30])

# average number of missings
mean(apply(Yreal[[1]]$unscoredData, 1, function(x) sum(is.na(x))))/30
mean(apply(Yreal[[2]]$unscoredData, 1, function(x) sum(is.na(x))))/30

# missing responses 
unscoredData <- rbind(Yreal[[1]]$unscoredData, Yreal[[2]]$unscoredData)
tapply(apply(unscoredData, 1, function(x) sum(is.na(x))), 
       INDEX = list(plotData$speed, plotData$Heft), mean)

# Bias
with(plotData, tapply(estimatedAbil - trueAbil, INDEX = list(speed, Heft), mean))
plotData %>%
  group_by(Heft, speed) %>%
  summarise(Bias = mean(estimatedAbil - trueAbil)) %>%
  dcast(speed ~ Heft, value.var = "Bias") %>%
  mutate(Diff = lowDis - highDis)


# RMSE
with(plotData, tapply(estimatedAbil - trueAbil, INDEX = list(speed, Heft), function(x) sqrt(mean(x^2))))
plotData %>%
  group_by(Heft, speed) %>%
  summarise(RMSE = sqrt(mean((estimatedAbil - trueAbil)^2))) %>%
  dcast(speed ~ Heft, value.var = "RMSE")



#### check results with LNIRT
library(LNIRT)
library(coda)
source("01_functions/realdata_preparation.R")
source("01_functions/tools.R")


# 1. a) Real data with LNIRT --------------------------------------------------------
#dat <- readRDS("c:/Benjamin_Becker/00_Promotion/11_Paper1_Bias/Data/PISA_analysisDat_math.rds")
dat_list <- readRDS("N:/PISA_analysisDat_math_list.rds")
datList <- jagsDat_wide(dat = dat_list[[1]], RT_conversion_factor = 1000) 


datLNIRT <- lapply(datList[1:2], function(x) {
  x <- as.matrix(x)
  dimnames(x) <- NULL
  x
})

# drop missings
# datLNIRT <- lapply(datLNIRT, function(x) x[complete.cases(datLNIRT$y) & complete.cases(datLNIRT$RT), ])

####### analyse data
## 3PLN
lnirtMod <- LNIRT::LNIRT(Y = datLNIRT$y, RT = log(datLNIRT$RT), WL = FALSE, XG = 10000, residual = T)
summary(lnirtMod)
str(lnirtMod)
str(summary(lnirtMod))

## 2PLN
lnirtMod <- LNIRT::LNIRT(Y = datLNIRT$y, RT = log(datLNIRT$RT), WL = TRUE, XG = 10000, residual = T)
summary(lnirtMod)

#### check LNIRT identification?






# ------------------------------------------ Archiv

#cirtMod <- cirt::estimate(Y = datLNIRT$y, Time = log(datLNIRT$RT), N = 1863, K = 12, iter = 1000, PL = 2, TM = 2)
cirtMod <- cirt::estimate(Y = datLNIRT$y, Time = log(datLNIRT$RT), N = 1152, K = 12, iter = 10000, PL = 2, TM = 2)
cirtMod_vdL <- cirt::estimate(Y = datLNIRT$y, Time = log(datLNIRT$RT), N = 1152, K = 12, iter = 10000, PL = 2, TM = 1)
cirt::summarize(cirtMod, 1000)
cirt::summarize(cirtMod_vdL, 1000)




prod(lnirtMod$Post.Means$Time.Discrimination)
# product should be ~1 but is ~1.4


mean(lnirtMod$Post.Means$Time.Discrimination)
sd(lnirtMod$Post.Means$Time.Discrimination)
cor(lnirtMod$Post.Means$Time.Discrimination, lnirtMod$Post.Means$Item.Difficulty)
cor(lnirtMod$Post.Means$Time.Discrimination, lnirtMod$Post.Means$Item.Discrimination)
cor(lnirtMod$Post.Means$Time.Discrimination, lnirtMod$Post.Means$Time.Intensity)

mcmc.object <- as.mcmc(lnirtMod$MCMC.Samples$Item.Difficulty) # Extract MCMC samples for coda
summary(mcmc.object)
plot(mcmc.object)



# 1.b) Real data with JAGS --------------------------------------------------------
sCoda_fox <- readRDS(file = "n:/RT jags/response-time-simulation-study-1/04_codaSamples/sCoda_fox.rds")
scoda_tables <- extract_from_scoda(sCoda_fox)



# 1.c) Compare real Data --------------------------------------------------------
b <- summary(lnirtMod)$SigmaIcor

b[c(2,1,4,3),c(2,1,4,3)] 

# differences
data.frame ( b[c(2,1,4,3),c(2,1,4,3)]) - data.frame ( scoda_tables$CORtable)[,-1]

# item parameters
scoda_tables$ItemPars
with(summary(lnirtMod), data.frame(b = idiff, a = idiscr, lambda = tintens, phi = tdiscr))

cor(scoda_tables$ItemPars)
cor(with(summary(lnirtMod), data.frame(b = idiff, a = idiscr, lambda = tintens, phi = tdiscr))
)

# 2) check simulated data --------------------------------------------------------
datList <- readRDS("02_data/datList.rds")
modList_Incorrect <- readRDS("02_data/modList_Incorrect.rds")

str(datList[[1]])

lnirtSim <- LNIRT::LNIRT(Y = datList[[1]]$complResp$highDis, RT = log(datList[[1]]$respTimes$highDis$responseTimes), 
                         WL = FALSE, XG = 10000, residual = T)


summary(lnirtSim)

#########################################################################################################

###################### compare parameter estimation (hand vs. jags)
# variance
scoda_tables$MVtable
# 
lapply(scoda_tables$ItemPars, sd)

# variance seems to be underestimated via hand (actual parameters show not enough variance; problem with distribution??)

# cov
scoda_tables$sigmaI
cov(scoda_tables$ItemPars)

######## simulate data
library(MASS)
test <- mvrnorm(n = 1000, mu = c(4.3, 1.0), Sigma = matrix(c(0.29, 0.1, 0.1, 0.25), 2, 2))

lala <- cov(test)
cov2cor(lala)
cor(test)

test1 <- replicate(1000, mvrnorm(n = 10, mu = c(4.3, 1.0), Sigma = matrix(c(0.29, 0.1, 0.1, 0.25), 2, 2)), simplify = F)
test2 <- replicate(1000, mvrnorm(n = 1000, mu = c(4.3, 1.0), Sigma = matrix(c(0.29, 0.1, 0.1, 0.25), 2, 2)), simplify = F)

# correlation
mean(unlist(lapply(test1, function(df) cor(df)[1, 2])))
mean(unlist(lapply(test2, function(df) cor(df)[1, 2])))

# variance
mean(unlist(lapply(test1, function(df) var(df[, 1]))))
mean(unlist(lapply(test2, function(df) var(df[, 1]))))

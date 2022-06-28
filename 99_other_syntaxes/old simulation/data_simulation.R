## Packages
############################################################## 
library(MASS)

## Meta-Function (User-Interface)
############################################################## 
simulateData <- function(n_speedGroup, mu_abil, sd_abil, speedValues, timeLimit,
                         n_items, mu_a, sd_a, mu_b, sd_b, 
                         mu_lambda, sd_lambda, mu_phi, sd_phi, mu_varEpsilon, sd_varEpsilon, mu_alpha, sd_alpha,
                         model, discr_shift) {
  ################################## Person parameters 
  # per test version
  # two dimensions (ability and speed)
  # ability parameters are always the same for all speed groups
  group1 <- mvrnorm(n_speedGroup, mu = mu_abil, Sigma = sd_abil)
  l <- length(group1)
  pers <- data.frame(abil = rep(group1, length(speedValues)), 
                     speed = unlist(lapply(speedValues, function(value) rep(value, n_speedGroup))))
  
  ################################## Item Parameters
  # generate a testform (low discriminating) note: alpha is reversed in the Hierarchical RT model!
  # 30 items (2pl for ability, 2pl for speed), but all discriminations equal
  # parameters chosen in accordance to van der Linden (2006)
  items <- vector("list")
  items$lowDis <- data.frame(b = mvrnorm(n_items, mu = mu_b, Sigma = sd_b, empirical = TRUE), 
                             a = mvrnorm(n_items, mu = mu_a, Sigma = sd_a, empirical = TRUE),
                             lambda = mvrnorm(n_items, mu = mu_lambda, Sigma = sd_lambda, empirical = TRUE)) 
  # set discrimination and error variance in dependece of model
  if(identical(model, "vanderLinden") && missing(sd_varEpsilon) && missing(mu_phi) && missing(sd_phi)) {
   # discrimination is 1 for all items in van der Linden
    items$lowDis$phi <- rep(1, n_items)
    # Alpha is parameter for residual variance
   items$lowDis$alpha <- mvrnorm(n_items, mu_alpha, Sigma = sd_alpha, empirical = TRUE) 
   # can be transformed
   items$lowDis$sd_res <- sqrt(items$lowDis$alpha^(-2)) 
  } else if(identical(model, "Fox") && missing(mu_alpha) && missing(sd_alpha)) {
   items$lowDis$phi = mvrnorm(n_items, mu = mu_phi, Sigma = sd_phi, empirical = TRUE)
   items$lowDis$sd_res = mvrnorm(n_items, mu = mu_varEpsilon, Sigma = sd_varEpsilon, empirical = TRUE) 
  } else stop("Wrong parameters specified")    
  
  ## second testform, identical parameters
  items$highDis <- items[[1]]
  if(model == "vanderLinden"){
    items$highDis$alpha <- items[[1]]$alpha + discr_shift
    items$highDis$sd_res <- sqrt(items$highDis$alpha^(-2))
    # correct time intensity for similar average response times
    items$highDis$lambda <- items[[1]]$lambda + discr_shift/4 ### different correction or as additional argument?
  } else if(model == "Fox"){
    items$highDis$phi <- items[[1]]$phi + discr_shift
  }
    
  ################################## Simulate Responses
  # according to van der Linden RT model (responses and response times)
  ### responses (for both test forms)
  logits <- lapply(items, function(i) irtLogits(theta = pers$abil, a = i$a, b = i$b))
  probits <- lapply(logits, logit2prob)
  Ytrue <- lapply(probits, drawResponseDF)
  
  ################################## Simulate Responses Times
  ### response times
  rtData <- lapply(items, function(i) doRTs(zeta = pers$speed, 
                                            lambda = i$lambda, phi = i$phi, sd_E = i$sd_res))
  
  ################################## Inster NAs
  # insert NAs for not reached items (set time Limit!) and score them as incorrect
  Yreal <- Map(function(Y, RT) generateNAs(Ytrue = Y, cumRT = RT$cumResponseTimes, timeLimit = timeLimit), 
               Y = Ytrue, RT = rtData)
  
  #### output
  list(resp = Yreal, respTimes = rtData, complResp = Ytrue, itemPar = items, persPar = pers)
}



## 0. Common Functions
############################################################## 
###### logit and probit conversion
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob2logit <- function(probit){
  logit <- log(probit/(1-probit))
  logit
}


## 1. Response Simulation Functions
############################################################## 
## 2pl model, output logits in matrix
irtLogits <- function(theta, a, b) {
  stopifnot(length(a) == length(b))
  stopifnot(is.double(theta))
  
  logits <- matrix(NA, nrow = length(theta), ncol = length(b))
  for(p in seq_along(theta)) {
    logits[p, ] <- a * (theta[p] - b) ## 2PL Modell
  }
  logits
}

## draw responses for probits; via comparing to random number between 0 and 1
drawResponseDF <- function(probDF) {
  stopifnot(is.matrix(probDF))
  
  responses <- randoms <- probDF
  randoms <- runif(nrow(probDF) * ncol(probDF), min = 0, max = 1)
  responses <- ifelse(probDF > randoms, 1, 0)
  responses
}


## 2. Simulate Response times 
############################################################## 
#### META-Function
doRTs <- function(zeta, lambda, phi, sd_E) {
  rt <- drawRTDF(zeta = zeta, lambda = lambda, phi = phi, sd_E = sd_E)
  
  ## accumulate response times
  cumRT <- rowapply(rt, cumsum)
  list(responseTimes = rt, cumResponseTimes = cumRT)
}


## lognormal response time model
drawRTDF <- function(zeta, lambda, phi, sd_E) {
  stopifnot(length(lambda) == length(phi) && length(lambda) == length(sd_E))
  stopifnot(is.double(zeta))
  # Model-information
  if(all(phi == 1)) message("all phi are 1 => van der Linden (2006) model")
  if(!all(phi == 1)) message("phi vary between items => KleinEntink/Fox model")
  
  E <- matrix(NA, nrow = length(zeta), ncol = length(lambda))
  for(p in seq_along(zeta)) {
    E[p, ] <- rnorm(length(lambda), mean = 0, sd = sd_E) # see below
  }
  
  logs <- matrix(NA, nrow = length(zeta), ncol = length(lambda))
  for(p in seq_along(zeta)) {
    logs[p, ] <- exp(lambda - (phi * zeta[p]) + E[p, ])   ## Fox & Marianti (2017), van der Linden with phi = 1 for all i 
  }
  logs
}

## 
rowapply <- function(dat, FUN) {
  stopifnot(is.data.frame(dat) | is.matrix(dat))
  
  out <- dat
  for(r in seq(nrow(out))) {
    out[r, ] <- FUN(dat[r, ])
  }
  out
}

## 3. Inserting NAs 
############################################################## 
generateNAs <- function(Ytrue, cumRT, timeLimit) {
  #cut-of values
  cutRT <- timeLimit ## seconds
  D <- ifelse(cumRT > cutRT, 1, 0)
  
  # code not reached as NAs
  Yreal <- ifelse(D == 1, NA, Ytrue)
  
  out <- list(unscoredData = Yreal)
  out
}

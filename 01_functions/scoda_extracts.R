
library(coda)



####
extract_from_scoda <- function(scoda) {
  # convert to data frame for convenience (all chains in one)
  codaDF <- as.data.frame(as.matrix(scoda))
  parNames <- c("b", "a", "lambda")
  if("SigmaI[4,3]" %in% names(codaDF)) parNames <- c(parNames, "phi")
  
  # means of posterior distributions, or use median
  postMeans <- colMeans(codaDF)
  # postMeans <- robustbase::colMedians(as.matrix(codaDF))
  
  ### 1.) Item Parameter Correlations
  cors <- grepVEC(strings = "correlI", postMeans)
  cors_Mat <- matrix(cors, length(parNames), length(parNames))
  
  # formatting
  CORtable <- data.frame(parNames, cors_Mat, stringsAsFactors = FALSE)
  names(CORtable) <- c("", parNames)
  CORtable <- roundDF(CORtable, roundN = 2)
  
  ### 2.) Means and Variances of Item Parameters
  means <- grepVEC(strings = "muI", postMeans)
  
  ### 3.) Standard Deviations of Item Parameters
  vars <- grepVEC(strings = "SigmaI", postMeans)
  vars_Mat <- matrix(vars, length(parNames), length(parNames))
  sd_vec <- sqrt(unlist(diag(vars_Mat)))
  
  # formatting (Table with means and variances of item parameters)
  MVtable <- data.frame(Parameter = parNames, M = means, SD = sd_vec, stringsAsFactors = F)
  MVtable <- roundDF(MVtable)
  
  ### 4.) Error variances
  errs <- grepVEC(strings = "errVar", postMeans)
  errs_vec <- c(mean = mean(errs), stand_dev = sd(errs))
  
  ### 5.) Person parameters
  persCov <- matrix(grepVEC(strings = "SigmaP", postMeans), 2, 2)
  persCor <- cov2cor(persCov)
  persCov <- as.data.frame(persCov)
  persCor <- as.data.frame(persCor)
  names(persCov) <- names(persCor) <- c("abil", "speed") 
  
  
  ### 6.) Item parameters
  ItemPars <- matrix(grepVEC(strings = "ItemPar", postMeans), ncol = length(parNames))
  ItemPars <- as.data.frame(ItemPars)
  names(ItemPars) <- parNames
  
  # check: sigmaI conversion to correlI correct?
  sigmaI <- matrix(grepVEC(strings = "SigmaI", postMeans), length(parNames), length(parNames))
  corr_check <- cov2cor(matrix(grepVEC(strings = "SigmaI", postMeans), length(parNames), length(parNames)))
  
  # different correlation calculation, add residual variance
  ItemPars_all <- cbind(ItemPars, errs)
  CORtable_hand <- cor(ItemPars_all) 
  COVtable_hand <- cov(ItemPars_all) 
  
  #browser() 
  
  # output all tables
  list(CORtable = CORtable, MVtable = MVtable, errs_MV = errs_vec, persCov = persCov, persCor = persCor,
       ItemPars = ItemPars, CORtable_hand = CORtable_hand, sigmaI = sigmaI, sigmaI_hand = COVtable_hand)
}



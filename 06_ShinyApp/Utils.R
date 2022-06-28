# package MASS should be installed.

# Simulate Item paramters form distributional parameters
simulateItems <- function(nItems, MuItems, SigmaItems, seed = 225){
   if(nItems == 1){
      out <- data.frame(a = MuItems[1], b = MuItems[2], alpha = MuItems[3],
                  psi = MuItems[4], lambda = MuItems[5])
   } else {
      set.seed(seed)
      out <- data.frame(MASS::mvrnorm(n = nItems, mu = MuItems, Sigma = SigmaItems))
      names(out) <- c("a", "b", "alpha", "psi", "lambda")
   }
   out$a <- exp(out$a)
   out$psi <- exp(out$psi)
   return(out)
}

# simulate Person parameters form distributional parameters
simulatePersons <- function(nPersons, MuPersons, SigmaPersons, seed = 225){
   if(nPersons == 1){
      out <- data.frame(Theta = MuPersons[1], Zeta = MuPersons[2])
   } else {
      set.seed(seed)
      out <- data.frame(MASS::mvrnorm(n = nPersons, mu = MuPersons, Sigma = SigmaPersons))
      names(out) <- c("Theta", "Zeta")
   }
   return(out)
}

# generate repsonses
gen2PL <- function(persons, items, nResponses, intercept = FALSE, seed = 225){
   set.seed(seed)
   I <- dim(items)[1]
   P <- dim(persons)[1]
   R <- nResponses
   if(intercept){
      mu <- exp(outer(persons$Theta, items$a) - outer(rep(1, P), items$b))
   } else {
      mu <- exp(rep(1, P) %o% items$a * outer(persons$Theta, items$b, FUN = "-"))
   }
   out <- array(rbinom(P*I*R, 1, mu/(1+mu)), dim = c(P, I, R), 
                dimnames = list(paste0("p", seq_len(P)),
                                paste0("it", seq_len(I)),
                                paste0("rep", seq_len(R))))
   return(out)
}

# generate RT
genRT <- function(persons, items, nResponses, vdLinden = FALSE, seed = 225){
   set.seed(seed)
   I <- dim(items)[1]
   P <- dim(persons)[1]
   R <- nResponses
   
   if(vdLinden){
     logRT <- outer(- persons$Zeta, items$lambda, FUN = "+")
     
   } else {
     logRT <- outer(- persons$Zeta, items$psi) + rep(1, P) %o% items$lambda
   }
   RT <- exp(rep(logRT, times = R) + rnorm(P*I*R, sd = 1/items$alpha))
   out <- array(RT, dim = c(P, I, R), 
                dimnames = list(paste0("p", seq_len(P)),
                                paste0("it", seq_len(I)),
                                paste0("RT", seq_len(R))))
   return(out)
}

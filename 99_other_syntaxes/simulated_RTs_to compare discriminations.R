


########## Archiv
zeta <- rnorm(10000, 0, 1)
zeta1 <- rep(-0.5, 10000)
zeta2 <- rep(0.5, 10000)
zeta <- c(zeta1, zeta2)

RTs <- data.frame(zeta = zeta)
RTs[, "low_alpha_low_phi"] <- drawRTDF(zeta = zeta, lambda = 4, phi = 0.3, var_E = as.matrix(1/2))
RTs[, "high_alpha_low_phi"] <- drawRTDF(zeta = zeta, lambda = 4, phi = 0.3, var_E = as.matrix(1/3.3))
RTs[, "low_alpha_high_phi"] <- drawRTDF(zeta = zeta, lambda = 4, phi = 0.7, var_E = as.matrix(1/2))
RTs[, "high_alpha_high_phi"] <- drawRTDF(zeta = zeta, lambda = 4, phi = 0.7, var_E = as.matrix(1/3.3))

par(mfrow = c(2, 2))
lapply(RTs[, 2:5], function(x) hist(log(x), breaks = 100, xlim = c(1, 7)))

by(RTs, RTs$zeta, function(x) summary(x))

hist(RTs$low_alpha_high_phi[1:10000, ], breaks = 500, xlim = c(0, 500))
hist(RTs$low_alpha_high_phi[10000:20000, ], breaks = 500, xlim = c(0, 500))




source("C:/Benjamin_Becker/02_Repositories/simulations/response-time-simulation-study-1/01_functions/graphs.R")


createRTdata <- function(phi, alpha, zeta = c(low = -0.5, high = 0.5), lambda = 4, n = 10000) {
  zeta_low <- rep(zeta[["low"]], n)
  zeta_high <- rep(zeta[["high"]], n)
  RT_low <- drawRTDF(zeta = zeta_low, lambda = lambda, phi = phi, var_E = as.matrix(1/alpha))
  RT_high <- drawRTDF(zeta = zeta_high, lambda = lambda, phi = phi, var_E = as.matrix(1/alpha))
  
  data.frame(phi = paste("phi = ", phi, sep = ""),
             alpha = paste("alpha = ", alpha, sep = ""),
             zeta = c(paste("zeta = ", zeta_low, sep = ""), paste("zeta = ", zeta_high, sep = "")),
             #RT = c(RT_low, RT_high))
             RT = log(c(RT_low, RT_high)))
}


df_list <- Map(createRTdata, phi = c(0.3, 0.7, 0.3, 0.7), alpha = c(1.5, 1.5, 2.5, 2.5))
df <- do.call(rbind, df_list)
summary(df)

### complete data set
ggplot(data = df, aes(x = RT)) +
  geom_histogram(binwidth = 0.2) + 
  xlim(c(0, 7)) + 
  facet_grid(zeta ~ alpha)

ggplot(data = df, aes(x = RT)) +
  geom_histogram(binwidth = 0.2) + 
  xlim(c(0, 7)) + 
  facet_grid(zeta ~ phi)



### seperate data sets choose one item
alpha_df <- df[df$phi == "phi = 0.3", ]
ggplot(data = alpha_df, aes(x = RT)) +
  geom_histogram(binwidth = 5) + 
  xlim(c(0, 500)) + 
  facet_grid(zeta ~ alpha)

phi_df <- df[df$alpha == "alpha = 1.7", ]
ggplot(data = phi_df, aes(x = RT)) +
  geom_histogram(binwidth = 5) + 
  xlim(c(0, 500)) + 
  facet_grid(zeta ~ phi)

### all in one plot
ggplot(data = df, aes(x = RT)) +
  geom_histogram(binwidth = 5) + 
  xlim(c(0, 500)) + 
  facet_grid(phi ~ alpha + zeta)



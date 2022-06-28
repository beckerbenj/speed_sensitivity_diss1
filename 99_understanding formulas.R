


lambda <- 4
phi <- 0.3
s2_e <- 0.4
s2_zeta <- 1
n <- 1000000
  
e <- rnorm(n, mean = 0, sd = sqrt(s2_e))
zeta <- rnorm(n, mean = 0, sd = s2_zeta)
RTs <- exp(lambda - phi * zeta + e)

## E(RT)
mean(RTs)
mean(exp(rnorm(n = n, mean = lambda, 
               sd = sqrt(s2_e + phi^2 * s2_zeta))))
e_rt <- exp(lambda + s2_e/2 + ((phi^2 * s2_zeta) / 2))
e_rt

##E(RT^2)
mean(RTs^2)
e_rt2 <- exp(2*lambda + 2*s2_e + 2*(phi^2 * s2_zeta^2))
e_rt2

# ------------------------------
##Var(RT)
var(RTs)
e_rt2 - e_rt^2
exp(2*lambda + 2*s2_e + 2*(phi^2*s2_zeta)) - exp(2*lambda + s2_e + phi^2*s2_zeta)
exp(2*lambda)*exp(s2_e + phi^2)*exp(s2_e + phi^2) - exp(2*lambda)*exp(s2_e + phi^2)
exp(2*lambda + s2_e + phi^2*s2_zeta) * (exp(s2_e + phi^2*s2_zeta) -1 )

## Mail Dries with explanation
exp(5+3)
exp(5) * exp(3)

exp(5) * exp(3 + 3) - exp(5) * exp(3)
exp(5 + 2*3)- exp(5+3)
exp(5+3) * (exp(3)-1)


# ------------------
#2. Item
lambda_l <- 4.2
phi_l <- 0.5
s2_e_l <- 0.5

e_l <- rnorm(n, mean = 0, sd = sqrt(s2_e_l))
RTs_l <- exp(lambda_l - phi_l * zeta + e_l)

## E(RT, RT_l)
mean(RTs * RTs_l)

var_dd_ori <- ((s2_e) + (s2_e_l))
mean(exp(rnorm(n = n, 
               mean = (lambda + lambda_l - (phi + phi_l) * zeta), 
               sd = sqrt(var_dd_ori))))
exp(lambda + lambda_l + (s2_e + s2_e_l)/2 + (((phi + phi_l)^2) * s2_zeta)/2 )





# ------------------
# correlation
cor(RTs, RTs_l)
cov(RTs, RTs_l)/sqrt(var(RTs)* var(RTs_l))

(exp(phi*phi_l*s2_zeta) - 1)/ sqrt((exp(s2_e + phi^2*s2_zeta) - 1) * (exp(s2_e_l + phi_l^2*s2_zeta) - 1))

# nice



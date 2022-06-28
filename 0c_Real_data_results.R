
library(ggplot2)
library(mcmcplots)
library(rjags)
library(coda)

source("01_functions/graphs.R")
source("01_functions/scoda_extracts.R")
source("01_functions/tools.R")
## set fonts
windowsFonts(Times=windowsFont("TT Times New Roman"))

# load coda files
#dic_fox_prod <- readRDS(file = "n:/04_codaSamples/dic_fox_prod.rds")
dic_fox <- readRDS(file = "n:/04_codaSamples/dic_fox.rds")
dic_fox_prod <- readRDS(file = "n:/04_codaSamples/dic_fox_prod.rds")
dic_vdL <- readRDS(file = "n:/04_codaSamples/dic_vdL.rds")

#sCoda_fox_prod <- readRDS(file = "n:/04_codaSamples/sCoda_fox_prod.rds")
sCoda_fox <- readRDS(file = "n:/04_codaSamples/sCoda_fox.rds")
sCoda_fox2 <- readRDS(file = "n:/04_codaSamples/sCoda_fox_prod.rds")
sCoda_vdL <- readRDS(file = "n:/04_codaSamples/sCoda_vdL.rds")
sCoda_vdL2 <- readRDS(file = "C:/Benjamin_Becker/02_Repositories/simulations/response-time-simulation-study-1/04_codaSamples/Archiv/04_codaSamples_reading1/sCoda_vdL.rds")


sCoda <- sCoda_fox

# a) Convergence diagnostikcs --------------------------------------------------------
mcmcplot(sCoda_fox2)
mcmcplot(sCoda_fox)
mcmcplot(sCoda_vdL)


# b) Extract Posterior distribution means -----------------------------------------
scoda_tables_fox <- extract_from_scoda(sCoda_fox)
scoda_tables_fox2 <- extract_from_scoda(sCoda_fox2)
scoda_tables_vdL <- extract_from_scoda(sCoda_vdL)
scoda_tables_vdL2 <- extract_from_scoda(sCoda_vdL2)

scoda_tables <- extract_from_scoda(sCoda)
range(scoda_tables$ItemPars$phi)

# how to calculate correlations?
# why are corrl and sigmaI different from correlating itemparameters and LNIRT output???


# c) Formatting for latex -----------------------------------------
CORtable <- scoda_tables$CORtable
MVtable <- scoda_tables$MVtable

# greek letters
names(CORtable)[2] <- CORtable[1, 1] <- "$b_{k}$"
names(CORtable)[3] <- CORtable[2, 1] <- "$a_{k}$"
names(CORtable)[4] <- CORtable[3, 1] <- "$\\lambda_{k}$"
names(CORtable)[5] <- CORtable[4, 1] <- "$\\phi_{k}$"
# formatting
CORtable2 <- as.data.frame(lapply(CORtable, as.character), stringsAsFactors = FALSE)
CORtable2[1, 2] <- CORtable2[1:2, 3] <- CORtable2[1:3, 4] <- CORtable2[1:4, 5] <- "" 
names(CORtable2) <- names(CORtable)

# greek letters
MVtable[1, "Parameter"] <- "$b_{k}$"
MVtable[2, "Parameter"] <- "$a_{k}$"
MVtable[3, "Parameter"] <- "$\\lambda_{k}$"
MVtable[4, "Parameter"] <- "$\\phi_{k}$"



# d) Export tables as tex -----------------------------------------
# set wd for graphs
setwd("c:/Benjamin_Becker/00_Promotion/20_Presentations/Graphs")

# create tables
t1 <- df2tex_xtable(MVtable, caption = "Means of the Posterior Distribution of Mean and Standard Deviation of the Item Parameters, j = 12.")
texSave(tex = t1, fileName = "means_and_variances_IP")
#
t2 <- df2tex_xtable(CORtable2, caption = "Means of the Posterior Distribution of Correlations Between the Item Parameters, j = 12.")
texSave(tex = t2, fileName = "corr_IP")


# e) plots -----------------------------------------
codaDF <- as.data.frame(as.matrix(sCoda))
# discrimination parameters in scatterplot
discr_df <- data.frame(a = sapply(codaDF[14:26], mean),
                       phi = sapply(codaDF[40:52], mean))

scatter_disc <- ggplot(data = discr_df) +
  geom_point(aes(y = a, x = phi), size = 3) +
  apatheme

saveGraph(scatter_disc, fileName = "scatter_disc")


# f) distri posterior phi ----------------------------- 
sd_phi <- data.frame(SD_phi = sqrt(as.data.frame(as.matrix(sCoda))$`SigmaI[4,4]`))
hdp <- sqrt(HPDinterval(sCoda)[[1]]["SigmaI[4,4]", ])

sd_dist <- ggplot(data = sd_phi) + geom_histogram(mapping = aes(x = SD_phi), bins = 70) +
            scale_x_continuous(limits = c(0, 0.8)) + 
            geom_vline(xintercept = hdp["lower"], color = "black", linetype = "solid") +
            geom_vline(xintercept = hdp["upper"], color = "black", linetype = "solid") +
            apatheme + xlab("SD(\u03D5)") + 
            theme(text = element_text(size = 60)) # overwrite font size for this particular graph


saveGraph(sd_dist, fileName = "phi_dist", width = 18, height = 14)

# g) model comparison, fit
dif <- diffdic(dic_vdL, dic_fox)
print(dic_vdL); print(dic_fox); dif







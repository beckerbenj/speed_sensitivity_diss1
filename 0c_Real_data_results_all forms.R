library(ggplot2)
library(mcmcplots)
library(coda)
library(rjags)

source("01_functions/graphs.R")
source("01_functions/scoda_extracts.R")
source("01_functions/tools.R")
## set fonts
windowsFonts(Times=windowsFont("TT Times New Roman"))

load_path <- "n:/00_Promotion/11_Paper1_Bias/04_codaSamples/3rd run"

# Math
coda_paths_math <- list.files(path = file.path(load_path, "math"), pattern = "^sCoda_fox_form_", full.names = TRUE)
dicfox_paths_math <- list.files(path = file.path(load_path, "math"), pattern = "^dic_fox_form_", full.names = TRUE)
dicvdL_paths_math <- list.files(path = file.path(load_path, "math"), pattern = "^dic_vdLform_", full.names = TRUE)

# Reading
#coda_paths_read <- list.files(path = "n:/04_codaSamples/reading/", pattern = "^sCoda_fox_form_", full.names = TRUE)

## vdl
vdl_paths_math <- list.files(path = file.path(load_path, "math"), pattern = "^sCoda_vdLform_", full.names = TRUE)
#vdl_paths_read <- list.files(path = "n:/04_codaSamples/reading", pattern = "^sCoda_vdLform_", full.names = TRUE)

## all comps in one
#vdl_paths <- c(vdl_paths_math, vdl_paths_read)
#coda_paths <- c(coda_paths_math, coda_paths_read)
vdl_paths <- c(vdl_paths_math)
coda_paths <- c(coda_paths_math)

# load coda files
coda_files <- lapply(coda_paths, readRDS)
vdl_files <- lapply(vdl_paths, readRDS)
dicfox_files <- lapply(dicfox_paths_math, readRDS)
dicvdL_files <- lapply(dicvdL_paths_math, readRDS)

#names(coda_files) <- c(paste0("M0", 1:6), paste0("R0", 1:4))
names(coda_files) <- c(paste0("M0", 1:6))
names(coda_files) <- gsub("6", "6ab", names(coda_files))
names(dicfox_files) <- names(dicvdL_files) <- names(coda_files)

# a) Convergence diagnostikcs --------------------------------------------------------
mcmcplot(coda_files[[4]])
#gelman.diag(coda_files$M02, multivariate = F)

lapply(coda_files, function(x){
  gel <- gelman.diag(x, multivariate = F)[[1]]
  #browser()
  apply(gel, 2, function(y) max(y, na.rm = T))
}) 

gelman.diag(coda_files$M0, multivariate = F)

## vdl
lapply(vdl_files, function(x){
  gel <- gelman.diag(x, multivariate = F)[[1]]
  #browser()
  apply(gel, 2, function(y) max(y, na.rm = T))
}) 

# b) Compare phi -----------------------------------------
coda_out <- lapply(coda_files, extract_from_scoda)
coda_out2 <- lapply(vdl_files, extract_from_scoda)

lapply(coda_out, function(x) x$MVtable)
lapply(coda_out, function(x) x$errs_MV)[[1]]

lapply(coda_out, function(x) x$ItemPars)[[1]]
lapply(coda_out2, function(x) x$ItemPars)[[1]]

lapply(coda_out, function(x) x$CORtable)

lapply(coda_out, function(x) x$persCor)
lapply(coda_out2, function(x) x$persCor)
lapply(coda_out2, function(x) x$sigmaI)[[1]]


##### Person and item numbers
mat_list <- readRDS("N:/PISA_analysisDat_math_list.rds")
#read_list <- readRDS("N:/PISA_analysisDat_read_list.rds")
lapply(mat_list, dim)
#lapply(read_list, dim)


# g) model fit -----------------------------------------
dif <- diffdic(dicvdL_files[[1]], dicfox_files[[1]])
print(dicvdL_files[[1]]); print(dicfox_files[[1]]); dif

## preliminary check
Map(function(dicvdL, dicfox) {
  diffdic(dicvdL, dicfox)
}, dicvdL = dicvdL_files, dicfox = dicfox_files)
#####################

extract_dic <- function (x, digits = 0) {
  deviance <- sum(x$deviance)
  psum <- sum(x[[2]])
  list(mean_deviance = round(as.numeric(deviance), digits), 
       penalty = round(as.numeric(psum), digits), 
       penalized_deviance = round(as.numeric(deviance + psum), digits))
}

# b) table for paper -----------------------------------------
phi_table <- do.call(rbind, lapply(names(coda_out), function(x) {
  min_phi <- min(coda_out[[x]]$ItemPars[, "phi"])
  max_phi <- max(coda_out[[x]]$ItemPars[, "phi"])
  hdp <- format(round(sqrt(HPDinterval(coda_files[[x]])[[1]]["SigmaI[4,4]", ]), 2), digits = 2)
  
  hdp_int <- paste0("[", hdp[["lower"]], ", ", hdp[["upper"]], "]")
  
  data.frame(Booklet = x, coda_out[[x]]$MVtable[4, 2:3], hdp_int = hdp_int, min_phi = min_phi, max_phi = max_phi, 
             cor_b = coda_out[[x]]$CORtable[1, "phi"],
             cor_a = coda_out[[x]]$CORtable[2, "phi"], 
             cor_lambda = coda_out[[x]]$CORtable[3, "phi"])
}))

dic_table <- do.call(rbind, lapply(names(coda_out), function(x) {
  data.frame(Booklet = x,
             DIC_fox = as.character(extract_dic(dicfox_files[[x]])$penalized_deviance), DIC_vdl = as.character(extract_dic(dicvdL_files[[x]])$penalized_deviance), 
             DIC_lower = as.character(extract_dic(dicvdL_files[[x]])$penalized_deviance - extract_dic(dicfox_files[[x]])$penalized_deviance))
}))

#Formatting for latex 
# greek letters
names(phi_table) <- c("Booklet",
                      "$M(\\phi)$", 
                        "$SD(\\phi)$",
                        "95 \\% HDP",
                        "$Min(\\phi)$",
                        "$Max(\\phi)$",
                        "$r_{\\phi, b}$",
                        "$r_{\\phi, a}$",
                        "$r_{\\phi, \\lambda}$")
names(dic_table) <- c("Booklet",
                      "$DIC(3PLN)$",
                      "$DIC(2PLN)$",
                      "$\\Delta_{DIC}$")
                        


# d) Export tables as tex -----------------------------------------
# set wd for graphs
setwd("c:/Benjamin_Becker/00_Promotion/20_Presentations/Graphs")

# create tables
t1 <- df2tex_xtable(phi_table, caption = "Means of the Posterior Distribution of Mean and Standard Deviation of the Speed Sensitivity Parameters for all
                    Math booklets.")
t2 <- df2tex_xtable(dic_table, caption = "DIC for the HRT with the 2PLN and the 3PLN and the corresponding difference for all
                    Math booklets.")
texSave(tex = t1, fileName = "phi_all_bookelts")
texSave(tex = t2, fileName = "dic_all_bookelts")


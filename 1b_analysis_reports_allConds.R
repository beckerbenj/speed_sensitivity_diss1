

### packages
######################################
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)

### sources
######################################
source("01_functions/graphs.R")
source("01_functions/tools.R")
source("01_functions/model_analysis.R")
## set fonts
windowsFonts(Times=windowsFont("TT Times New Roman"))

resutList_list <- list()
resutList_list[["cond1"]] <- readRDS(file = "c:/Benjamin_Becker/02_Repositories/simulations/response-time-simulation-study-1/02_data_all/resultList_cond1.RDS")
resutList_list[["cond2"]] <- readRDS(file = "c:/Benjamin_Becker/02_Repositories/simulations/response-time-simulation-study-1/02_data_all/resultList_cond2.RDS")


#### per Test forms and speed
tableList <- list()

subgrpAnalysis <- function(plotData) {
  plotData %>%
    group_by(Booklet, speedCat)%>%
    summarise('M(cumRT)' = mean(cumRT), 
              'SD(cumRT)' = sd(cumRT),
              'M(mis)' = mean(missings)/30,
              'SD(mis)' = sd(missings/30),
              'cor(abil, est)' = cor(abil, estimatedAbil),
              'RMSE' = sqrt(mean((estimatedAbil - abil)^2)),
              'M(Bias)' = mean(estimatedAbil - abil))
}
rawtable_list <- lapply(resutList_list, function(resultList) {
  means_tab <- averageResults(resultList, FUN = subgrpAnalysis, replicationFUN = mean)[c(1, 5), ]
  t(means_tab)
})

do.call(cbind, rawtable_list)




means_cond2 <- averageResults(resultList_cond1, FUN = subgrpAnalysis, replicationFUN = mean)[c(1, 5), ]



as.matrix(rbind(means_cond1, names(means_cond1)))













## IV. Plots and result analysis
########################################################################################
# pick single repetition for plots
repNum <- 1
plotData <- resultList[[repNum]]
singleDat <- datList[[repNum]]

plotList <- list()
tableList <- list()
################## Descriptives ###############
##### per test forms
testformAnalysis <- function(plotData) {
  plotData %>%
    group_by(Booklet) %>%
    summarise('M(cumRT)' = mean(cumRT),
              'M(mis)' = mean(missings)/30,
              'cor(mis, speed)' = cor(speed, missings),
              'perc(fin)' = sum(missings < 1)/n(),
              'Max(mis)' = max(missings)/30)
}
tableList$mean_testForms <- averageResults(resultList, FUN = testformAnalysis, replicationFUN = mean)
tableList$sd_testForms <- averageResults(resultList, FUN = testformAnalysis, replicationFUN = sd)

#### per Test forms and speed
subgrpAnalysis <- function(plotData) {
  plotData %>%
    group_by(Booklet, speedCat)%>%
    summarise('M(cumRT)' = mean(cumRT), 
              'SD(cumRT)' = sd(cumRT),
              'M(mis)' = mean(missings)/30,
              'SD(mis)' = sd(missings/30),
              'cor(abil, est)' = cor(abil, estimatedAbil),
              'RMSE' = sqrt(mean((estimatedAbil - abil)^2)))
}
tableList$mean_all <- averageResults(resultList, FUN = subgrpAnalysis, replicationFUN = mean)
tableList$sd_all <- averageResults(resultList, FUN = subgrpAnalysis, replicationFUN = sd)


################## Ability Estimation ###############
# Bias in ability estimation between testforms and per speed level
# with(plotData, tapply(estimatedAbil - abil, INDEX = list(speed, Booklet), mean))
biasAnalysis <- function(plotData) {
  plotData %>%
    group_by(Booklet, speedCat)%>%
    summarise(Mean = mean(estimatedAbil - abil)) %>%
    dcast(speedCat ~ Booklet, value.var = "Mean") %>%
    mutate(Diff = .[, 2] - .[, 3])
}
tableList$bias_mean_all <- averageResults(resultList, FUN = biasAnalysis, replicationFUN = mean)
tableList$bias_sd_all <- averageResults(resultList, FUN = biasAnalysis, replicationFUN = sd)


levels(plotData$speedCat) <- c("slowest subgroup", "slow subgroup", "fast subgroup", "fastest subgroup")

################## Univariate Plots ###############
# Missing pattern of test forms
by(plotData, INDICES = list(plotData$speedCat, plotData$Booklet), function(dat) dat$missings)

# exemplary distribution of response times of a single item
compareDist(singleDat$respTimes$lowDis$responseTimes[, 1],
            singleDat$respTimes$highDis$responseTimes[, 1], limX = c(0, 1000), width = 10)

# distribution of test time (cumulated response time)
compareDist(singleDat$respTimes$lowDis$cumResponseTimes[, 30],
            singleDat$respTimes$highDis$cumResponseTimes[, 30], limX = c(0, 8000), width = 70)


### univerate plots per subgroup  
# testing time per subgroup
plotList$cumRT_subGroup <- plotData %>%
  ggplot(aes(cumRT)) +
  geom_histogram(binwidth = 80) +
  geom_vline(xintercept = 2600, color = "red") +
  facet_wrap(speedCat ~ Booklet, nrow = 4) +
  apatheme + 
  labs(x = "cumulative Response Times", y = "Frequency")

# missings per subgroup
plotList$miss_subGroup <- plotData %>%
  ggplot(aes(missings)) +
  geom_histogram(binwidth = 1) +
  xlim(-1, 30) + 
  facet_wrap(speedCat ~ Booklet, nrow = 4) +
  apatheme + 
  labs(x = "Missings", y = "Frequency") +
  scale_y_continuous(breaks = c(0, 250, 500))



################## Plots ###############
### plots (estimated and true ability depending on test form and speed)
plotList$abil_subGroup <- ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = Booklet)) +
  geom_smooth(mapping = aes(colour = Booklet), method = lm) +
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ speedCat) +
  apatheme + 
  labs(x = "True Ability", y = "Estimated Ability") +
  scale_color_manual(values=c("#999999", "#000000"))

plotList$abil_subGroup_plusLine <- ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = Booklet)) +
  geom_smooth(mapping = aes(colour = Booklet), method = lm) +
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ speedCat) 
apatheme + 
  labs(x = "True Ability", y = "Estimated Ability") +
  geom_abline(intercept = 0, slope = 1)

plotList$abil_testForm <- ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point(mapping = aes(colour = speedCat)) +
  geom_smooth(mapping = aes(linetype = speed < 0)) + 
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(~ Booklet) + 
  apatheme + 
  labs(x = "True Ability", y = "Estimated Ability")

plotList$abil_all <- ggplot(data = plotData, mapping = aes(x = abil, y = estimatedAbil)) +
  geom_point() +
  geom_smooth() + 
  xlim(-3, 3) + ylim(-3, 3) + 
  facet_wrap(Booklet ~ speedCat, nrow = 2) + 
  apatheme + 
  labs(x = "True Ability", y = "Estimated Ability")


## print all saved plots
plotList

### captions for tables
tableCaptions <- lapply(tableList, function(x) "")

tableCaptions$mean_testForms <- "Analysis of Response Times and Missings for Booklets, Averaged Across All Replications."
tableCaptions$mean_all <- "Analysis of Mean Response Times M($RT_{tot}$) and the Corresponding Standard Deviation SD($RT_{tot}$), Mean Amount of Missings M(Mis), the Corresponding Standard Deviation SD(Mis), Correlation Between True and Estimated Ability $cor(\\theta_{true} \\theta_{est})$ and Root Mean Square Error (RMSE) for Booklets and Speed Subgroups, Averaged Across All Replications."
tableCaptions$bias_mean_all <- "Analysis of Bias in Person Parameter Estimation and Differences in Bias Between Booklets for Speed Subgroups, Averaged Across All Replications."

# hotfix variable names table
names(tableList$bias_mean_all) <- c("Speed", "low $\\phi$", "high $\\phi$", "Diff")

tableList$mean_all$Booklet <- c(rep("low $\\phi$", 4), rep("high $\\phi$", 4))
names(tableList$mean_all) <- c("Test Form", "$\\zeta_{i}$", "$M(RT_{tot})$", "$SD(RT_{tot})$", "$M(Mis)$", "$SD(Mis)$",
                               "$cor(\\theta_{true} \\theta_{est})$", "RMSE")



### Modfiy tables and graphs for german presentation
tableList$bias_mean_all_german <- tableList$bias_mean_all
tableList$bias_mean_all_german$Speed <- c("sehr langsam", "langsam", "schnell", "sehr schnell")
names(tableList$bias_mean_all_german) <- c("Speed", "$\\phi$ = 0.3", "$\\phi$ = 0.7", "Diff")

tableCaptions$bias_mean_all_german <- "Analyse des Bias in der Personenparamersch{\\\"a}tzung, aufgeteilt nach Testform und nach Speed der Personen, gemittelt {\\\"u}ber alle Replikationen."


### save all graphs to wd
######################################
Map(saveGraph, graph = plotList, fileName = names(plotList))

# round
tableList_r <- lapply(tableList, roundDF, roundN = 3)
# convert to latex code
texList <- Map(df2tex_xtable, df = tableList_r, caption = tableCaptions)
# save as tex file
Map(texSave, tex = texList, fileName = names(texList))


## IV. Other Graphs
########################################################################################
GHRT_low <- function(x) exp(4 - 0.3 * x)
GHRT_high <- function(x) exp(4 - 0.7 * x)
##### plot showing sensitivity
phi_theoret <- ggplot(data.frame(x=c(-1.5, 1.5)), aes(x=x)) + 
  # stat_function(fun=GHRT_low, geom="line", aes(colour = "\u03D5 = 0.4"), size=2.5) +
  stat_function(fun=GHRT_low, geom="line", aes(colour = "Item1"), size=2.5) +
  stat_function(fun=GHRT_high, geom="line", aes(colour = "Item2"), size=2.5) +
  #scale_colour_manual("", values = c("coral", "steelblue")) +
  scale_color_manual("Speed sensitivity \n", values=c(Item1 = "#999999", Item2 = "#000000"),
                     labels = c(expression(paste(phi[{1}], " = 0.3")), expression(paste(phi[{2}], " = 0.7")))) + 
  #ylab("Response Time") + xlab("\u03B6") +
  ylab("Response time") + xlab("\u03B6") +
  apatheme +
  theme(text = element_text(family='Times', size = 60)) + # overwrite font size for this particular graph
  theme(legend.key.size = unit(2.5, "cm"))  

# pdf.options(encoding = "CP1250")
# ggsave("phi_theoret.pdf", phi_theoret, width = 20, height = 10, device = "pdf")

saveGraph(phi_theoret, fileName = "phi_theoret", width = 22, height = 11)



##### plot showing discriminations vs sensitivity
plot_expected_RTs <- function(alpha, phi, func, lims, title = "", xlabel) {
  zeta1 <- 4 - phi * 1
  zeta2 <- 4 - phi * -1
  # add vline
  mean1 <- zeta1
  mean2 <- zeta2
  if(func == "dlnorm") {
    mean1 <- exp(zeta1)
    mean2 <- exp(zeta2)
  }
  
  func <- get(func)
  
  df <- data.frame(x = lims, title = title)
  out <- ggplot(data = df, aes(x)) +
    stat_function(fun = func, n = 101, args = list(mean = zeta1, sd = sqrt(1/alpha))) +
    stat_function(fun = func, n = 101, args = list(mean = zeta2, sd = sqrt(1/alpha)), colour = "darkgrey") + 
    # stat_function(fun = func, n = 101, args = list(mean = zeta2, sd = sqrt(1/alpha)), linetype = 2) + 
    ylab("") + xlab(xlabel) + 
    scale_y_continuous(breaks = NULL) +
    #geom_vline(xintercept = mean1) +
    #geom_vline(xintercept = mean2, colour = "darkgrey") +
    apatheme 
  if(!identical(title, "")) out <- out + facet_grid(. ~ title)
  out
}


lim_RT <- replicate(3, c(0, 8), simplify = F)
lim_logRT <- replicate(3, c(0, 320), simplify = F)
titles <- list("\u03B1 = 1; \u03D5 = 0.3", 
               "\u03B1 = 3; \u03D5 = 0.3",
               "\u03B1 = 1; \u03D5 = 0.7")
p <- p_log <- list()
p <- Map(plot_expected_RTs, alpha = c(1, 3, 1), phi = c(0.3, 0.3, 0.7), 
         func = "dnorm", lims = lim_RT, title = titles, xlabel = "log(RT)")
# func = "dnorm", lims = lim_RT, title = "\u03B1 = 1; \u03D5 = 0.3", xlabel = "log(RT)")
p_log <- Map(plot_expected_RTs, alpha = c(1, 3, 1), phi = c(0.3, 0.3, 0.7), 
             func = "dlnorm", lims = lim_logRT, xlabel = "RT")


### how to save gridExtra object?
grid.arrange(p[[1]], p[[2]], p[[3]], p_log[[1]], p_log[[2]], p_log[[3]], nrow = 2)

discri_comp <- arrangeGrob(p[[1]], p[[2]], p[[3]], p_log[[1]], p_log[[2]], p_log[[3]], nrow = 2)
# ggsave("discrimination_comp.eps", discri_comp, width = 16, height = 8, device = "eps")

cairo_ps("discrimination_comp.eps", width = 16, height = 8, fallback_resolution = 1200)
grid:::grid.draw(discri_comp)
dev.off()



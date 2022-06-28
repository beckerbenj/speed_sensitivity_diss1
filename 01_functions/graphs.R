
## Packages
############################################################## 
library(kableExtra)
library(xtable)
library(ggplot2)

## Prepare data
############################################################## 
createPlotData <- function(common, lowdis, meddis, highdis) {
  lowdis <- cbind(common, lowdis, Booklet = rep("low \u03D5", nrow(common)))
  meddis <- cbind(common, meddis, Booklet = rep("medium \u03D5", nrow(common)))
  highdis <- cbind(common, highdis, Booklet = rep("high \u03D5", nrow(common)))
  rbind(lowdis, meddis, highdis)
}

## Graphs
############################################################## 
compareDist <- function(lowDisRT, highDisRT, limX, width) {
  df <- data.frame(lowDisRT = lowDisRT, highDisRT = highDisRT)
  #browser()
  formatting <- geom_histogram(binwidth = width) 
  limits <- xlim(limX)
  plot1 <- ggplot(df, aes(lowDisRT)) + 
    formatting + limits
  plot2 <- ggplot(df, aes(highDisRT)) + 
    formatting + limits
  grid.arrange(plot1, plot2, ncol = 2)
}



#### apa-formatting
apatheme <- theme_bw()  + 
            theme(panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),
                  panel.border=element_blank(),
                  axis.line=element_line(),
                  text = element_text(family='Times', size = 42))


## save graph as png to working directory
saveGraph <- function(graph, fileName, width = 15, height = 14) {
  #setEPS()
  #postscript(paste(fileName, ".tiff", sep = ""), width = width, height = height)
  cairo_ps(paste(fileName, ".eps", sep = ""), width = width, height = height, fallback_resolution = 1200)
  ## ----  functioning PDF solution: (but ugly)
  # width <- 13; hight <- 12
  # cairo_pdf(paste(fileName, ".pdf", sep = ""), width = width, height = height)
  ## ---- normal solutions not suitable for submission
  # width <- 1100; hight <- 1000
  #png(paste(fileName, ".png", sep = ""), width = width, height = height)
  #tiff(paste(fileName, ".tiff", sep = ""), width = width, height = height)
  print(graph)
  dev.off()
}

##### data frame/tibble to latex code for table
# kable version
df2tex_kable <- function(df, caption = "") {
  kableDF <- kable(df, caption = caption, format = "latex", booktabs = T)
  if(ncol(df) > 5 || nrow(df) > 5) {
    return(kable_styling(kableDF, latex_options = c("HOLD_position", "scale_down")))
  } else {
    return(kable_styling(kableDF, latex_options = "HOLD_position"))
  }
}

# with xtable
df2tex_xtable <- function(df, caption = "", san = TRUE) {
  rescFactor <- ifelse(ncol(df) > 5, 1 - (ncol(df) - 5) * 0.1, 1)
  xTab <- xtable(df, caption = caption)
  if(identical(san, TRUE)) san <- function(str) gsub("\\", "\\", str, fixed = TRUE)
  print(xTab, caption.placement = "top", include.rownames = FALSE,
         sanitize.text.function = san)
        # scalebox = as.character(rescFactor), sanitize.text.function = san)
}
#df2tex_xtable(tableList_r$mean_testForms, caption = tableCaptions$mean_testForms)

## save as tex file for \input
texSave <- function(tex, fileName) {
  fileConn <- file(paste("table_", fileName, ".tex", sep = ""))
  writeLines(tex, fileConn)
  close(fileConn)
}



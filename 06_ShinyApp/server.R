
library(shiny)
source("Utils.R")

shinyServer(function(input, output) {
  output$uiItems <- renderUI({
     if(input$nItems == 1) return()
     
     tagList(h4("Standard deviations for item parameters"),
             sliderInput("sdA",
                         "Item Discrimination:",
                         min = 0,
                         max = 3,
                         value = 1.2, 
                         step = .05),
             
             sliderInput("sdB",
                         "Item Location:",
                         min = 0,
                         max = 3,
                         value = 1, 
                         step = .05),
             
             sliderInput("sdAlpha",
                         "Item Time Discrimination (van der Linden):",
                         min = 0,
                         max = 5,
                         value = 2, 
                         step = .05),
             
             sliderInput("sdPsi",
                         "Item Time Discrimination (Klein-Entink):",
                         min = 0,
                         max = 3,
                         value = 1.2, 
                         step = .05),
             
             sliderInput("sdLambda",
                         "Item Time Intesity:",
                         min = 0,
                         max = 5,
                         value = .5, 
                         step = .05),
             
             
             h4("Correlations between item parameters"),
             sliderInput("rhoLogAB",
                          "ln(Dicrimination) vs. Location:",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoLogAAlpha",
                          "ln(dicrimination) vs. Time Discrimination (van der Linden):",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoLogALogPsi",
                          "ln(dicrimination) vs. ln(Time Discrimination (Klein-Entink)):",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoLogALambda",
                          "ln(dicrimination) vs. Time Intensity:",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoBAlpha",
                          "Location vs. Time Discrimination (van der Linden):",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoBLogPsi",
                          "Location vs. ln(Time Discrimination (Klein-Entink)):",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoBLambda",
                          "Location vs. Time Intensity:",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoAlphaLogPsi",
                          "Time Discrimination (van der Linden) vs. ln(Time Discrimination (Klein-Entink)):",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoAlphaLambda",
                          "Time Discrimination (van der Linden) vs. Time Intensity:",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05),
             
             sliderInput("rhoLogPsiLambda",
                          "ln(Time Discrimination (Klein-Entink)) vs. Time Intensity:",
                          min = -1,
                          max = 1,
                          value = 0, 
                          step = .05)
        
     )
  })
  
  output$uiPersons <- renderUI({
     if(input$nPersons == 1) return()
     
     tagList(h4("Standard deviations for person parameters"),
     sliderInput("sdTheta",
                 "Ability:",
                 min = 0,
                 max = 3,
                 value = 1, 
                 step = .05),
     
     sliderInput("sdZeta",
                 "Speed:",
                 min = 0,
                 max = 3,
                 value = 0.5, 
                 step = .05),
     
     h4("Correlations between person parameters"),
     sliderInput("rhoThetaZeta",
                  "Ability and Speed:",
                  min = -1,
                  max = 1,
                  value = 0, 
                  step = .05)
     )
  })
  
  ## generate items
  items <- reactive({
     MuItems <- c(log(input$muA), input$muB, input$muAlpha, log(input$muPsi), input$muLambda)
    
     if(input$nItems == 1){
        SigmaItems = NULL
     } else {
        # correlation matrix
        sdItems <- c(log(input$sdA), input$sdB, input$sdAlpha, log(input$sdPsi), input$sdLambda)
        RhoItems <- matrix(0, ncol = 5, nrow = 5)
        RhoItems[lower.tri(RhoItems)] <- c(input$rhoLogAB, input$rhoLogAAlpha, input$rhoLogALogPsi, input$rhoLogALambda,
                                           input$rhoBAlpha, input$rhoBLogPsi, input$rhoBLambda, 
                                           input$rhoAlphaLogPsi, input$rhoAlphaLambda,
                                           input$rhoLogPsiLambda)
        RhoItems <- RhoItems + t(RhoItems) + diag(5)
        
        # covariance matrix
        SigmaItems <- sdItems %o% rep(1, 5) * RhoItems * rep(1, 5) %o% sdItems
     } 
     
     simulateItems(input$nItems, MuItems, SigmaItems, input$seed)   
  })
  
  ## generate persons
  persons <- reactive({
     MuPersons <- c(input$muTheta, input$muZeta)

     if(input$nPersons == 1){
        SigmaPersons = NULL
     } else {
        # correlation matrix
        sdPersons <- c(input$sdTheta, input$sdZeta)
        RhoPersons <- matrix(0, ncol = 2, nrow = 2)
        RhoPersons[lower.tri(RhoPersons)] <- input$rhoThetaZeta
        RhoPersons <- RhoPersons + t(RhoPersons) + diag(2)
        
        # covariance matrix
        SigmaPersons <- sdPersons %o% rep(1, 2) * RhoPersons * rep(1, 2) %o% sdPersons
     }
     
     simulatePersons(input$nPersons, MuPersons, SigmaPersons, input$seed)   
  })
  
  ## generate responses and RTs
  data <- reactive({
     items <- items()
     persons <- persons()
     
     X <- gen2PL(persons, items, input$nResponses, input$intercept, input$seed)
     RT <- genRT(persons, items, input$nResponses, input$vdLinden, input$seed)
     
     # responses after cut-off are always incorrect
     timeOut <- which(RT > input$RTcut)
     # RT[timeOut] <- input$RTcut
     X[timeOut] <- 0
     
     list(X = X, RT = RT)
  })
  
  output$plot1 <- renderPlot({
     X <- data()$X
     RT <- data()$RT
     xlimMax <- max(c(max(RT), input$RTcut))
     
     RT_all <- hist(RT, plot = FALSE)
     RT_cor <- hist(RT[as.logical(X)], plot = FALSE, breaks = RT_all$breaks)
     
     mfactor <- mean(RT_all$counts / RT_all$density, na.rm = TRUE)
     
     plot(RT_all, col="lightgray", 
          xlab = "Response Times",
          main = "Response Times", 
          # xlim = c(0, input$RTcut)) 
          xlim = c(0, xlimMax)) 
     
     abline(v = input$RTcut, col="red")
     
     plot(RT_cor, col = "skyblue", add = TRUE)
     
     legend("topright", legend=c("Incorrect", "Correct"), 
            fill=c("lightgray","skyblue"))
  })
  
})

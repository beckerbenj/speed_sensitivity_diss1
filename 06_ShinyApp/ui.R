
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Accuracy and Reaction Time according to the hierachical model"),

  fluidRow(
     
     column(2, wellPanel(
        h4("Select many respones should be generated?"),
        numericInput("nResponses",
                     "Number of responses (per person per item):",
                     min = 1,
                     max = 1000,
                     value = 1),
        
        numericInput("nItems",
                     "Number of items:",
                     min = 1,
                     max = 60,
                     value = 1),
        
        numericInput("nPersons",
                     "Number of persons:",
                     min = 1,
                     max = 1000,
                     value = 1), 
        
        h4("Additional options"),
        numericInput("RTcut",
                     "Cut-off for reaction times:",
                     min = 0,
                     max = 600,
                     value = 60),
        
        numericInput("seed",
                     "random seed",
                     min = 0,
                     max = 99999999,
                     value = 79572),
        
        checkboxInput("intercept", "Intercept-Slope version of 2PL",
                      value = FALSE),
        
        checkboxInput("vdLinden", "van der Linden Model?",
                      value = FALSE)
     )
   ),
   
   column(2, wellPanel(
      h4("Choose the (mean) item parameters"),
      sliderInput("muA",
                  "Item Discrimination:",
                  min = 0,
                  max = 2,
                  value = 1, 
                  step = .05),
      
      sliderInput("muB",
                  "Item Location:",
                  min = -2,
                  max = 2,
                  value = 0, 
                  step = .05),
      
      sliderInput("muAlpha",
                  "Item Time Discrimination (van der Linden):",
                  min = 0,
                  max = 100,
                  value = 20, 
                  step = .05),
      
      sliderInput("muPsi",
                  "Item Time Discrimination (Klein-Entink):",
                  min = 0,
                  max = 2,
                  value = .5, 
                  step = .05),
      
      sliderInput("muLambda",
                  "Item Time Intesity:",
                  min = -2,
                  max = 20,
                  value = 2, 
                  step = .05),
      
      
      h4("Choose the (mean) person parameters"),
      sliderInput("muTheta",
                  "Ability:",
                  min = -2,
                  max = 2,
                  value = 0, 
                  step = .05),
      
      sliderInput("muZeta",
                  "Speed:",
                  min = -3,
                  max = 3,
                  value = 0, 
                  step = .05)
     )
   ),
   
   column(2, wellPanel(
        uiOutput("uiPersons"),
        uiOutput("uiItems")
     )
   ),
   
   column(6, wellPanel(
       plotOutput("plot1")
     )
   )

 )
))

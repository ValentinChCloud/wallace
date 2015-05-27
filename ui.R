library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  sidebarLayout(
    sidebarPanel(
      ## Input to get GBIF data based on genus species names
      textInput("gbifName", label = "Enter scientific name of species", value = ''),
      actionButton("goName", "Submit name"),
      
      ## Input upload a CSV file
      fileInput("csvInput", label = "Upload Occurrence CSV"),
      
      ## Conditionals are set such that you have to either run a gbif
      ## search or upload a file to get the rest of these options
      
      ## Map points button
      conditionalPanel("input.goName || output.csvUploaded",
                       actionButton("goMap", "Map points")),
      
      ## Predictor variable resolution choice, or input your own rasters
      conditionalPanel("input.goName || output.csvUploaded",
                       selectInput("pred", label = "Choose predictors",
                                   choices = list(" " = " ", "30 arcsec" = "wc30arcsec", "2.5 arcmin" = "2.5arcmin", 
                                                  "5 arcmin" = "wc5arcmin", "10 arcmin" = "wc10arcmin", 
                                                  "Input rasters" = "inp"))),
      conditionalPanel("input.pred == 'inp'",
                       fileInput(inputId = "predInput", label = "Input predictor rasters", multiple = TRUE)),
      
      ## Set thinning parameter and spThin button
      conditionalPanel("input.goName || output.csvUploaded",
                       numericInput("thinDist", label = "Thinning distance (km)", value = 25),
                       actionButton("goThin", "Run spThin")),
      
      ## Maxent parameters selection buttons and choices (features, regularization, etc)
      conditionalPanel("input.goName || output.csvUploaded",
                       checkboxGroupInput("fcs", label = "Select feature classes", 
                                          choices = list("L" = "L", "LQ" = "LQ", "H" = "H",
                                                         "LQH" = "LQH", "LQHP" = "LQHP", "LQHPT" = "LQHPT")),
                       sliderInput("rms", label = "Select regularization multipliers",
                                   min = 0, max = 10, value = c(1, 5)),
                       numericInput("rmsBy", label = "RM step value", value = 1),
                       selectInput("method", label = "Evaluation method",
                                   choices = list("jackknife" = "jackknife", "block" = "block",
                                                  "checkerboard1" = "checkerboard1", "checkerboard2" = "checkerboard2",
                                                  "randomkfold" = "randomkfold", "user" = "user"), selected = "block"),
                       actionButton("goEval", "Run ENMeval"))
      ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Main",
                 br(),
                 conditionalPanel("input.goName", textOutput('GBIFtxt')),
                 conditionalPanel("output.csvUploaded", uiOutput('CSVtxt')),
                 conditionalPanel("input.goName || output.csvUploaded", textOutput('dataTxt')),
                 #uiOutput("test"),
                 #tableOutput("test2"),
                 textOutput("predTxt"),
                 conditionalPanel("input.goMap", textOutput('mapText')),
                 conditionalPanel("input.goThin", textOutput('thinText')),
                 conditionalPanel("input.goEval", textOutput('evalTxt')),
                 br(),
                 plotOutput('GBIFmap')),
        tabPanel("Occurrence Table", tableOutput('occTbl')),
        tabPanel("ENMeval Table", tableOutput('evalTbl'))
        #tabPanel("Plots", plotOutput())
      )
    )
  )
))

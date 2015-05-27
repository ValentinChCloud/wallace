library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ENM Interactive Evaluation with GBIF"),
  helpText("Calibrate and evaluate ENMs with GBIF data."),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel("!output.fileUploaded",
                       textInput("gbifName", label = "Enter scientific name of species", value = ''),
                       actionButton("goName", "Submit name")),
      conditionalPanel("!output.gbifSearched",
                       fileInput(inputId = "csvInput", label = "Upload Occurrence CSV")),
      conditionalPanel("output.gbifSearched || output.fileUploaded",
                       actionButton("goMap", "Map points")),
      conditionalPanel("output.gbifSearched || output.fileUploaded",
                       selectInput("pred", label = "Choose predictors",
                                   choices = list(" " = " ", "30 arcsec" = "wc30arcsec", "2.5 arcmin" = "2.5arcmin", 
                                                  "5 arcmin" = "wc5arcmin", "10 arcmin" = "wc10arcmin", 
                                                  "Input rasters" = "inp"))),
      conditionalPanel("input.pred == 'inp'",
                       fileInput(inputId = "predInput", label = "Input predictor rasters", multiple = TRUE)),
      conditionalPanel("output.gbifSearched || output.fileUploaded",
                       numericInput("thinDist", label = "Thinning distance (km)", value = 25),
                       actionButton("goThin", "Run spThin")),
      conditionalPanel("output.gbifSearched || output.fileUploaded",
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
                 conditionalPanel("output.gbifSearched", textOutput('GBIFtxt')),
                 conditionalPanel("output.fileUploaded", textOutput('CSVtxt')),
                 conditionalPanel("output.gbifSearched || output.fileUploaded", textOutput('dataTxt')),
                 textOutput("predTxt"),
                 tableOutput("test1"),
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

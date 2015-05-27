list.of.packages <- c("shiny", "shinyIncubator", "ggplot2", "maps", "rgbif", "spThin")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
#library(shinyIncubator)
library(dismo)
library(rgbif)
library(spThin)
library(ENMeval)
library(dismo)
library(ggplot2)

#source("functions.R")

options(shiny.maxRequestSize=3000*1024^2)

shinyServer(function(input, output, session) {
  
  values <- reactiveValues()
  
  GBIFsearch <- reactive({
    if (input$goName == 0) return(NULL) 
    input$goName
    isolate({
      withProgress(message = "Searching GBIF...", {
        results <- occ_search(scientificName = input$gbifName, limit = 50, 
                              fields = 'minimal', hasCoordinate = TRUE)
        locs <- results$data[!is.na(results$data[,3]),][,c(1,4,3)]
        names(locs)[2:3] <- c('lon', 'lat')
        values$df <- locs
        c(nrow(locs), results$meta$count)
      })
    })
  })
  
  getCSV <- reactive({
    if (is.null(input$csvInput)) return(NULL)
    x <- read.csv(input$csvInput$datapath, header = TRUE)
    names(x)[2:3] <- c('lon', 'lat')
    values$df <- x
  })
  
  output$dataTxt <- renderText({values$dataTxt})

  output$fileUploaded <- reactive({
    return(!is.null(getCSV()))
  })
  
  output$gbifSearched <- reactive({
    return(!is.null(GBIFsearch()))
  })
  
  output$occTbl <- renderTable({values$df})
  
  output$GBIFtxt <- renderText({
    if (input$goName == 0) return()
    input$goName
    out <- GBIFsearch()
    name <- isolate(input$gbifName)
    paste('Total records for', name, 'returned [', out[1], '] out of [', out[2], '] total (limit 500).')
  })
  
  output$CSVtxt <- renderText({
    if (is.null(input$csvInput)) return()
    getCSV()
    name <- values$df[1,1]
    paste('Total records for', name, '[', nrow(values$df), '].')
  })
  
  output$GBIFmap <- renderPlot({
    mapWorld <- borders('world', colour = 'white', fill = 'white')
    mp <- ggplot() + mapWorld + 
      theme(panel.background = element_rect(fill = 'lightblue'))
    if (input$goMap == 0) return(print(mp))
    input$goMap
    isolate({
      xl <- c(min(values$df$lon) - 5, max(values$df$lon) + 5)
      yl <- c(min(values$df$lat) - 5, max(values$df$lat) + 5)
      mp <- mp + geom_point(data = values$df, mapping=aes(x = lon, y = lat), color = 'blue', size = 3) +
        coord_cartesian(xlim = xl, ylim = yl)
      print(mp)
    })
  })
  
  output$mapText <- renderText({
    if (input$goMap == 0) return()
    input$goMap
    isolate({
        ptsNum <- nrow(values$df)
      })
      paste('Currently displaying [', ptsNum, '] points.')
  })
  
  runThin <- reactive({
    input$goThin
    isolate({
      withProgress(message = "Thinning...", {
        output <- thin(values$df, 'lat', 'lon', 'name', thin.par = input$thinDist, 
                       reps = 10, locs.thinned.list.return = TRUE, write.files = FALSE)
        names(output[[1]]) <- c('lon', 'lat')
        values$df <- cbind(rep(values$df[1,1], nrow(output[[1]])), output[[1]])
      })
    })
  })
  
  output$thinText <- renderText({
    if (input$goThin == 0) return()
    input$goThin
    runThin()
    paste('Total records thinned to [', nrow(values$df), '] points.')
  })

  loadWC <- reactive({
    withProgress(message = "Loading Worldclim predictor rasters...", {
      values$preds <- stack(paste0("worldclim/", input$pred, '/', 
                                   list.files(paste0("worldclim/", input$pred), pattern = "bil$")))
      output$predTxt <- renderText(paste("Loaded", substr(input$pred, 3, nchar(input$pred)), 
                                         "Worldclim bio1-19 predictor rasters."))
    })
  })
  
  loadInputPreds <- reactive({
    if (is.null(input$predInput)) return()
    output$test1 <- renderTable(input$predInput)
    #values$preds <- stack(input$predInput[1,'datapath'])
    #output$predTxt <- renderText(paste("Loaded", nlayers(values$preds), "input predictor rasters.")) 
  })

  
  output$predTxt <- renderText({
    if (input$pred == " ") return()
    input$pred
    if (input$pred == 'inp') {
      loadInputPreds()
    } else {
      loadWC()
    }
    return(values$predTxt)
  })
  
  runENMeval <- reactive({
    input$goEval

      
    rms <- seq(input$rms[1], input$rms[2], input$rmsBy)
    progress <- shiny::Progress$new()
    progress$set(message = "Evaluating ENMs...", value = 0)
    on.exit(progress$close())
    n <- length(rms) * length(input$fcs)
    updateProgress <- function(value = NULL, detail = NULL) {
      progress$inc(amount = 1/n, detail = detail)
    }
    e <- ENMevaluate(values$df, preds, RMvalues = rms, fc = input$fcs, 
                     method = input$method, updateProgress = updateProgress)
    values$evalTbl <- e@results
  })
  
  output$evalTbl <- renderTable({values$evalTbl})
  
  output$evalTxt <- renderText({
    if (input$goEval == 0) return()
    input$goEval
    runENMeval()
    paste("Ran ENMeval and output table with", nrow(values$evalTbl), "rows.")
  })
  
#   output$thinConsole <- renderPrint({
#     if (input$goThin == 0) return()
#     input$goThin
#     isolate({values[["log"]] <- capture.output(runThin())})
#     return(print(values[["log"]]))
#   })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'gbifSearched', suspendWhenHidden=FALSE)
})

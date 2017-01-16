#' documentation
#'
#'
#'


userOccs_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("userCSV"), label = "Upload Occurrence CSV")
  )
}

userOccs <- function(input, output, session, map) {

  if (is.null(input$userCSV)) return()  # exit if user CSV not specifed

  formatUserOccs <- reactive({
    csv <- read.csv(input$userCSV$datapath)
    ## NEED TO DO PROPER FUNCTION STOP HERE TO EXIT FUNCTION
    if (!all(c('name', 'longitude', 'latitude') %in% names(csv))) {
      isolate({writeLog('<font color="red"><b>! ERROR</b></font> : Please input CSV file with columns "name", "longitude", "latitude".')})
      return()
    }

    # subset to only occs, not backg, and just fields that match df
    ls$spName <- as.character(csv$name[1])  # get species name
    uoccs <- csv[csv[,1] == ls$spName,]  # limit to records with this name
    # subset to just records with latitude and longitude
    uoccs <- uoccs %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
    if (nrow(uoccs) == 0) {
      writeLog(paste('<font color="orange"><b>! WARNING</b></font> : No records with coordinates found in', input$userCSV$name, "for", ls$spName, "."))
      return()
    }

    isolate({writeLog(paste("> User-specified CSV file", input$userCSV$name, "with total of", nrow(uoccs),
                            "records with coordinates was uploaded."))})

    # for (col in c("institutionCode", "country", "stateProvince",
    #               "locality", "elevation", "basisOfRecord")) {  # add all cols to match origOccs if not already there
    #   if (!(col %in% names(uoccs))) uoccs[,col] <- NA
    # }

    uoccs$origID <- row.names(uoccs)  # add col for IDs
    uoccs$pop <- unlist(apply(uoccs, 1, popUpContent))  # add col for map marker popup text

    # origOccs is the unmodified occs, to preserve in comp2 when points are modified
    # ls$df <- ls$origOccs <- uoccs

    return(uoccs)
  })

  # MAPPING
  map %>% zoom2Occs(formatUserOccs()) %>% map_plotLocs(formatUserOccs())

  return(formatUserOccs)
}

# module userOccs
comp1_userOccs <- function() {
  observe({
    dbOccs <- callModule(userOccs, 'c1_userOccs', map)
    if (is.function('dbOccs')) {
      ls$dbOccs <- dbOccs()
    }
  })
}

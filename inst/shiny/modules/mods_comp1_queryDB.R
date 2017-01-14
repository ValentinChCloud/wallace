
queryDB_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("occDb"), "Choose Database:",
                 choices = list("GBIF" = 'gbif',
                                "VertNet" = 'vertnet',
                                "BISON" = 'bison')),
    textInput(ns("spName"), label = "Enter scientific name (format: Genus species)", value = 'Cryptotis mexicana'),

    sliderInput(ns("occNum"), "Maximum number of occurrences:", min = 1, max = 500, value = 50)
  )
}

queryDB <- function(input, output, session, map) {
  if (input$spName == "") return()

  getDBoccs <- reactive({
    # figure out how many separate names (components of scientific name) were entered
    nameSplit <- length(unlist(strsplit(input$spName, " ")))
    # if two names not entered, throw error and return
    if (nameSplit != 2) {
      writeLog('<font color="red"><b>! ERROR</b></font> : Please input both genus and species names.')
      return()
    }

    writeLog(paste("... Searching", input$occDb, "..."))
    # query database
    q <- spocc::occ(input$spName, input$occDb, limit=input$occNum)

    # if species not found, print message to log box and return
    if (q[[input$occDb]]$meta$found == 0) {
      writeLog(paste('! No records found for ', input$spName, ". Please check the spelling."))
      values$df <- NULL  # reset df
      shinyjs::disable("dlDbOccs")
      return()
    }

    # get total number of records found in database
    nrows <- q[[input$occDb]]$meta$found

    # extract occurrence tibble
    d <- q[[input$occDb]]$data[[formatSpName(input$spName)]]
    # make sure latitude and longitude are numeric (sometimes they aren't)
    d$latitude <- as.numeric(d$latitude)
    d$longitude <- as.numeric(d$longitude)

    # get number of original rows
    nrecs <- nrow(d)

    writeLog('> Total', input$occDb, 'records for', input$spName, 'returned [', nrecs,
    '] out of [', nrows, '] total (limit ', input$occNum, ').')

    return(d)
  })


  # # record spName and dbMod in values
  # values$spName <- input$spName
  # values$dbMod <- input$occDb
  # # create tag to signal db search
  # values$mod_db <- TRUE

  # store dbOccs.orig in values list
  # values$df.orig <- dbOccs.orig

  d <- getDBoccs()

  # subset to just records with latitude and longitude
  dc <- d %>% dplyr::filter(!is.na(latitude) & !is.na(longitude))
  if (nrow(dc) == 0) {
    writeLog('<font color="orange"><b>! WARNING</b></font> : No records with coordinates found.')
    return()
  }

  formatDBoccs <- reactive({
    # standardize column names
    if (input$occDb == 'vertnet') {
      dc <- dc %>%
        dplyr::rename(institutionCode = institutioncode) %>%
        dplyr::rename(stateProvince = stateprovince) %>%
        dplyr::rename(basisOfRecord = basisofrecord) %>%
        dplyr::rename(elevation = maximumelevationinmeters)
    }
    # standardize column names
    if (input$occDb == 'bison') {
      dc <- dc %>%
        dplyr::rename(country = countryCode) %>%
        dplyr::rename(institutionCode = ownerInstitutionCollectionCode) %>%
        dplyr::rename(locality = calculatedCounty) %>%
        dplyr::mutate(elevation = NULL)
    }

    # remove duplicate records
    dcNoDups <- dc %>% remDups()
    # make sure the class is numeric for coordinates
    dcNoDups$longitude <- as.numeric(dcNoDups$longitude)
    dcNoDups$latitude <- as.numeric(dcNoDups$latitude)

    # subset by key columns and make id and popup columns
    cols <- c("name", "longitude", "latitude","year", "institutionCode", "country", "stateProvince",
              "locality", "elevation", "basisOfRecord")
    dcNoDups <- dcNoDups %>%
      dplyr::select(dplyr::one_of(cols)) %>%
      dplyr::mutate(origID = row.names(dcNoDups))  # make new column for ID
    dcNoDups <- dcNoDups %>% dplyr::mutate(pop = unlist(apply(dcNoDups, 1, popUpContent)))  # make new column for leaflet marker popup content

    # # origOccs is the unmodified occs, to preserve in comp2 when points are modified
    # values$df <- values$origOccs <- dbOccs

    noCoordsRemoved <- nrow(d) - nrow(dc)
    dupsRemoved <- nrow(dc) - nrow(dcNoDups)
    writeLog(paste('Records without coordinates removed [', noCoordsRemoved, '].
                   Duplicated records removed [', dupsRemoved, ']. Remaining records [', nrow(dcNoDups), '].'))
  })


  # functionality for concatenating multiple db calls
  # add current dbOccs to the list
  #   dbOccsList[[db]] <- dbOccs
  # }
  # # rbind all the data frames together into one
  # dbOccsConcat <- do.call("rbind", dbOccsList)
  # if (length(input$occDb) > 1) {
  #   concat.orig <- nrow(dbOccsConcat)
  #   # remove records with duplicate coordinates
  #   dbOccsConcat <- dbOccsConcat[!duplicated(dbOccsConcat[,c('longitude', 'latitude')]),]
  #   concat.dupsRem <- nrow(dbOccsConcat)
  #   dupsRemNum <- concat.orig - concat.dupsRem
  #   writeLog(paste("Concatenated", paste(input$occDb, collapse=' and '), "."))
  #   if (dupsRemNum > 0) {
  #     writeLog(paste("Duplicated records removed [", dupsRemNum, "]: Remaining records [", concat.dupsRem, "]."))
  #   }
  # MAPPING
  proxy %>% zoom2Occs(formatDBoccs()) %>% map_plotLocs(formatDBoccs())

  return(formatDBoccs)
}

galaxyOccs_UI <- function(id) {
  ns <- NS(id)
  python.load("/import_list_history.py")
  x <- python.call("x")
  v<-list()
  # This one is a tricky one, if history contain many dataset, x is gonna be a list of list
  # But in the case where there is only one dataset, x will be just a list.
  # So I test the first element of the list, if it's a another list it will be a lenght more than 1
  # else it's an element of the list, and length will be 1

  l<-length(x[[1]])
  if(l == 1) {
     if(x$'extension' == 'csv'){
            name<-paste(x$'hid',x$'name')
            id<-unname(x$'hid')
            v[[name]]<-id
        }

  }else{
  l<-length(x)
  for (y in 1:l) {
        if(x[[y]]$'extension' == 'csv'){
            name<-paste(x[[y]]$'hid',x[[y]]$'name')
            id<-unname(x[[y]]$'hid')
            v[[name]]<-id
        }
  }
  }
  tagList(
    tags$div(title='Galaxy portal.',
             selectInput(ns("userCSV"), label = "Select from your Galaxy History User csv file",
                choices = v))
         )

  
  }
galaxyOccs_MOD <- function(input, output, session, rvs) {

  readOccsCSV <- reactive({
  print(input$userCSV)
  command=paste('python /import_csv_user.py',input$userCSV)
   system(command)
    # make occDB record NULL to keep track of where occurrences are coming from
    rvs$occDB <- NULL
    # record for RMD
    path=paste('/import/',input$userCSV,sep="")
    csv <- read.csv(path)

    spName <- trimws(as.character(csv$name[1]))

    if (!all(c('name', 'longitude', 'latitude') %in% names(csv))) {
      rvs %>% writeLog(type = "error", 'Please input CSV file with columns
                        "name", "longitude", "latitude".')
      return()
    }


    # subset to just records with first species name, and non-NA latitude and longitude
    uoccs <- csv %>%
      dplyr::filter(name == spName) %>%
      dplyr::filter(!is.na(latitude) & !is.na(longitude))

    if (nrow(uoccs) == 0) {
      rvs %>% writeLog(type = 'warning', 'No records with coordinates found in',
                        input$userCSV,"mor", spName, ".")
      return()
    }

    rvs %>% writeLog("User-specified CSV file", input$userCSV, "with total of",
                      nrow(uoccs), "records with coordinates was uploaded.")

    for (col in c("year", "institutionCode", "country", "stateProvince",
                  "locality", "elevation", "basisOfRecord")) {  # add all cols to match origOccs if not already there
      if (!(col %in% names(uoccs))) uoccs[,col] <- NA
    }

    uoccs$occID <- row.names(uoccs)  # add col for IDs
    uoccs$pop <- unlist(apply(uoccs, 1, popUpContent))  # add col for map marker popup text

    return(uoccs)
  })

  return(readOccsCSV)
}	

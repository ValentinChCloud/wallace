galaxyEnvs_UI <- function(id) {
  ns <- NS(id)
  python.load("/import_list_history.py")
  x <- python.call("x")
  v<-list()
  # This one is a tricky one, if history contain many dataset, x is gonna be a list of list
  # But in the case where there is only one dataset, x will be just a list.
  # So I test the first element of the list, if it's a another list it will be a lenght more than 1
  # else it's an element of the list, and length will be 1 
  l<-length(x[[1]])
  if(l==1) {
            name<-paste(x$'hid',x$'name')
            id<-unname(x$'hid')
            v[[name]]<-id

  }else {
  l<-length(x)
  for (y in 1:l) {
        name<-paste(x[[y]]$'hid',x[[y]]$'name')
        id<-unname(x[[y]]$'hid')
        v[[name]]<-id
  }
}
  tagList(
    tags$div(title='Galaxy portal',
             selectInput(ns("userEnvs"), label = "Select from your Galaxy History User raster file",
                choices = v))
         )

}

galaxyEnvs_MOD <- function(input, output, session, rvs) {
  reactive({

    if (is.null(rvs$occs)) {
      rvs %>% writeLog(type = 'error', "Before obtaining environmental variables, 
                       obtain occurrence data in component 1.")
      return()
    }
    if (is.null(input$userEnvs)) {
      rvs %>% writeLog(type = 'error', "Raster files not uploaded.")
      return()
    }  

    # Import Galaxy
    command=paste('python /import_csv_user.py',input$userEnvs)
    system(command)
    path=paste('/import/',input$userEnvs,sep="")
    raster<-read.table(path)
    # record for RMD
    rvs$userEnvs <- raster
    
    withProgress(message = "Reading in rasters...", {
      uenvs <- raster::stack(raster$datapath)
      names(uenvs) <- fileNameNoExt(raster$name)
    })
    
    rvs %>% writeLog("Environmental predictors: User input.")
    
    if (is.na(raster::crs(uenvs))) {
      rvs %>% writeLog(type = "warning", "Input rasters have undefined coordinate 
                       reference system (CRS). Mapping functionality in components 
                       Visualize Model Results and Project Model will not work. If 
                       you wish to map rasters in these components, please define 
                       their projections and upload again. See guidance text in 
                       this module for more details.")
    }
    
    return(uenvs)
  })
}

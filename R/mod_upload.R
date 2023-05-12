
uploadUI <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(
               fileInput(ns("upload"), "Upload Reference geodata file"),
               hr(),
               br()
      )
  )
}

uploadServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      userFile <- reactive({
        # If no file is selected, don't do anything
        validate(need(input$upload, message = FALSE))
        input$upload
      })

      sf_data <- reactiveVal(NULL)

      observe({
        req(!is.null(userFile()))
        sf_tbl = sf::st_read(userFile()$datapath)
        sf_data(sf_tbl)
      })
      # Return the reactive that yields the data frame
      return(sf_data)
    }
  )
}
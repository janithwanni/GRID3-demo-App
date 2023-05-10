
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
      
      sf_data <- eventReactive(input$submit_btn, {
        sf_tbl = sf::st_read(userFile()$datapath)
        # sf_tbl = sf::st_read("data/national-health-care-facilities/health-care-facilities-primary-secondary-and-tertiary.geojson")
      })
      # Return the reactive that yields the data frame
      return(sf_data)
    }
  )
}